(* Csv parsing utils *)

type row_accessor = string -> string
(** The type of a function that looks up a column value by its header name. *)

let is_blank_row row =
  List.for_all
    (fun cell -> Option.is_none (Exlab_core.String_utils.non_empty cell))
    row

let read_rows body =
  try Ok (Csv.of_string body |> Csv.input_all) with
  | Csv.Failure (line_n, _, msg) ->
      Error (Printf.sprintf "CSV Syntax Error on line %d: %s" line_n msg)
  | exn -> Error ("CSV Parsing Error: " ^ Printexc.to_string exn)

(** Safely associates headers with a row. If the row is shorter than the header,
    pads with empty strings. If the row is longer, truncates the extras. *)
let safe_associate header row =
  let rec aux h r =
    match (h, r) with
    | [], _ -> [] (* generic stop: ignore extra columns in row *)
    | h_item :: h_tail, r_item :: r_tail ->
        (h_item, r_item) :: aux h_tail r_tail
    | h_item :: h_tail, [] ->
        (h_item, "") :: aux h_tail [] (* pad missing data *)
  in
  aux header row

(** Reads a raw CSV string and converts it to a list of Association Lists. *)
let read_csv_with_headers body =
  (* Csv.of_string creates a generic channel, input_all reads it fully *)
  let csv_data = Csv.of_string body |> Csv.input_all in

  match csv_data with
  | [] -> [] (* Empty file *)
  | header :: rows ->
      (* Clean headers: trim whitespace to ensure " name" matches "name" *)
      let clean_header = List.map String.trim header in
      List.map (fun row -> safe_associate clean_header row) rows

(** Parses a CSV body using a provided decoder function.
    @param body: The raw CSV string.
    @param decoder:
      A function that takes a 'get' function and returns your custom Type. *)
let parse (body : string) (decoder : row_accessor -> ('a, string) result) :
    ('a list, string) result =
  match read_rows body with
  | Error msg -> Error msg
  | Ok [] -> Ok []
  | Ok (header :: rows) ->
      let clean_header = List.map String.trim header in
      let rec process_rows acc index = function
        | [] -> Ok (List.rev acc)
        | row :: rest when is_blank_row row -> process_rows acc (index + 1) rest
        | row :: rest -> (
            let assoc = safe_associate clean_header row in
            let get col = try List.assoc col assoc with Not_found -> "" in
            match decoder get with
            | Ok item -> process_rows (item :: acc) (index + 1) rest
            | Error msg -> Error (Printf.sprintf "Row %d: %s" index msg))
      in
      process_rows [] 2 rows

let parse_matrix body =
  match read_rows body with
  | Error msg -> Error (`Bad_Request msg)
  | Ok [] -> Ok []
  | Ok (header :: rows) -> (
      let cols =
        match header with [] -> [] | _ :: cs -> List.map String.trim cs
      in
      try
        let rev_chunks =
          List.fold_left
            (fun acc row ->
              if is_blank_row row then acc
              else
                match row with
                | [] -> acc
                | row_char :: sample_ids ->
                    let clean_row = String.trim row_char in
                    let combined = List.combine cols sample_ids in
                    let row_items =
                      List.fold_left
                        (fun items (col, sample_id) ->
                          match Exlab_core.String_utils.non_empty sample_id with
                          | Some trimmed_sample_id ->
                              {
                                Api_types.Plate.well = clean_row ^ col;
                                sample_short_id = trimmed_sample_id;
                              }
                              :: items
                          | None -> items)
                        [] combined
                    in
                    row_items :: acc)
            [] rows
        in
        Ok (List.flatten (List.rev rev_chunks))
      with Invalid_argument _ ->
        Error (`Bad_Request "CSV matrix is malformed"))
