(* Csv parsing utils *)

type row_accessor = string -> string
(** The type of a function that looks up a column value by its header name. *)

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
  let rows = read_csv_with_headers body in

  let rec process_rows acc index = function
    | [] -> Ok (List.rev acc)
    | row :: rest -> (
        let get col = try List.assoc col row with Not_found -> "" in
        match decoder get with
        | Ok item -> process_rows (item :: acc) (index + 1) rest
        | Error msg -> Error (Printf.sprintf "Row %d: %s" index msg))
  in

  process_rows [] 2 rows

(** Returns the string trimmed of whitespace. *)
let to_string str = String.trim str

(** Returns Some string if not empty, else None. *)
let to_string_option str = match String.trim str with "" -> None | s -> Some s

(** Returns Some int, or None if empty/invalid. *)
let to_int_option str =
  match String.trim str with "" -> None | s -> int_of_string_opt s

(** Returns int, defaulting to [default] (0) if empty/invalid. *)
let to_int ?(default = 0) str = Option.value ~default (to_int_option str)

(** Returns Some float, or None if empty/invalid. *)
let to_float_option str =
  match String.trim str with "" -> None | s -> float_of_string_opt s

(** Returns float, defaulting to [default] (0.0) if empty/invalid. *)
let to_float ?(default = 0.0) str = Option.value ~default (to_float_option str)

(** Handles "true", "1", "yes" as true. Case insensitive. *)
let to_bool_option str =
  match String.lowercase_ascii (String.trim str) with
  | "true" | "1" | "yes" | "t" -> Some true
  | "false" | "0" | "no" | "f" -> Some false
  | _ -> None

let to_bool ?(default = false) str = Option.value ~default (to_bool_option str)

(** Splits a string by [sep] (default ';') into a string list. *)
let to_list ?(sep = ';') str =
  if String.trim str = "" then []
  else
    String.split_on_char sep str
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")

(** Splits a string by [sep] and converts to ints. *)
let to_int_list ?(sep = ';') str =
  to_list ~sep str |> List.filter_map int_of_string_opt

(** Splits a string by [sep] and converts to floats. *)
let to_float_list ?(sep = ';') str =
  to_list ~sep str |> List.filter_map float_of_string_opt

let parse_matrix body =
  let csv_data = Csv.of_string body |> Csv.input_all in
  match csv_data with
  | [] -> Ok []
  | header :: rows -> (
      let cols = List.tl header in
      (* "1", "2", ... *)
      try
        let layout_items =
          List.fold_left
            (fun acc row ->
              match row with
              | [] -> acc
              | row_char :: sample_ids ->
                  let combined = List.combine cols sample_ids in
                  let new_items =
                    List.fold_left
                      (fun items (col, sample_id) ->
                        let trimmed_sample_id = String.trim sample_id in
                        if trimmed_sample_id <> "" then
                          let well = row_char ^ col in
                          {
                            Api_types.Plate.well;
                            sample_short_id = trimmed_sample_id;
                          }
                          :: items
                        else items)
                      [] combined
                  in
                  acc @ new_items)
            [] rows
        in
        Ok layout_items
      with Invalid_argument _ ->
        Error (`Bad_Request "CSV matrix is malformed"))
