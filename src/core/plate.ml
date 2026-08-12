(** Implementation of plate business logic. *)

open Types

let validate_format format_str =
  match plate_format_of_string format_str with
  | Ok fmt -> Ok fmt
  | Error _ ->
      Error (Printf.sprintf "Unsupported plate format: '%s'" format_str)

let validate_name name =
  if String.trim name = "" then Error "Plate name cannot be empty" else Ok name

let get_plate_dimensions = function
  | Well_24 -> (4, 6)
  | Well_48 -> (6, 8)
  | Well_96 -> (8, 12)
  | Well_384 -> (16, 24)

let coordinate_of_index format index =
  let rows, cols = get_plate_dimensions format in
  let max_wells = rows * cols in

  if index <= 0 then Error "Index must be a positive integer."
  else if index > max_wells then
    Error
      (Printf.sprintf "Index %d is out of bounds for this plate format (max %d)"
         index max_wells)
  else
    let zero_based_index = index - 1 in
    let row_index = zero_based_index mod rows in
    let col_index = zero_based_index / rows in
    let row_char = char_of_int (int_of_char 'A' + row_index) in
    let col_string = string_of_int (col_index + 1) in
    Ok (String.make 1 row_char ^ col_string)

(* Pull row and column from an alphanumeric well representation "A1" -> ("A", 1) *)
let parse_coordinate coord_str =
  try
    Scanf.sscanf coord_str "%c%d%!" (fun row_char col_num ->
        let r_char = Char.uppercase_ascii row_char in
        let row_index = int_of_char r_char - int_of_char 'A' in
        if row_index < 0 || row_index > 25 then
          (* A-Z are the only supported row indices; two-letter rows (AA, BB, ...) are
           not valid. *)
          Error (Printf.sprintf "Invalid row character: '%c'" row_char)
        else if col_num < 1 then
          (* Microwell plate columns start on 1 and not 0, no negatives. *)
          Error (Printf.sprintf "Invalid column number: %d" col_num)
        else
          let col_index = col_num - 1 in
          Ok (row_index, col_index))
  with _ -> Error "Invalid coordinate format"

let index_of_coordinate plate_format coord_str =
  match parse_coordinate coord_str with
  | Ok (row_index, col_index) ->
      let rows, cols = get_plate_dimensions plate_format in
      if row_index >= rows || col_index >= cols then
        Error
          (Printf.sprintf "Coordinate %s is out of bounds for this plate."
             coord_str)
      else Ok ((col_index * rows) + row_index + 1)
  | Error msg -> Error msg

(* Create a coordinate map for handling wells and plates *)
let generate_well_coordinates (format : plate_format) =
  let rows, cols = get_plate_dimensions format in

  let row_chars =
    List.init rows (fun i -> String.make 1 (char_of_int (int_of_char 'A' + i)))
  in
  let col_nums = List.init cols (fun i -> string_of_int (i + 1)) in

  List.concat_map (fun r -> List.map (fun c -> r ^ c) col_nums) row_chars

(* Ensure a plate on contains samples of the same category (no mixing source and experimental)*)
let validate_sample_mixture ~existing_categories ~new_category =
  let has_conflict =
    List.exists (fun cat -> cat <> new_category) existing_categories
  in

  if has_conflict then
    Error
      "Compliance Violation: Source and Experimental samples cannot be mixed \
       on the same plate."
  else Ok ()

(* Normalize a coordinate input for a plate *)
let normalize_coordinate plate_format input =
  (* Is it numeric? ("1", "96", etc.) *)
  match int_of_string_opt input with
  | Some index -> coordinate_of_index plate_format index
  | None -> (
      (* Is it a coordinate? ("A1", "B3", etc.)*)
      match parse_coordinate input with
      | Ok (row, col) ->
          let rows, cols = get_plate_dimensions plate_format in
          if row < rows && col < cols then Ok input
          else Error (Printf.sprintf "Well %s is out of bounds" input)
      | Error _ -> Error (Printf.sprintf "Invalid well format: '%s'" input))

(** Compares two coordinates in "Column-Major" order. Order: A1 -> B1 -> C1 ...
    -> A2 -> B2 -> C2. Useful for sorting wells to match vertical workflow or
    specific equipment reading order. *)
let compare_coordinates c1 c2 =
  match (parse_coordinate c1, parse_coordinate c2) with
  | Ok (r1, c1_idx), Ok (r2, c2_idx) ->
      if c1_idx <> c2_idx then
        (* compare columns *)
        compare c1_idx c2_idx
      else
        (* compare rows *)
        compare r1 r2
  | Ok _, Error _ -> -1
  | Error _, Ok _ -> 1
  | Error _, Error _ -> String.compare c1 c2

let is_edge_well plate_format coordinate =
  match normalize_coordinate plate_format coordinate with
  | Ok norm_coord -> (
      match parse_coordinate norm_coord with
      | Ok (row, col) ->
          let rows, cols = get_plate_dimensions plate_format in
          row = 0 || row = rows - 1 || col = 0 || col = cols - 1
      | Error _ -> false)
  | Error _ -> false

let get_category_prefix category =
  match category with Some "Source" -> "S" | _ -> "P"

let generate_short_id ~category ~project_id ~count =
  let prefix = get_category_prefix category in
  Printf.sprintf "%s-%d-%04d" prefix project_id (count + 1)
