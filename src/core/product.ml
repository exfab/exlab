(** Implementation of product business logic. *)

open Types

(** Data Normalization *)
let normalize_part_number input =
  String_utils.clean_option input |> Option.map String.uppercase_ascii

(** Validation *)
let validate_name name =
  match String_utils.non_empty name with
  | None -> Error "Product name cannot be empty"
  | Some trimmed -> Ok trimmed

(** The Master Validator Returns the cleaned/normalized data ready for the DB *)
let validate_creation ~name ~manufacturer_part_number =
  match validate_name name with
  | Error msg -> Error msg
  | Ok valid_name ->
      let valid_pn = normalize_part_number manufacturer_part_number in
      Ok (valid_name, valid_pn)

(** Inventory Helper Check if a product matches a requirement. *)
let matches_requirement ~required_part_number product =
  match
    ( normalize_part_number (Some required_part_number),
      product.manufacturer_part_number )
  with
  | Some req, Some existing -> req = existing
  | _ -> false

let generate_short_id ~product_id = Printf.sprintf "PRD-%04d" product_id
