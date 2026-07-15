(** Implementation of product business logic. *)

open Types

(** Data Normalization *)
let normalize_part_number input =
  match input with
  | None -> None
  | Some s ->
      let trimmed = String.trim s in
      if trimmed = "" then None else Some (String.uppercase_ascii trimmed)

(** Validation *)
let validate_name name =
  let trimmed = String.trim name in
  if trimmed = "" then Error "Product name cannot be empty" else Ok trimmed

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
