(** Interface file for product.ml. Contains business logic for product catalog
    entries. *)

open Types

val normalize_part_number : string option -> string option
(** [normalize_part_number input] converts a part number to uppercase and trims
    it, returning [None] if empty. *)

val validate_name : string -> (string, string) result
(** [validate_name name] ensures a product name is not empty. Returns the
    trimmed name or an error. *)

val validate_creation :
  name:string ->
  manufacturer_part_number:string option ->
  (string * string option, string) result
(** [validate_creation ~name ~manufacturer_part_number] normalizes and validates
    inputs prior to product creation. Returns a tuple of cleaned strings. *)

val matches_requirement : required_part_number:string -> product -> bool
(** [matches_requirement ~required_part_number product] checks if the product
    matches a specified required part number. *)

val generate_short_id : product_id:int -> string
(** [generate_short_id ~product_id] generates a short ID for a product. *)
