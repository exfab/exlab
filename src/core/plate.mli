(** Interface file for plate.ml. Contains business logic and utilities for
    handling microwell plates. *)

open Types

val validate_name : string -> (string, string) result
(** [validate_name name] returns [Ok name] if valid, or an [Error msg]. *)

val validate_format : string -> (plate_format, string) result
(** [validate_format format_str] parses a string into a [plate_format] or
    returns Error. *)

val generate_well_coordinates : plate_format -> string list
(** [generate_well_coordinates format] returns a list of alphanumeric
    coordinates (e.g. "A1", "A2") based on the plate dimensions. *)

val validate_sample_mixture :
  existing_categories:sample_category list ->
  new_category:sample_category ->
  (unit, string) result
(** [validate_sample_mixture ~existing_categories ~new_category] checks
    compliance rules. Returns Ok if the mixture is allowed, or Error msg if not.
    Mixing source and experimental samples is forbidden. *)

val get_plate_dimensions : plate_format -> int * int
(** [get_plate_dimensions format] returns a tuple of (rows, columns) for the
    specified plate format. *)

val coordinate_of_index : plate_format -> int -> (string, string) result
(** [coordinate_of_index format index] converts a 1-based integer index to an
    alphanumeric coordinate string. *)

val parse_coordinate : string -> (int * int, string) result
(** [parse_coordinate coord_str] parses an alphanumeric string (e.g. "A1") into
    a 0-based (row, column) index pair. *)

val index_of_coordinate : plate_format -> string -> (int, string) result
(** [index_of_coordinate format coord_str] converts an alphanumeric coordinate
    string to a 1-based integer index. *)

val normalize_coordinate : plate_format -> string -> (string, string) result
(** [normalize_coordinate format input] takes a string that could be a numeric
    index ("1") or an alphanumeric coordinate ("A1") and returns the
    standardized alphanumeric coordinate string (e.g., "A1"). *)

val compare_coordinates : string -> string -> int
(** [compare_coordinates c1 c2] compares two coordinates in "Column-Major"
    order. Order: A1 -> B1 -> C1 ... -> A2 -> B2 -> C2. Useful for sorting wells
    to match vertical workflow or specific equipment reading order. *)

val is_edge_well : plate_format -> string -> bool
(** [is_edge_well format coordinate] returns true if the coordinate lies on the
    outermost rows or columns of the plate. *)

val get_category_prefix : string option -> string
(** [get_category_prefix category_opt] returns the prefix for a plate based on
    its category. *)

val generate_short_id :
  category:string option -> project_id:int -> count:int -> string
(** [generate_short_id ~category ~project_id ~count] generates a new short ID
    for a plate. *)
