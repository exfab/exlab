(** CSV structural parsing utilities.

    Handles reading CSV headers, associating columns safely, iterating rows with
    a row-accessor decoder, and parsing 2D plate matrices. Pure string cleaning
    and typed parsing live in {!Exlab_core.String_utils}. *)

type row_accessor = string -> string
(** Lookup function that returns the raw cell string for a column header (or
    [""] if the column is absent). *)

val is_blank_row : string list -> bool
(** [is_blank_row row] returns [true] when [row] has no non-whitespace cells. *)

val read_rows : string -> (string list list, string) result
(** [read_rows body] parses a raw CSV string into rows while converting
    [Csv.Failure] syntax errors into [Error] messages instead of raising. *)

val safe_associate : string list -> string list -> (string * string) list
(** [safe_associate header row] zips column headers with row cells, padding
    short rows with [""] and ignoring trailing extra cells. *)

val read_csv_with_headers : string -> (string * string) list list
(** [read_csv_with_headers body] parses a CSV string whose first row contains
    column headers (trimmed) into a list of header-to-cell association lists. *)

val parse :
  string -> (row_accessor -> ('a, string) result) -> ('a list, string) result
(** [parse body decoder] decodes every non-blank data row in [body] using
    [decoder], prefixing any decode error with its 1-based CSV row number
    (starting at row 2 after the header). *)

val parse_matrix :
  string ->
  (Api_types.Plate.well_layout_item list, [> `Bad_Request of string ]) result
(** [parse_matrix body] parses a 2D plate layout CSV (row letters in column 0,
    column numbers in row 0, sample short IDs in cells) into well assignments,
    trimming coordinates and skipping blank rows. *)
