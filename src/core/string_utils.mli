(** String utilities *)

val natural_compare : string -> string -> int
(** [natural_compare s1 s2] compares two strings naturally, meaning that numeric
    substrings are compared numerically rather than lexicographically. E.g.,
    "Plate_2" < "Plate_10". *)

val strip_trailing_slashes : string -> string
(** [strip_trailing_slashes s] removes all trailing ['/'] characters from [s].
*)
