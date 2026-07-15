(** String utilities *)

val natural_compare : string -> string -> int
(** [natural_compare s1 s2] compares two strings naturally, meaning that numeric
    substrings are compared numerically rather than lexicographically. E.g.,
    "Plate_2" < "Plate_10". *)
