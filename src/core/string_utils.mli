(** String utilities *)

val natural_compare : string -> string -> int
(** [natural_compare s1 s2] compares two strings naturally, meaning that numeric
    substrings are compared numerically rather than lexicographically (e.g.,
    ["Plate_2" < "Plate_10"] and ["UCR-001-0001-10" < "UCR-001-0001-100"]).
    Strings that are numerically identical (such as ["01"] and ["1"]) are
    tie-broken lexicographically so [natural_compare] forms a total order safe
    for [List.sort_uniq]. *)

val strip_trailing_slashes : string -> string
(** [strip_trailing_slashes s] removes all trailing ['/'] characters from [s].
*)
