(** String comparison, normalization, canonicalization, and typed parsing
    utilities. *)

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

val non_empty : string -> string option
(** [non_empty s] trims leading and trailing whitespace from [s], returning
    [None] if the result is empty and [Some trimmed] otherwise. *)

val clean_option : string option -> string option
(** [clean_option opt] trims [Some s] via {!non_empty}, collapsing
    whitespace-only values to [None]. *)

val find_canonical_ci : string -> string list -> string option
(** [find_canonical_ci target values] matches [target] case-insensitively
    against [values] (trimming both sides) and returns the trimmed canonical
    spelling from [values]. Returns [None] if [target] is blank or unmatched. *)

val to_int_option : string -> int option
(** [to_int_option s] trims [s] and parses an integer, returning [None] if blank
    or invalid. *)

val to_int : ?default:int -> string -> int
(** [to_int ?default s] parses an integer from [s], falling back to [default]
    (defaulting to [0]) if blank or invalid. *)

val to_float_option : string -> float option
(** [to_float_option s] trims [s] and parses a float, returning [None] if blank
    or invalid. *)

val to_float : ?default:float -> string -> float
(** [to_float ?default s] parses a float from [s], falling back to [default]
    (defaulting to [0.0]) if blank or invalid. *)

val to_bool_option : string -> bool option
(** [to_bool_option s] parses ["true"], ["1"], ["yes"], ["t"] as [Some true] and
    ["false"], ["0"], ["no"], ["f"] as [Some false] (case-insensitively, after
    trimming). *)

val to_bool : ?default:bool -> string -> bool
(** [to_bool ?default s] parses a boolean from [s], falling back to [default]
    (defaulting to [false]) if blank or unrecognized. *)

val split_trimmed : ?sep:char -> string -> string list
(** [split_trimmed ?sep s] splits [s] on [sep] (default [';']), trims each item,
    and discards blank entries. *)

val to_int_list : ?sep:char -> string -> int list
(** [to_int_list ?sep s] splits [s] via {!split_trimmed} and keeps valid ints.
*)

val to_float_list : ?sep:char -> string -> float list
(** [to_float_list ?sep s] splits [s] via {!split_trimmed} and keeps valid
    floats. *)
