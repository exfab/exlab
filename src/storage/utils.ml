(** Utility functions for the storage layer. *)

(** State for random number generation. *)
let random_state = Random.State.make_self_init ()

(** [generate_uuid_v4] is a UUID v4 generator. *)
let generate_uuid_v4 = Uuidm.v4_gen random_state

(** [make_uuid ()] generates a new UUID v4 string. *)
let make_uuid () = generate_uuid_v4 () |> Uuidm.to_string

(** [pg_array_string_of_strings items] converts a list of strings to a
    PostgreSQL array literal. *)
let pg_array_string_of_strings (items : string list) : string =
  let quoted_items = List.map (fun s -> "\"" ^ s ^ "\"") items in
  "{" ^ String.concat "," quoted_items ^ "}"

(** [pg_array_string_of_ints items] converts a list of integers to a PostgreSQL
    array literal. *)
let pg_array_string_of_ints (items : int list) : string =
  "{" ^ String.concat "," (List.map string_of_int items) ^ "}"

(** [unwrap_or_fail result] returns the value if [Ok], or raises a Failure
    exception with the error message if [Error]. Used when reading trusted data
    from the database where a parsing error indicates database corruption. *)
let unwrap_or_fail = function Ok v -> v | Error msg -> failwith msg
