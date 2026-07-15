(** Interface file for user.ml. Contains logic for user role management and
    password hashing. *)

open Types

val role_of_string : string -> (user_role, string) result
(** [role_of_string s] safely converts a string to a [user_role]. *)

val role_to_string : user_role -> string
(** [role_to_string role] converts a [user_role] back to its string
    representation. *)

val validate_password : string -> (unit, string) result
(** [validate_password password] checks if a password meets complexity
    requirements (e.g. minimum length). *)

val hashed_password : string -> string
(** [hashed_password plaintext] securely hashes a plaintext password using
    Argon2. *)

val verify_password : plaintext:string -> hashed:string -> bool
(** [verify_password ~plaintext ~hashed] verifies a plaintext password against a
    stored hash. *)
