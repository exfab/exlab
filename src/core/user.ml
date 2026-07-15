(** Implementation of user business logic. *)

(* Handle role conversions *)
let role_of_string = Types.role_of_string
let role_to_string = Types.string_of_role

let validate_password password =
  if String.length password < 8 then
    Error "Password must be at least 8 characters long"
  else Ok ()

(* Password Hashing functions using passe *)

let hashed_password plaintext_password =
  match Passe.Argon2.hash plaintext_password with
  | Ok hash -> Passe.hash_to_string hash
  | Error _ -> failwith "Failed to hash password"

let verify_password ~plaintext ~hashed =
  let hash_type = Passe.hash_of_string hashed in
  match Passe.Argon2.verify ~hash:hash_type plaintext with
  | Ok is_match -> is_match
  | Error _ -> false
