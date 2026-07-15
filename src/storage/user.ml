(** User data type and database operations. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields =
  "id, uid, email, password_hash, role, created_at, updated_at"

let user_t =
  let encode (u : Exlab_core.Types.user) =
    Ok
      ( u.id,
        u.uid,
        u.email,
        u.password_hash,
        Exlab_core.User.role_to_string u.role,
        u.created_at,
        u.updated_at )
  in
  let decode (id, uid, email, password_hash, role_str, created_at, updated_at) =
    match Exlab_core.User.role_of_string role_str with
    | Ok role ->
        Ok
          ({ id; uid; email; password_hash; role; created_at; updated_at }
            : Exlab_core.Types.user)
    | Error _ -> Error (Printf.sprintf "Invalid role string in DB: %s" role_str)
  in
  Caqti_type.custom ~encode ~decode
    (t7 int string string string string float float)

(* SQL Queries *)

let add_query =
  (t6 string string string string float float ->! user_t)
    (Printf.sprintf
       "INSERT INTO users (uid, email, password_hash, role, created_at, \
        updated_at)\n\
       \        VALUES (?, ?, ?, ?, ?, ?)\n\
       \        RETURNING %s"
       select_fields)

let get_by_id_query =
  (int ->? user_t)
    (Printf.sprintf "SELECT %s FROM users WHERE id = ?" select_fields)

let get_by_email_query =
  (string ->? user_t)
    (Printf.sprintf "SELECT %s FROM users WHERE email = ?" select_fields)

let get_all_query =
  (unit ->* user_t)
    (Printf.sprintf "SELECT %s FROM users ORDER BY created_at DESC"
       select_fields)

let search_query =
  (string ->* user_t)
    (Printf.sprintf
       "SELECT %s FROM users WHERE email ILIKE '%%' || ? || '%%' ORDER BY \
        created_at DESC"
       select_fields)

let update_query =
  (t4 string string string int ->! user_t)
    (Printf.sprintf
       "UPDATE users\n\
       \        SET email = ?, password_hash = ?, role = ?\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

let update_password_query =
  (t2 string int ->. unit)
    "UPDATE users SET password_hash = ?, updated_at = extract(epoch from \
     now()) WHERE id = ?"

let delete_query = (int ->. unit) "DELETE FROM users WHERE id = ?"

(* OCaml Functions *)

(** [insert conn ~email ~password ~role] inserts a new user within a
    transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~email ~password ~role =
  let role_str = Exlab_core.User.role_to_string role in
  let uid = Utils.make_uuid () in
  let now = Unix.time () in
  (* Hash the plaintext password *)
  let password_hash = Exlab_core.User.hashed_password password in
  let* row =
    Conn.find add_query (uid, email, password_hash, role_str, now, now)
  in
  Lwt.return (Ok row)

(** [add ~email ~password ~role] creates a new user. *)
let add ~email ~password ~role =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert (module Conn) ~email ~password ~role)

(** [get_by_id id] retrieves a user by their internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_email email] retrieves a user by their email address. *)
let get_by_email email =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_email_query email in
      Lwt_result.return row)

(** [get_all ()] retrieves all users, ordered by creation date descending. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [search email] retrieves users where email matches term, ordered by creation
    date descending. *)
let search term =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list search_query term in
      Lwt_result.return rows)

(** [modify conn ~id ~email ~password_hash ~role] updates a user within a
    transaction. *)
let modify (module Conn : Caqti_lwt.CONNECTION) ~id ~email ~password_hash ~role
    =
  let role_str = Exlab_core.User.role_to_string role in
  let* row = Conn.find update_query (email, password_hash, role_str, id) in
  Lwt.return (Ok row)

(** [update ~id ~email ~password_hash ~role] updates an existing user. Wrapped
    in a transaction. *)
let update ~id ~email ~password_hash ~role =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify (module Conn) ~id ~email ~password_hash ~role)

(** [delete id] removes a user by their internal ID. *)
let delete id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.exec delete_query id)

(** [update_password id new_password] hashes the new password and updates the
    database row. *)
let update_password id new_password =
  let password_hash = Exlab_core.User.hashed_password new_password in
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.exec update_password_query (password_hash, id))
