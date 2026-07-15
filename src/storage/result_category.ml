(** Handling for the ResultCategory data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields = "id, uid, name, description"

let result_category_t =
  let encode (c : Exlab_core.Types.result_category) =
    Ok (c.id, c.uid, c.name, c.description)
  in
  let decode (id, uid, name, description) =
    Ok ({ id; uid; name; description } : Exlab_core.Types.result_category)
  in
  Caqti_type.custom ~encode ~decode (t4 int string string (option string))

(* SQL Queries *)

let get_all_query =
  (unit ->* result_category_t)
    (Printf.sprintf "SELECT %s FROM result_categories" select_fields)

let get_by_id_query =
  (int ->? result_category_t)
    (Printf.sprintf "SELECT %s FROM result_categories WHERE id = ?"
       select_fields)

let get_by_uid_query =
  (string ->? result_category_t)
    (Printf.sprintf "SELECT %s FROM result_categories WHERE uid = ?"
       select_fields)

let add_query =
  (t3 string string (option string) ->! result_category_t)
    (Printf.sprintf
       "INSERT INTO result_categories (uid, name, description)\n\
       \        VALUES (?, ?, ?)\n\
       \        RETURNING %s"
       select_fields)

let update_query =
  (t3 string (option string) int ->! result_category_t)
    (Printf.sprintf
       "UPDATE result_categories SET name = ?, description = ? WHERE id = ? \
        RETURNING %s"
       select_fields)

(* OCaml functions *)

(** [get_all ()] retrieves all result categories. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [get_by_id id] retrieves a category by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_uid uid] retrieves a category by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [add ~name ~description] creates a new result category. *)
let add ~name ~description =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let uid = Utils.make_uuid () in
      let* row = Conn.find add_query (uid, name, description) in
      Lwt.return (Ok row))

(** [update ~id ~name ~description] updates an existing result category. *)
let update ~id ~name ~description =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find update_query (name, description, id) in
      Lwt_result.return row)
