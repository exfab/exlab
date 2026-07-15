(** Handling for the ExternalDbDefinition data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields = "id, uid, name, url_template, created_at, updated_at"

let db_definition_t =
  let encode (d : Exlab_core.Types.external_db_definition) =
    Ok (d.id, d.uid, d.name, d.url_template, d.created_at, d.updated_at)
  in
  let decode (id, uid, name, url_template, created_at, updated_at) =
    Ok
      ({ id; uid; name; url_template; created_at; updated_at }
        : Exlab_core.Types.external_db_definition)
  in
  let rep = t6 int string string (option string) float float in
  Caqti_type.custom ~encode ~decode rep

(* SQL Queries *)

let get_all_query =
  (unit ->* db_definition_t)
    (Printf.sprintf "SELECT %s FROM external_db_definitions" select_fields)

let get_by_id_query =
  (int ->? db_definition_t)
    (Printf.sprintf "SELECT %s FROM external_db_definitions WHERE id = ?"
       select_fields)

let get_by_uid_query =
  (string ->? db_definition_t)
    (Printf.sprintf "SELECT %s FROM external_db_definitions WHERE uid = ?"
       select_fields)

let add_query =
  (t4 string string string float ->! db_definition_t)
    (Printf.sprintf
       "INSERT INTO external_db_definitions (uid, name, url_template, \
        created_at)\n\
       \          VALUES (?, ?, ?, ?)\n\
       \          RETURNING %s"
       select_fields)

let update_query =
  (t3 string string int ->! db_definition_t)
    (Printf.sprintf
       "UPDATE external_db_definitions\n\
       \        SET name = ?, url_template = ?\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

(* OCaml functions *)

(** [get_all ()] retrieves all external database definitions. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [get_by_id id] retrieves an external database definition by its internal ID.
*)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_uid uid] retrieves an external database definition by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [insert conn ~name ~url_template] inserts a new definition within a
    transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~name ~url_template =
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let* row = Conn.find add_query (uid, name, url_template, created_at) in
  Lwt.return (Ok row)

(** [add ~name ~url_template] creates a new external database definition. *)
let add ~name ~url_template =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert (module Conn) ~name ~url_template)

(** [modify conn ~id ~name ~url_template] updates a definition within a
    transaction. *)
let modify (module Conn : Caqti_lwt.CONNECTION) ~id ~name ~url_template =
  let* row = Conn.find update_query (name, url_template, id) in
  Lwt.return (Ok row)

(** [update ~id ~name ~url_template] updates an existing external database
    definition. *)
let update ~id ~name ~url_template =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify (module Conn) ~id ~name ~url_template)
