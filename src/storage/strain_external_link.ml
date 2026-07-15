(** Handling for the StrainExternalLink data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields =
  "id, uid, strain_id, external_db_definition_id, value, created_at"

let strain_link_t =
  let encode (l : Exlab_core.Types.strain_external_link) =
    Ok
      ( l.id,
        l.uid,
        l.strain_id,
        l.external_db_definition_id,
        l.value,
        l.created_at )
  in
  let decode (id, uid, strain_id, external_db_definition_id, value, created_at)
      =
    Ok
      ({ id; uid; strain_id; external_db_definition_id; value; created_at }
        : Exlab_core.Types.strain_external_link)
  in
  Caqti_type.custom ~encode ~decode (t6 int string int int string float)

(* SQL Queries *)

let get_all_query =
  (unit ->* strain_link_t)
    (Printf.sprintf "SELECT %s FROM strain_external_links" select_fields)

let get_by_id_query =
  (int ->? strain_link_t)
    (Printf.sprintf "SELECT %s FROM strain_external_links WHERE id = ?"
       select_fields)

let get_by_uid_query =
  (string ->? strain_link_t)
    (Printf.sprintf "SELECT %s FROM strain_external_links WHERE uid = ?"
       select_fields)

let add_query =
  (t5 string int int string float ->! strain_link_t)
    (Printf.sprintf
       "INSERT INTO strain_external_links (uid, strain_id, \
        external_db_definition_id, value, created_at)\n\
       \          VALUES (?, ?, ?, ?, ?)\n\
       \          RETURNING %s"
       select_fields)

let update_query =
  (t4 int int string int ->! strain_link_t)
    (Printf.sprintf
       "UPDATE strain_external_links\n\
       \        SET strain_id = ?, external_db_definition_id = ?, value = ?\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

let delete_query =
  (int ->. unit) "DELETE FROM strain_external_links WHERE id = ?"

(* OCaml functions *)

(** [get_all ()] retrieves all strain external links. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [get_by_id id] retrieves a strain external link by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_uid uid] retrieves a strain external link by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [insert conn ~strain_id ~external_db_definition_id ~value] inserts a new
    link within a transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~strain_id
    ~external_db_definition_id ~value =
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let* row =
    Conn.find add_query
      (uid, strain_id, external_db_definition_id, value, created_at)
  in
  Lwt.return (Ok row)

(** [add ~strain_id ~external_db_definition_id ~value] creates a new strain
    external link. *)
let add ~strain_id ~external_db_definition_id ~value =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert (module Conn) ~strain_id ~external_db_definition_id ~value)

(** [modify conn ~id ~strain_id ~external_db_definition_id ~value] updates a
    link within a transaction. *)
let modify (module Conn : Caqti_lwt.CONNECTION) ~id ~strain_id
    ~external_db_definition_id ~value =
  let* row =
    Conn.find update_query (strain_id, external_db_definition_id, value, id)
  in
  Lwt.return (Ok row)

(** [update ~id ~strain_id ~external_db_definition_id ~value] updates an
    existing strain external link. *)
let update ~id ~strain_id ~external_db_definition_id ~value =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify (module Conn) ~id ~strain_id ~external_db_definition_id ~value)

(** [remove conn id] removes a link within a transaction. *)
let remove (module Conn : Caqti_lwt.CONNECTION) id = Conn.exec delete_query id

(** [delete id] removes a strain external link by its internal ID. *)
let delete id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      remove (module Conn) id)
