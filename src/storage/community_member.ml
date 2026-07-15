open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

let select_fields = "id, uid, community_id, strain_id, label, taxon, created_at"

let community_member_t =
  let encode (m : Exlab_core.Types.community_member) =
    Ok (m.id, m.uid, m.community_id, m.strain_id, m.label, m.taxon, m.created_at)
  in
  let decode (id, uid, community_id, strain_id, label, taxon, created_at) =
    Ok
      ({ id; uid; community_id; strain_id; label; taxon; created_at }
        : Exlab_core.Types.community_member)
  in
  let rep = t7 int string int (option int) string (option string) float in
  Caqti_type.custom ~encode ~decode rep

(* SQL Queries *)

let get_by_community_id_query =
  (int ->* community_member_t)
    (Printf.sprintf "SELECT %s FROM community_members WHERE community_id = ?"
       select_fields)

let add_query =
  (t6 string int (option int) string (option string) float
  ->! community_member_t)
    (Printf.sprintf
       "INSERT INTO community_members (uid, community_id, strain_id, label, \
        taxon, created_at)\n\
       \        VALUES (?, ?, ?, ?, ?, ?)\n\
       \        RETURNING %s"
       select_fields)

let remove_query = (int ->. unit) "DELETE FROM community_members WHERE id = ?"

let update_query =
  (t4 (option int) string (option string) int ->! community_member_t)
    (Printf.sprintf
       "UPDATE community_members\n\
       \        SET strain_id = ?, label = ?, taxon = ?\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

(* OCaml Functions *)

let get_by_community_id community_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_community_id_query community_id in
      Lwt.return (Ok rows))

let insert (module Conn : Caqti_lwt.CONNECTION) ~community_id ~strain_id ~label
    ~taxon =
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let* row =
    Conn.find add_query (uid, community_id, strain_id, label, taxon, created_at)
  in
  Lwt.return (Ok row)

let add ~community_id ~strain_id ~label ~taxon =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert (module Conn) ~community_id ~strain_id ~label ~taxon)

let remove id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.exec remove_query id)

let modify (module Conn : Caqti_lwt.CONNECTION) ~id ~strain_id ~label ~taxon =
  let* row = Conn.find update_query (strain_id, label, taxon, id) in
  Lwt.return (Ok row)

let update ~id ~strain_id ~label ~taxon =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify (module Conn) ~id ~strain_id ~label ~taxon)

let remove_by_community_id community_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let query =
        (int ->. unit) "DELETE FROM community_members WHERE community_id = ?"
      in
      Conn.exec query community_id)
