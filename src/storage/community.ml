open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

type create_community = {
  name : string;
  notes : string option;
  metadata : Yojson.Safe.t option;
}

let select_fields = "id, uid, name, notes, metadata, created_at, updated_at"

let community_t =
  let encode (c : Exlab_core.Types.community) =
    let metadata_str =
      match c.metadata with
      | Some j -> Some (Yojson.Safe.to_string j)
      | None -> None
    in
    Ok (c.id, c.uid, c.name, c.notes, metadata_str, c.created_at, c.updated_at)
  in
  let decode (id, uid, name, notes, metadata_str, created_at, updated_at) =
    let metadata =
      match metadata_str with
      | Some s -> (
          match Yojson.Safe.from_string s with
          | j -> Some j
          | exception _ -> None)
      | None -> None
    in
    Ok
      ({ id; uid; name; notes; metadata; created_at; updated_at }
        : Exlab_core.Types.community)
  in
  let rep = t7 int string string (option string) (option string) float float in
  Caqti_type.custom ~encode ~decode rep

(* SQL Queries *)

let get_all_query =
  (unit ->* community_t)
    (Printf.sprintf "SELECT %s FROM communities" select_fields)

let add_query =
  (t5 string string (option string) (option string) float ->! community_t)
    (Printf.sprintf
       "INSERT INTO communities (uid, name, notes, metadata, created_at)\n\
       \        VALUES (?, ?, ?, ?::jsonb, ?)\n\
       \        RETURNING %s"
       select_fields)

let get_by_id_query =
  (int ->? community_t)
    (Printf.sprintf "SELECT %s FROM communities WHERE id = ?" select_fields)

let get_by_uid_query =
  (string ->? community_t)
    (Printf.sprintf "SELECT %s FROM communities WHERE uid = ?" select_fields)

let update_query =
  (t4 string (option string) (option string) int ->! community_t)
    (Printf.sprintf
       "UPDATE communities\n\
       \        SET name = ?, notes = ?, metadata = ?::jsonb\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

(* OCaml Functions *)

let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt.return (Ok rows))

let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row_opt = Conn.find_opt get_by_id_query id in
      Lwt.return (Ok row_opt))

let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row_opt = Conn.find_opt get_by_uid_query uid in
      Lwt.return (Ok row_opt))

let insert (module Conn : Caqti_lwt.CONNECTION) ~name ~notes ~metadata =
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let metadata_str = Option.map Yojson.Safe.to_string metadata in
  let* row = Conn.find add_query (uid, name, notes, metadata_str, created_at) in
  Lwt.return (Ok row)

let add ~name ~notes ~metadata =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert (module Conn) ~name ~notes ~metadata)

let modify (module Conn : Caqti_lwt.CONNECTION) ~id ~name ~notes ~metadata =
  let metadata_str = Option.map Yojson.Safe.to_string metadata in
  let* row = Conn.find update_query (name, notes, metadata_str, id) in
  Lwt.return (Ok row)

let update ~id ~name ~notes ~metadata =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify (module Conn) ~id ~name ~notes ~metadata)
