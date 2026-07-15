(** Handling for the Project data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields =
  "id, uid, short_id, name, description, status, contact_name, owner, \
   metadata, created_at, updated_at"

let project_t =
  let encode (p : Exlab_core.Types.project) =
    let status_str = string_of_entity_status p.status in
    let metadata_str = Option.map Yojson.Safe.to_string p.metadata in
    Ok
      ( p.id,
        p.uid,
        p.short_id,
        p.name,
        p.description,
        status_str,
        p.contact_name,
        p.owner,
        metadata_str,
        p.created_at,
        p.updated_at )
  in
  let decode
      ( id,
        uid,
        short_id,
        name,
        description,
        status_str,
        contact_name,
        owner,
        metadata_str,
        created_at,
        updated_at ) =
    let metadata = Option.map Yojson.Safe.from_string metadata_str in
    match entity_status_of_string status_str with
    | Ok status ->
        Ok
          ({
             id;
             uid;
             short_id;
             name;
             description;
             status;
             contact_name;
             owner;
             metadata;
             created_at;
             updated_at;
           }
            : Exlab_core.Types.project)
    | Error _ ->
        Error (Printf.sprintf "Invalid status string in DB: %s" status_str)
  in
  let rep =
    t11 int string string string (option string) string (option string)
      (option string) (option string) float float
  in
  Caqti_type.custom ~encode ~decode rep

(* SQL Queries *)

let get_all_query =
  (unit ->* project_t) (Printf.sprintf "SELECT %s FROM projects" select_fields)

let get_all_for_user_query =
  (int ->* project_t)
    (Printf.sprintf
       "SELECT p.id, p.uid, p.short_id, p.name, p.description, p.status, \
        p.contact_name, p.owner, p.metadata, p.created_at, p.updated_at FROM \
        projects p INNER JOIN project_users pu ON p.id = pu.project_id WHERE \
        pu.user_id = ?")

let get_by_short_id_query =
  (string ->? project_t)
    (Printf.sprintf "SELECT %s FROM projects WHERE short_id = ?" select_fields)

let get_by_id_query =
  (int ->? project_t)
    (Printf.sprintf "SELECT %s FROM projects WHERE id = ?" select_fields)

let get_by_uid_query =
  (string ->? project_t)
    (Printf.sprintf "SELECT %s FROM projects WHERE uid = ?" select_fields)

let get_many_by_ids_query =
  (string ->* project_t)
    (Printf.sprintf "SELECT %s FROM projects WHERE id = ANY(?::int[])"
       select_fields)

let add_query =
  (t9 string string string float (option string) string (option string)
     (option string) (option string)
  ->! project_t)
    (Printf.sprintf
       "INSERT INTO projects (uid, short_id, name, created_at, description, \
        status, contact_name, owner, metadata)\n\
       \        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?::jsonb)\n\
       \        RETURNING %s"
       select_fields)

let set_short_id_query =
  (t2 string int ->. unit) "UPDATE projects SET short_id = ? WHERE id = ?"

let update_query =
  (t7 string (option string) string (option string) (option string)
     (option string) string
  ->! project_t)
    (Printf.sprintf
       "UPDATE projects\n\
       \        SET name = ?, description = ?, status = ?, contact_name = ?, \
        owner = ?, metadata = ?::jsonb\n\
       \        WHERE short_id = ?\n\
       \        RETURNING %s"
       select_fields)

let delete_query =
  (string ->. unit) "UPDATE projects SET status = 'Archived' WHERE short_id = ?"

(* OCaml Functions *)

(** [get_all ()] retrieves all projects. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [get_all_for_user user_id] retrieves all projects accessible to a user. *)
let get_all_for_user user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_for_user_query user_id in
      Lwt_result.return rows)

(** [insert conn ~name ~description ~status ~contact_name ~owner ?metadata
     ?short_id ()] inserts a project within a transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~name ~description ~status
    ~contact_name ~owner ?metadata ?short_id () =
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let status_str = string_of_entity_status status in
  let metadata_str = Option.map Yojson.Safe.to_string metadata in

  match short_id with
  | Some custom_id ->
      let* project =
        Conn.find add_query
          ( uid,
            custom_id,
            name,
            created_at,
            description,
            status_str,
            contact_name,
            owner,
            metadata_str )
      in
      Lwt.return (Ok project)
  | None ->
      let temp_short_id = uid in

      let* project =
        Conn.find add_query
          ( uid,
            temp_short_id,
            name,
            created_at,
            description,
            status_str,
            contact_name,
            owner,
            metadata_str )
      in

      let auto_gen_id =
        Exlab_core.Project.generate_short_id ~project_id:project.id
      in

      let* () = Conn.exec set_short_id_query (auto_gen_id, project.id) in

      let* updated_project =
        let* project_opt = Conn.find_opt get_by_id_query project.id in
        match project_opt with
        | Some p -> Lwt.return (Ok p)
        | None ->
            Lwt.return
              (Error (`Not_Found "Failed to fetch project after creation"))
      in
      Lwt.return (Ok updated_project)

(** [add ~name ~description ~status ~contact_name ~owner ?metadata ?short_id ()]
    creates a new project. *)
let add ~name ~description ~status ~contact_name ~owner ?metadata ?short_id () =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert
        (module Conn)
        ~name ~description ~status ~contact_name ~owner ?metadata ?short_id ())

(** [get_by_short_id short_id] retrieves a project by its short ID. *)
let get_by_short_id short_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_short_id_query short_id in
      Lwt_result.return row)

(** [get_by_id id] retrieves a project by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_many_by_ids ids] retrieves multiple projects by their internal IDs. *)
let get_many_by_ids ids =
  if ids = [] then Lwt_result.return []
  else
    let param = Utils.pg_array_string_of_ints ids in
    Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
        let* rows = Conn.collect_list get_many_by_ids_query param in
        Lwt_result.return rows)

(** [get_by_uid uid] retrieves a project by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [modify conn ~short_id ~name ~description ~status ~contact_name ~owner
     ?metadata] updates a project within a transaction. *)
let modify (module Conn : Caqti_lwt.CONNECTION) ~short_id ~name ~description
    ~status ~contact_name ~owner ?metadata () =
  let status_str = string_of_entity_status status in
  let metadata_str = Option.map Yojson.Safe.to_string metadata in
  let* project =
    Conn.find update_query
      ( name,
        description,
        status_str,
        contact_name,
        owner,
        metadata_str,
        short_id )
  in
  Lwt.return (Ok project)

(** [update ~short_id ~name ~description ~status ~contact_name ~owner ?metadata]
    updates an existing project. *)
let update ~short_id ~name ~description ~status ~contact_name ~owner ?metadata
    () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify
        (module Conn)
        ~short_id ~name ~description ~status ~contact_name ~owner ?metadata ())

(** [delete short_id] archives a project. *)
let delete short_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.exec delete_query short_id)
