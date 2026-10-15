(** Handling for the Sample data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields =
  "id, uid, short_id, project_id, sample_type, status, category, \
   parent_sample_id, strain_id, community_id, created_at, updated_at"

let sample_t =
  let encode (s : Exlab_core.Types.sample) =
    let category_str = string_of_sample_category s.category in
    let status_str = string_of_entity_status s.status in
    Ok
      ( s.id,
        s.uid,
        s.short_id,
        s.project_id,
        s.sample_type,
        status_str,
        category_str,
        s.parent_sample_id,
        s.strain_id,
        s.community_id,
        s.created_at,
        s.updated_at )
  in
  let decode
      ( id,
        uid,
        short_id,
        project_id,
        sample_type,
        status_str,
        category_str,
        parent_sample_id,
        strain_id,
        community_id,
        created_at,
        updated_at ) =
    match
      ( sample_category_of_string category_str,
        entity_status_of_string status_str )
    with
    | Ok category, Ok status ->
        Ok
          ({
             category;
             id;
             uid;
             short_id;
             project_id;
             sample_type;
             status;
             parent_sample_id;
             strain_id;
             community_id;
             created_at;
             updated_at;
           }
            : Exlab_core.Types.sample)
    | Error _, _ ->
        Error (Printf.sprintf "Invalid category string in DB: %s" category_str)
    | _, Error _ ->
        Error (Printf.sprintf "Invalid status string in DB: %s" status_str)
  in
  let rep =
    t12 int string string int string string string (option int) (option int)
      (option int) float float
  in
  Caqti_type.custom ~encode ~decode rep

(* SQL Queries *)

(* Fetch all samples associated with a given project ID. *)
let get_by_plate_id_query =
  (int ->* sample_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at FROM samples s INNER JOIN wells w ON \
        w.sample_id = s.id WHERE w.plate_id = ?")

let get_by_project_id_query =
  (int ->* sample_t)
    (Printf.sprintf "SELECT %s FROM samples WHERE project_id = ?" select_fields)

let search_by_project_query =
  (t4 int string string string ->* sample_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at FROM samples s LEFT JOIN strains st ON \
        s.strain_id = st.id WHERE s.project_id = ? AND (s.short_id ILIKE '%%' \
        || ? || '%%' OR s.uid ILIKE '%%' || ? || '%%' OR st.strain_name ILIKE \
        '%%' || ? || '%%') LIMIT 50")

(* Return the short_id of the project *)
let get_project_short_id_query =
  (int ->? string) "SELECT short_id FROM projects WHERE id = ?"

(* Insert a new sample and return the created row. *)
let add_query =
  (t10 string string int string string string (option int) (option int)
     (option int) float
  ->! sample_t)
    (Printf.sprintf
       "INSERT INTO samples (uid, short_id, project_id, sample_type, status, \
        category, parent_sample_id, strain_id, community_id, created_at)\n\
       \        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)\n\
       \        RETURNING %s"
       select_fields)

(* GET all samples in the database *)
let get_all_query =
  (unit ->* sample_t) (Printf.sprintf "SELECT %s FROM samples" select_fields)

let search_all_query =
  (t3 string string string ->* sample_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at FROM samples s LEFT JOIN strains st ON \
        s.strain_id = st.id WHERE s.short_id ILIKE '%%' || ? || '%%' OR s.uid \
        ILIKE '%%' || ? || '%%' OR st.strain_name ILIKE '%%' || ? || '%%' \
        LIMIT 50")

let get_all_for_user_query =
  (int ->* sample_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at FROM samples s INNER JOIN project_users pu \
        ON s.project_id = pu.project_id WHERE pu.user_id = ?")

let search_all_for_user_query =
  (t4 int string string string ->* sample_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at FROM samples s INNER JOIN project_users pu \
        ON s.project_id = pu.project_id LEFT JOIN strains st ON s.strain_id = \
        st.id WHERE pu.user_id = ? AND (s.short_id ILIKE '%%' || ? || '%%' OR \
        s.uid ILIKE '%%' || ? || '%%' OR st.strain_name ILIKE '%%' || ? || \
        '%%') LIMIT 50")

type sample_filters = {
  term : string;
  project_id : int option;
  category : string option;
  sample_type : string option;
  strain_id : int option;
}

let filters_t =
  let encode f =
    Ok
      ( (f.term, f.term, f.term, f.project_id, f.project_id, f.category),
        (f.category, f.sample_type, f.sample_type, f.strain_id, f.strain_id) )
  in
  let decode _ = Error "Decode not implemented for filters" in
  Caqti_type.custom ~encode ~decode
    Caqti_type.Std.(
      t2
        (t6 string string string (option int) (option int) (option string))
        (t5 (option string) (option string) (option string) (option int)
           (option int)))

let search_with_filters_query =
  (filters_t ->* sample_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at FROM samples s LEFT JOIN strains st ON \
        s.strain_id = st.id WHERE (s.short_id ILIKE '%%' || ? || '%%' OR s.uid \
        ILIKE '%%' || ? || '%%' OR st.strain_name ILIKE '%%' || ? || '%%') AND \
        (?::int IS NULL OR s.project_id = ?) AND (?::text IS NULL OR \
        s.category = ?) AND (?::text IS NULL OR s.sample_type = ?) AND (?::int \
        IS NULL OR s.strain_id = ?) ORDER BY s.created_at DESC LIMIT 200")

type user_sample_filters = { user_id : int; filters : sample_filters }

let user_filters_t =
  let encode uf =
    let f = uf.filters in
    Ok
      ( (uf.user_id, f.term, f.term, f.term, f.project_id, f.project_id),
        ( f.category,
          f.category,
          f.sample_type,
          f.sample_type,
          f.strain_id,
          f.strain_id ) )
  in
  let decode _ = Error "Decode not implemented" in
  Caqti_type.custom ~encode ~decode
    Caqti_type.Std.(
      t2
        (t6 int string string string (option int) (option int))
        (t6 (option string) (option string) (option string) (option string)
           (option int) (option int)))

let search_with_filters_for_user_query =
  (user_filters_t ->* sample_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at FROM samples s INNER JOIN project_users pu \
        ON s.project_id = pu.project_id LEFT JOIN strains st ON s.strain_id = \
        st.id WHERE pu.user_id = ? AND (s.short_id ILIKE '%%' || ? || '%%' OR \
        s.uid ILIKE '%%' || ? || '%%' OR st.strain_name ILIKE '%%' || ? || \
        '%%') AND (?::int IS NULL OR s.project_id = ?) AND (?::text IS NULL OR \
        s.category = ?) AND (?::text IS NULL OR s.sample_type = ?) AND (?::int \
        IS NULL OR s.strain_id = ?) ORDER BY s.created_at DESC LIMIT 200")

let get_many_by_short_ids_query =
  (string ->* sample_t)
    (Printf.sprintf "SELECT %s FROM samples WHERE short_id = ANY(?)"
       select_fields)

let get_many_by_ids_query =
  (string ->* sample_t)
    (Printf.sprintf "SELECT %s FROM samples WHERE id = ANY(?::int[])"
       select_fields)

let get_by_parent_id_query =
  (int ->* sample_t)
    (Printf.sprintf "SELECT %s FROM samples WHERE parent_sample_id = ?"
       select_fields)

let get_by_id_query =
  (int ->? sample_t)
    (Printf.sprintf "SELECT %s FROM samples WHERE id = ?" select_fields)

let get_by_short_id_query =
  (string ->? sample_t)
    (Printf.sprintf "SELECT %s FROM samples WHERE short_id = ?" select_fields)

let get_by_uid_query =
  (string ->? sample_t)
    (Printf.sprintf "SELECT %s FROM samples WHERE uid = ?" select_fields)

let get_source_by_strain_id_query =
  (int ->* sample_t)
    (Printf.sprintf
       "SELECT %s FROM samples WHERE strain_id = ? AND category = 'Source'"
       select_fields)

let get_source_by_community_id_query =
  (int ->* sample_t)
    (Printf.sprintf
       "SELECT %s FROM samples WHERE community_id = ? AND category = 'Source'"
       select_fields)

let source_with_project_t = Caqti_type.Std.t2 sample_t Project.project_t

let get_source_with_project_by_strain_id_query =
  (Caqti_type.Std.int ->* source_with_project_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at, p.id, p.uid, p.short_id, p.name, \
        p.description, p.status, p.contact_name, p.owner, p.metadata, \
        p.created_at, p.updated_at FROM samples s INNER JOIN projects p ON \
        s.project_id = p.id WHERE s.strain_id = ? AND s.category = 'Source'")

let get_source_with_project_by_community_id_query =
  (Caqti_type.Std.int ->* source_with_project_t)
    (Printf.sprintf
       "SELECT s.id, s.uid, s.short_id, s.project_id, s.sample_type, s.status, \
        s.category, s.parent_sample_id, s.strain_id, s.community_id, \
        s.created_at, s.updated_at, p.id, p.uid, p.short_id, p.name, \
        p.description, p.status, p.contact_name, p.owner, p.metadata, \
        p.created_at, p.updated_at FROM samples s INNER JOIN projects p ON \
        s.project_id = p.id WHERE s.community_id = ? AND s.category = 'Source'")

let get_all_children_query =
  (int ->* int)
    "WITH RECURSIVE descendants AS (\n\
    \   SELECT id, parent_sample_id FROM samples WHERE parent_sample_id = ?\n\
    \   UNION\n\
    \   SELECT s.id, s.parent_sample_id FROM samples s\n\
    \   INNER JOIN descendants d ON s.parent_sample_id = d.id\n\
    \   )\n\
    \   SELECT id FROM descendants\n\
    \   "

let update_strain_id_for_many_query =
  (t2 (option int) string ->. unit)
    "UPDATE samples SET strain_id = ? WHERE id = ANY(?::int[])"

let update_community_id_for_many_query =
  (t2 (option int) string ->. unit)
    "UPDATE samples SET community_id = ? WHERE id = ANY(?::int[])"

let update_query =
  (t7 string (option string) (option int) (option int) (option int) string int
  ->! sample_t)
    (Printf.sprintf
       "UPDATE samples \n\
       \        SET \n\
       \        sample_type = ?, \n\
       \        category = COALESCE(?, category), \n\
       \        parent_sample_id = ?,\n\
       \        strain_id = ?,\n\
       \        community_id = ?,\n\
       \        status = ?\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

let archive_query =
  (int ->. unit) "UPDATE samples SET status = 'Archived' WHERE id = ?"

(* OCaml functions *)

(** [get_by_id id] retrieves a sample by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_uid uid] retrieves a sample by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [get_by_short_id short_id] retrieves a sample by its short ID. *)
let get_by_short_id short_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_short_id_query short_id in
      Lwt_result.return row)

(** [get_many_by_short_ids_tx conn short_ids] retrieves multiple samples by
    their short IDs within a transaction. *)
let get_many_by_short_ids_tx (module Conn : Caqti_lwt.CONNECTION) short_ids =
  if short_ids = [] then Lwt_result.return []
  else
    let param = Utils.pg_array_string_of_strings short_ids in
    Conn.collect_list get_many_by_short_ids_query param

(** [get_many_by_short_ids short_ids] retrieves multiple samples by their short
    IDs. *)
let get_many_by_short_ids short_ids =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      get_many_by_short_ids_tx (module Conn) short_ids)

(** [get_many_by_ids_tx conn ids] retrieves multiple samples by their internal
    IDs within a transaction. *)
let get_many_by_ids_tx (module Conn : Caqti_lwt.CONNECTION) ids =
  if ids = [] then Lwt_result.return []
  else
    let param = Utils.pg_array_string_of_ints ids in
    Conn.collect_list get_many_by_ids_query param

(** [get_many_by_ids ids] retrieves multiple samples by their internal IDs. *)
let get_many_by_ids ids =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      get_many_by_ids_tx (module Conn) ids)

(** [modify conn ~id ~sample_type ~category ~parent_sample_id ~strain_id
     ~community_id ~status] updates a sample within a transaction. *)
let modify (module Conn : Caqti_lwt.CONNECTION) ~id ~sample_type ~category
    ~parent_sample_id ~strain_id ~community_id ~status =
  let open Lwt_result.Syntax in
  let* original_sample_opt = Conn.find_opt get_by_id_query id in
  let* original_sample =
    match original_sample_opt with
    | Some r -> Lwt.return (Ok r)
    | None -> Lwt.return (Error (`Not_Found "Original sample not found"))
  in

  (* Validation *)
  let* () =
    match Exlab_core.Sample.validate_sample_type sample_type with
    | Error err -> Lwt.return (Error (`Msg err))
    | Ok () -> Lwt.return (Ok ())
  in

  let status_str = string_of_entity_status status in

  (* Execution *)
  let* row =
    Conn.find update_query
      ( sample_type,
        category,
        parent_sample_id,
        strain_id,
        community_id,
        status_str,
        id )
  in

  let* () =
    if
      original_sample.category = Source
      && original_sample.strain_id <> strain_id
    then
      let* children_ids = Conn.collect_list get_all_children_query id in
      match children_ids with
      | [] -> Lwt.return (Ok ())
      | _ ->
          let array_param = Utils.pg_array_string_of_ints children_ids in
          Conn.exec update_strain_id_for_many_query (strain_id, array_param)
    else Lwt.return (Ok ())
  in

  let* () =
    if
      original_sample.category = Source
      && original_sample.community_id <> community_id
    then
      let* children_ids = Conn.collect_list get_all_children_query id in
      match children_ids with
      | [] -> Lwt.return (Ok ())
      | _ ->
          let array_param = Utils.pg_array_string_of_ints children_ids in
          Conn.exec update_community_id_for_many_query
            (community_id, array_param)
    else Lwt.return (Ok ())
  in

  Lwt.return (Ok row)

(** [update ~id ~sample_type ~category ~parent_sample_id ~strain_id
     ~community_id ~status] updates an existing sample. *)
let update ~id ~sample_type ~category ~parent_sample_id ~strain_id ~community_id
    ~status =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify
        (module Conn)
        ~id ~sample_type ~category ~parent_sample_id ~strain_id ~community_id
        ~status)

(** [archive id] sets a sample's status to 'Archived'. *)
let archive id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.exec archive_query id)

(** [get_by_plate_id plate_id] retrieves all samples present in the wells of a
    specific plate. *)
let get_by_plate_id plate_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_plate_id_query plate_id in
      Lwt_result.return rows)

(** [get_by_project_id project_id] retrieves all samples for a specific project.
*)
let get_by_project_id project_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_project_id_query project_id in
      Lwt_result.return rows)

(** [get_all ()] retrieves all samples globally. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [get_all_for_user user_id] retrieves all samples accessible to a user. *)
let get_all_for_user user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_for_user_query user_id in
      Lwt_result.return rows)

(** [search_all term] searches all samples globally. *)
let search_all term =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list search_all_query (term, term, term) in
      Lwt_result.return rows)

(** [search_all_for_user user_id term] searches all samples accessible to a
    user. *)
let search_all_for_user user_id term =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        Conn.collect_list search_all_for_user_query (user_id, term, term, term)
      in
      Lwt_result.return rows)

(** [search_with_filters filters] searches samples using multiple optional
    database filters. *)
let search_with_filters filters =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list search_with_filters_query filters in
      Lwt_result.return rows)

(** [search_with_filters_for_user user_id filters] searches samples accessible
    to a user using multiple optional database filters. *)
let search_with_filters_for_user user_id filters =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let uf = { user_id; filters } in
      let* rows = Conn.collect_list search_with_filters_for_user_query uf in
      Lwt_result.return rows)

(** [search_by_project project_id term] retrieves samples matching the term for
    a specific project. *)
let search_by_project project_id term =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        Conn.collect_list search_by_project_query (project_id, term, term, term)
      in
      Lwt_result.return rows)

(** [get_by_parent_id parent_id] retrieves all child samples for a given parent
    sample ID. *)
let get_by_parent_id parent_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_parent_id_query parent_id in
      Lwt_result.return rows)

(** [get_source_by_strain_id strain_id] retrieves all source samples associated
    with a given strain ID. *)
let get_source_by_strain_id strain_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let%lwt result =
        Conn.collect_list get_source_by_strain_id_query strain_id
      in
      match result with
      | Ok rows -> Lwt.return (Ok rows)
      | Error err -> Lwt.return (Error err))

(** [get_source_by_community_id community_id] retrieves all source samples
    associated with a given community ID. *)
let get_source_by_community_id community_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        Conn.collect_list get_source_by_community_id_query community_id
      in
      Lwt_result.return rows)

(** [get_source_with_project_by_strain_id strain_id] retrieves all source
    samples with their associated projects for a given strain ID. *)
let get_source_with_project_by_strain_id strain_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        Conn.collect_list get_source_with_project_by_strain_id_query strain_id
      in
      Lwt_result.return rows)

(** [get_source_with_project_by_community_id community_id] retrieves all source
    samples with their associated projects for a given community ID. *)
let get_source_with_project_by_community_id community_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        Conn.collect_list get_source_with_project_by_community_id_query
          community_id
      in
      Lwt_result.return rows)

(** [insert_row conn ~short_id ~project_id ~sample_type ~category
     ~parent_sample_id ~strain_id ~community_id ~result_definition_ids] inserts
    a validated sample row and its initial result values within a transaction.
*)
let insert_row (module Conn : Caqti_lwt.CONNECTION) ~short_id ~project_id
    ~sample_type ~category ~parent_sample_id ~strain_id ~community_id
    ~result_definition_ids =
  let open Lwt_result.Syntax in
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let status = Exlab_core.Types.Active in
  let status_str = string_of_entity_status status in
  let category_str = string_of_sample_category category in

  (* Insert the sample *)
  let* sample =
    Conn.find add_query
      ( uid,
        short_id,
        project_id,
        sample_type,
        status_str,
        category_str,
        parent_sample_id,
        strain_id,
        community_id,
        created_at )
  in
  let id = sample.id in

  (* Handle Result Values *)
  let* () =
    Lwt_list.fold_left_s
      (fun acc result_def_id ->
        match acc with
        | Error e -> Lwt.return (Error e)
        | Ok () -> (
            let* def_opt =
              Result_definition.lookup (module Conn) result_def_id
            in

            match def_opt with
            | None ->
                Lwt.return
                  (Error
                     (`Not_Found
                        (Printf.sprintf "Result definition %d not found"
                           result_def_id)))
            | Some definition ->
                let* _ =
                  Result_value.insert
                    (module Conn)
                    ~parent:(Result_value.Sample id) ~definition ~value:None
                in
                Lwt.return (Ok ())))
      (Ok ()) result_definition_ids
  in

  Lwt.return (Ok sample)

(** [resolve_sample_context ~project_id ~project_short_id_opt ~parent_opt
     ~sample_type ~category ~parent_sample_id ~strain_id ~community_id]
    validates sample creation rules, inherits strain/community from the parent
    for Experimental samples, and returns the short-ID owner and counter key. *)
let resolve_sample_context ~project_id ~project_short_id_opt
    ~(parent_opt : Exlab_core.Types.sample option) ~sample_type ~category
    ~parent_sample_id ~strain_id ~community_id =
  let final_strain_id =
    match (category, strain_id, parent_opt) with
    | Experimental, None, Some p -> p.strain_id
    | _ -> strain_id
  in
  let final_community_id =
    match (category, community_id, parent_opt) with
    | Experimental, None, Some p -> p.community_id
    | _ -> community_id
  in
  match
    Exlab_core.Sample.validate_creation ~sample_type ~category ~parent_sample_id
      ~strain_id:final_strain_id ~community_id:final_community_id
  with
  | Error msg -> Error (`Msg msg)
  | Ok () -> (
      match (category, parent_opt, project_short_id_opt) with
      | Experimental, Some p, _ ->
          Ok
            ( `Parent p.short_id,
              (Short_id_counter.Experimental_sample, p.id),
              final_strain_id,
              final_community_id )
      | Experimental, None, _ -> Error (`Not_Found "Parent sample not found")
      | Source, _, Some proj_short_id ->
          Ok
            ( `Project proj_short_id,
              (Short_id_counter.Source_sample, project_id),
              final_strain_id,
              final_community_id )
      | Source, _, None -> Error (`Not_Found "Project not found"))

(** [insert conn ~project_id ~sample_type ~category ~parent_sample_id ~strain_id
     ~community_id ?result_definition_ids ()] inserts a new sample within a
    transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~project_id ~sample_type
    ~category ~parent_sample_id ~strain_id ~community_id
    ?(result_definition_ids = []) () =
  let open Lwt_result.Syntax in
  let* parent_opt =
    match (category, parent_sample_id) with
    | Experimental, Some p_id -> (
        let* p_opt = Conn.find_opt get_by_id_query p_id in
        match p_opt with
        | Some p -> Lwt.return (Ok (Some p))
        | None -> Lwt.return (Error (`Not_Found "Parent sample not found")))
    | _ -> Lwt.return (Ok None)
  in
  let* project_short_id_opt =
    match category with
    | Source -> Conn.find_opt get_project_short_id_query project_id
    | Experimental -> Lwt.return (Ok None)
  in
  let* owner, (scope, scope_id), final_strain_id, final_community_id =
    Lwt.return
      (resolve_sample_context ~project_id ~project_short_id_opt ~parent_opt
         ~sample_type ~category ~parent_sample_id ~strain_id ~community_id)
  in

  let* seq = Short_id_counter.reserve (module Conn) ~scope ~scope_id ~count:1 in
  let short_id = Exlab_core.Sample.generate_short_id ~owner ~seq in

  insert_row
    (module Conn)
    ~short_id ~project_id ~sample_type ~category ~parent_sample_id
    ~strain_id:final_strain_id ~community_id:final_community_id
    ~result_definition_ids

(** [add ~project_id ~sample_type ~category ~parent_sample_id ~strain_id
     ~community_id ?result_definition_ids ()] creates a new sample. *)
let add ~project_id ~sample_type ~category ~parent_sample_id ~strain_id
    ~community_id ?(result_definition_ids = []) () =
  let validation_result =
    Exlab_core.Sample.validate_creation ~sample_type ~category ~parent_sample_id
      ~strain_id ~community_id
  in
  match validation_result with
  | Error err -> Lwt.return (Error (`Msg err))
  | Ok () ->
      (* Transaction *)
      Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
          insert
            (module Conn)
            ~project_id ~sample_type ~category ~parent_sample_id ~strain_id
            ~community_id ~result_definition_ids ())

(** [get_all ()] retrieves all samples. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [get_all_for_user user_id] retrieves all samples accessible to a user. *)
let get_all_for_user user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_for_user_query user_id in
      Lwt_result.return rows)

type create_sample = {
  sample_type : string;
  category : sample_category;
  parent_sample_id : int option;
  strain_id : int option;
  community_id : int option;
  result_definition_ids : int list;
}

(** [create_many ~project_id items] creates multiple samples in a single
    transaction. *)
let create_many ~project_id (items : create_sample list) =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let open Lwt_result.Syntax in
      (* 1. Prefetch project short_id if any Source samples exist *)
      let* project_short_id_opt =
        if List.exists (fun item -> item.category = Source) items then
          let* proj_opt = Conn.find_opt get_project_short_id_query project_id in
          match proj_opt with
          | Some s -> Lwt.return (Ok (Some s))
          | None -> Lwt.return (Error (`Not_Found "Project not found"))
        else Lwt.return (Ok None)
      in

      (* 2. Prefetch all distinct parent samples in one query *)
      let parent_ids =
        List.filter_map
          (fun item ->
            match item.category with
            | Experimental -> item.parent_sample_id
            | Source -> None)
          items
        |> List.sort_uniq Int.compare
      in
      let* parent_rows = get_many_by_ids_tx (module Conn) parent_ids in
      let parent_map = Hashtbl.create (List.length parent_rows) in
      List.iter
        (fun (p : Exlab_core.Types.sample) -> Hashtbl.replace parent_map p.id p)
        parent_rows;

      (* 3. Single ordered pass: check parent existence + pure validation *)
      let resolve_item item =
        let parent_res =
          match (item.category, item.parent_sample_id) with
          | Experimental, Some p_id -> (
              match Hashtbl.find_opt parent_map p_id with
              | Some p -> Ok (Some p)
              | None -> Error (`Not_Found "Parent sample not found"))
          | _ -> Ok None
        in
        match parent_res with
        | Error e -> Error e
        | Ok parent_opt -> (
            match
              resolve_sample_context ~project_id ~project_short_id_opt
                ~parent_opt ~sample_type:item.sample_type
                ~category:item.category ~parent_sample_id:item.parent_sample_id
                ~strain_id:item.strain_id ~community_id:item.community_id
            with
            | Error e -> Error e
            | Ok (owner, key, final_strain_id, final_community_id) ->
                Ok (item, owner, key, final_strain_id, final_community_id))
      in
      let rec validate_items acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest -> (
            match resolve_item item with
            | Error e -> Error e
            | Ok resolved -> validate_items (resolved :: acc) rest)
      in
      let* validated_items = Lwt.return (validate_items [] items) in

      (* 4. Group counts by (scope, scope_id) and reserve in sorted order *)
      let scope_counts = Hashtbl.create 8 in
      List.iter
        (fun (_, _, key, _, _) ->
          let prev =
            Option.value ~default:0 (Hashtbl.find_opt scope_counts key)
          in
          Hashtbl.replace scope_counts key (prev + 1))
        validated_items;

      let sorted_scopes =
        Hashtbl.to_seq scope_counts
        |> List.of_seq
        |> List.sort (fun (k1, _) (k2, _) -> Stdlib.compare k1 k2)
      in

      let scope_next_seq = Hashtbl.create (List.length sorted_scopes) in
      let* () =
        Lwt_list.fold_left_s
          (fun acc ((scope, scope_id), count) ->
            match acc with
            | Error _ as e -> Lwt.return e
            | Ok () ->
                let* first_seq =
                  Short_id_counter.reserve (module Conn) ~scope ~scope_id ~count
                in
                Hashtbl.replace scope_next_seq (scope, scope_id) first_seq;
                Lwt.return (Ok ()))
          (Ok ()) sorted_scopes
      in

      (* 5. Assign sequential seq numbers in submission order and insert *)
      let items_with_short_id =
        List.map
          (fun (item, owner, key, final_strain_id, final_community_id) ->
            let seq = Hashtbl.find scope_next_seq key in
            Hashtbl.replace scope_next_seq key (seq + 1);
            let short_id = Exlab_core.Sample.generate_short_id ~owner ~seq in
            (item, short_id, final_strain_id, final_community_id))
          validated_items
      in

      let* created_samples =
        Lwt_list.fold_left_s
          (fun acc (item, short_id, final_strain_id, final_community_id) ->
            match acc with
            | Error e -> Lwt.return (Error e)
            | Ok samples ->
                let* new_sample =
                  insert_row
                    (module Conn)
                    ~short_id ~project_id ~sample_type:item.sample_type
                    ~category:item.category
                    ~parent_sample_id:item.parent_sample_id
                    ~strain_id:final_strain_id ~community_id:final_community_id
                    ~result_definition_ids:item.result_definition_ids
                in
                Lwt.return (Ok (new_sample :: samples)))
          (Ok []) items_with_short_id
      in
      Lwt.return (Ok (List.rev created_samples)))
