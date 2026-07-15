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

(* Count how many samples are currently in the given project *)
let get_sample_count_query =
  (int ->! int) "SELECT COUNT(*)::int FROM samples WHERE project_id = ?"

let get_source_sample_count_query =
  (int ->! int)
    "SELECT COUNT(*)::int FROM samples WHERE project_id = ? AND category = \
     'Source'"

let count_children_query =
  (int ->! int) "SELECT COUNT(*)::int FROM samples WHERE parent_sample_id = ?"

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

(** [insert conn ~project_id ~sample_type ~category ~parent_sample_id ~strain_id
     ~community_id ?result_definition_ids ()] inserts a new sample within a
    transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~project_id ~sample_type
    ~category ~parent_sample_id ~strain_id ~community_id
    ?(result_definition_ids = []) () =
  let open Lwt_result.Syntax in
  let* final_strain_id =
    match (category, strain_id, parent_sample_id) with
    | Experimental, None, Some p_id -> (
        let* parent_opt = Conn.find_opt get_by_id_query p_id in
        match parent_opt with
        | Some p -> Lwt.return (Ok p.strain_id)
        | None -> Lwt.return (Error (`Not_Found "Parent sample not found")))
    | _ -> Lwt.return (Ok strain_id)
  in
  let* final_community_id =
    match (category, community_id, parent_sample_id) with
    | Experimental, None, Some p_id -> (
        let* parent_opt = Conn.find_opt get_by_id_query p_id in
        match parent_opt with
        | Some p -> Lwt.return (Ok p.community_id)
        | None -> Lwt.return (Error (`Not_Found "Parent sample not found")))
    | _ -> Lwt.return (Ok community_id)
  in

  let* () =
    match
      Exlab_core.Sample.validate_creation ~sample_type ~category
        ~parent_sample_id ~strain_id:final_strain_id
        ~community_id:final_community_id
    with
    | Ok () -> Lwt.return (Ok ())
    | Error msg -> Lwt.return (Error (`Msg msg))
  in

  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let status = Exlab_core.Types.Active in
  let status_str = string_of_entity_status status in
  let category_str = string_of_sample_category category in

  let* short_id =
    match (category, parent_sample_id) with
    | Experimental, Some p_id -> (
        let* parent_opt = Conn.find_opt get_by_id_query p_id in
        match parent_opt with
        | Some p ->
            let* count = Conn.find count_children_query p_id in
            Lwt.return
              (Exlab_core.Sample.generate_short_id ~category
                 ~parent_short_id_opt:(Some p.short_id) ~project_prefix_opt:None
                 ~count
              |> Result.map_error (fun e -> `Msg e))
        | None -> Lwt.return (Error (`Not_Found "Parent sample not found")))
    | _ ->
        let* project_short_id_opt =
          Conn.find_opt get_project_short_id_query project_id
        in
        let* count = Conn.find get_source_sample_count_query project_id in
        Lwt.return
          (Exlab_core.Sample.generate_short_id ~category
             ~parent_short_id_opt:None ~project_prefix_opt:project_short_id_opt
             ~count
          |> Result.map_error (fun e -> `Msg e))
  in

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
        final_strain_id,
        final_community_id,
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
      let* created_samples =
        Lwt_list.fold_left_s
          (fun acc item ->
            match acc with
            | Error e -> Lwt.return (Error e) (* Stop on first error *)
            | Ok samples ->
                let* new_sample =
                  insert
                    (module Conn)
                    ~project_id ~sample_type:item.sample_type
                    ~category:item.category
                    ~parent_sample_id:item.parent_sample_id
                    ~strain_id:item.strain_id ~community_id:item.community_id
                    ~result_definition_ids:item.result_definition_ids ()
                in
                Lwt.return (Ok (new_sample :: samples)))
          (Ok []) items
      in
      Lwt.return (Ok (List.rev created_samples)))
