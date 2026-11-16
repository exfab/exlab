(** Handling for the vendor-neutral AnalysisRun data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

let select_fields =
  "id, uid, project_id, tool_name, tool_version, version_number, is_canonical, \
   storage_base_uri, dataset_checksum, pipeline_name, pipeline_config::text, \
   artifacts::text, summary_metrics::text, plate_media::text, notes, \
   created_by, created_at, updated_at"

let analysis_run_t =
  let encode (r : analysis_run) =
    let pipeline_config_str =
      Option.map Yojson.Safe.to_string r.pipeline_config
    in
    let artifacts_str = Yojson.Safe.to_string r.artifacts in
    let summary_metrics_str =
      Option.map Yojson.Safe.to_string r.summary_metrics
    in
    let plate_media_str = Option.map Yojson.Safe.to_string r.plate_media in
    Ok
      ( ( r.id,
          r.uid,
          r.project_id,
          r.tool_name,
          r.tool_version,
          r.version_number,
          r.is_canonical,
          r.storage_base_uri,
          r.dataset_checksum ),
        ( r.pipeline_name,
          pipeline_config_str,
          artifacts_str,
          summary_metrics_str,
          plate_media_str,
          r.notes,
          r.created_by,
          r.created_at,
          r.updated_at ) )
  in
  let decode
      ( ( id,
          uid,
          project_id,
          tool_name,
          tool_version,
          version_number,
          is_canonical,
          storage_base_uri,
          dataset_checksum ),
        ( pipeline_name,
          pipeline_config_str,
          artifacts_str,
          summary_metrics_str,
          plate_media_str,
          notes,
          created_by,
          created_at,
          updated_at ) ) =
    try
      let pipeline_config =
        Option.map Yojson.Safe.from_string pipeline_config_str
      in
      let artifacts = Yojson.Safe.from_string artifacts_str in
      let summary_metrics =
        Option.map Yojson.Safe.from_string summary_metrics_str
      in
      let plate_media = Option.map Yojson.Safe.from_string plate_media_str in
      Ok
        ({
           id;
           uid;
           project_id;
           tool_name;
           tool_version;
           version_number;
           is_canonical;
           storage_base_uri;
           dataset_checksum;
           pipeline_name;
           pipeline_config;
           artifacts;
           summary_metrics;
           plate_media;
           notes;
           created_by;
           created_at;
           updated_at;
         }
          : analysis_run)
    with _ -> Error "Failed to parse JSON columns in analysis_runs"
  in
  let rep =
    t2
      (t9 int string int string (option string) int bool string string)
      (t9 (option string) (option string) string (option string) (option string)
         (option string) (option int) float float)
  in
  Caqti_type.custom ~encode ~decode rep

let insert_query =
  let input_t =
    t2
      (t8 string int string (option string) int bool string string)
      (t7 (option string) (option string) string (option string) (option string)
         (option string) (option int))
  in
  (t2 input_t float ->! analysis_run_t)
    (Printf.sprintf
       "INSERT INTO analysis_runs (uid, project_id, tool_name, tool_version, \
        version_number, is_canonical, storage_base_uri, dataset_checksum, \
        pipeline_name, pipeline_config, artifacts, summary_metrics, \
        plate_media, notes, created_by, created_at) VALUES (?, ?, ?, ?, ?, ?, \
        ?, ?, ?, ?::jsonb, ?::jsonb, ?::jsonb, ?::jsonb, ?, ?, ?) RETURNING %s"
       select_fields)

let clear_canonical_for_project_tool_query =
  (t2 int string ->. unit)
    "UPDATE analysis_runs SET is_canonical = FALSE WHERE project_id = ? AND \
     lower(tool_name) = lower(?) AND is_canonical = TRUE"

let next_version_for_project_tool_query =
  (t2 int string ->! int)
    "SELECT COALESCE(MAX(version_number), 0) + 1 FROM analysis_runs WHERE \
     project_id = ? AND lower(tool_name) = lower(?)"

let get_canonical_by_project_id_query =
  (int ->? analysis_run_t)
    (Printf.sprintf
       "SELECT %s FROM analysis_runs WHERE project_id = ? AND is_canonical = \
        TRUE ORDER BY updated_at DESC, id DESC LIMIT 1"
       select_fields)

let get_canonical_by_project_and_tool_query =
  (t2 int string ->? analysis_run_t)
    (Printf.sprintf
       "SELECT %s FROM analysis_runs WHERE project_id = ? AND lower(tool_name) \
        = lower(?) AND is_canonical = TRUE LIMIT 1"
       select_fields)

let get_canonical_runs_by_project_id_query =
  (int ->* analysis_run_t)
    (Printf.sprintf
       "SELECT %s FROM analysis_runs WHERE project_id = ? AND is_canonical = \
        TRUE ORDER BY updated_at DESC, id DESC"
       select_fields)

let get_by_project_id_query =
  (int ->* analysis_run_t)
    (Printf.sprintf
       "SELECT %s FROM analysis_runs WHERE project_id = ? ORDER BY \
        version_number DESC, id DESC"
       select_fields)

let get_by_project_and_version_query =
  (t2 int int ->? analysis_run_t)
    (Printf.sprintf
       "SELECT %s FROM analysis_runs WHERE project_id = ? AND version_number = \
        ? ORDER BY updated_at DESC, id DESC LIMIT 1"
       select_fields)

let get_by_project_tool_and_version_query =
  (t3 int string int ->? analysis_run_t)
    (Printf.sprintf
       "SELECT %s FROM analysis_runs WHERE project_id = ? AND lower(tool_name) \
        = lower(?) AND version_number = ? LIMIT 1"
       select_fields)

let set_canonical_by_id_query =
  (int ->? analysis_run_t)
    (Printf.sprintf
       "UPDATE analysis_runs SET is_canonical = TRUE WHERE id = ? RETURNING %s"
       select_fields)

let get_all_query =
  (unit ->* analysis_run_t)
    (Printf.sprintf
       "SELECT %s FROM analysis_runs ORDER BY project_id ASC, tool_name ASC, \
        version_number DESC"
       select_fields)

let get_all_canonical_query =
  (unit ->* analysis_run_t)
    (Printf.sprintf
       "SELECT %s FROM analysis_runs WHERE is_canonical = TRUE ORDER BY \
        project_id ASC, tool_name ASC"
       select_fields)

let get_all_for_user_query =
  (int ->* analysis_run_t)
    "SELECT ar.id, ar.uid, ar.project_id, ar.tool_name, ar.tool_version, \
     ar.version_number, ar.is_canonical, ar.storage_base_uri, \
     ar.dataset_checksum, ar.pipeline_name, ar.pipeline_config::text, \
     ar.artifacts::text, ar.summary_metrics::text, ar.plate_media::text, \
     ar.notes, ar.created_by, ar.created_at, ar.updated_at FROM analysis_runs \
     ar INNER JOIN project_users pu ON ar.project_id = pu.project_id WHERE \
     pu.user_id = ? ORDER BY ar.project_id ASC, ar.tool_name ASC, \
     ar.version_number DESC"

let get_all_canonical_for_user_query =
  (int ->* analysis_run_t)
    "SELECT ar.id, ar.uid, ar.project_id, ar.tool_name, ar.tool_version, \
     ar.version_number, ar.is_canonical, ar.storage_base_uri, \
     ar.dataset_checksum, ar.pipeline_name, ar.pipeline_config::text, \
     ar.artifacts::text, ar.summary_metrics::text, ar.plate_media::text, \
     ar.notes, ar.created_by, ar.created_at, ar.updated_at FROM analysis_runs \
     ar INNER JOIN project_users pu ON ar.project_id = pu.project_id WHERE \
     pu.user_id = ? AND ar.is_canonical = TRUE ORDER BY ar.project_id ASC, \
     ar.tool_name ASC"

let filter_by_tool tool_opt runs =
  match tool_opt with
  | None -> runs
  | Some t when String.trim t = "" -> runs
  | Some t ->
      let target = String.lowercase_ascii (String.trim t) in
      List.filter
        (fun (r : analysis_run) -> String.lowercase_ascii r.tool_name = target)
        runs

let add ~project_id ~tool_name ?tool_version ~storage_base_uri ~dataset_checksum
    ?pipeline_name ?pipeline_config ?(artifacts = `Assoc []) ?summary_metrics
    ?plate_media ?notes ?created_by () =
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let pipeline_config_str = Option.map Yojson.Safe.to_string pipeline_config in
  let artifacts_str = Yojson.Safe.to_string artifacts in
  let summary_metrics_str = Option.map Yojson.Safe.to_string summary_metrics in
  let plate_media_str = Option.map Yojson.Safe.to_string plate_media in
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* () =
        Conn.exec clear_canonical_for_project_tool_query (project_id, tool_name)
      in
      let* next_version =
        Conn.find next_version_for_project_tool_query (project_id, tool_name)
      in
      Conn.find insert_query
        ( ( ( uid,
              project_id,
              tool_name,
              tool_version,
              next_version,
              true,
              storage_base_uri,
              dataset_checksum ),
            ( pipeline_name,
              pipeline_config_str,
              artifacts_str,
              summary_metrics_str,
              plate_media_str,
              notes,
              created_by ) ),
          created_at ))

let get_canonical_by_project_id ?tool project_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      match tool with
      | Some t when String.trim t <> "" ->
          Conn.find_opt get_canonical_by_project_and_tool_query
            (project_id, String.trim t)
      | _ -> Conn.find_opt get_canonical_by_project_id_query project_id)

let get_canonical_runs_by_project_id project_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.collect_list get_canonical_runs_by_project_id_query project_id)

let get_by_project_id ?tool project_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_project_id_query project_id in
      Lwt_result.return (filter_by_tool tool rows))

let get_by_project_and_version ?tool ~project_id ~version_number () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      match tool with
      | Some t when String.trim t <> "" ->
          Conn.find_opt get_by_project_tool_and_version_query
            (project_id, String.trim t, version_number)
      | _ ->
          Conn.find_opt get_by_project_and_version_query
            (project_id, version_number))

let set_canonical ?tool ~project_id ~version_number () =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* existing_opt =
        match tool with
        | Some t when String.trim t <> "" ->
            Conn.find_opt get_by_project_tool_and_version_query
              (project_id, String.trim t, version_number)
        | _ ->
            Conn.find_opt get_by_project_and_version_query
              (project_id, version_number)
      in
      match existing_opt with
      | None -> Lwt_result.return None
      | Some target ->
          let* () =
            Conn.exec clear_canonical_for_project_tool_query
              (project_id, target.tool_name)
          in
          Conn.find_opt set_canonical_by_id_query target.id)

let get_all ~canonical_only ?tool () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        if canonical_only then Conn.collect_list get_all_canonical_query ()
        else Conn.collect_list get_all_query ()
      in
      Lwt_result.return (filter_by_tool tool rows))

let get_all_for_user ~canonical_only ?tool user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        if canonical_only then
          Conn.collect_list get_all_canonical_for_user_query user_id
        else Conn.collect_list get_all_for_user_query user_id
      in
      Lwt_result.return (filter_by_tool tool rows))
