(** API routes for vendor-neutral Analysis Runs, Plate Media, and Admin Storage
    Connections. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

let default_expires_in = 900
let storage_connections_key = "storage_connections"

let resolve_master_secret () : string option =
  match Sys.getenv_opt "EXLAB_ENCRYPTION_KEY" with
  | Some k when String.trim k <> "" -> Some (String.trim k)
  | _ -> (
      match Sys.getenv_opt "DREAM_SECRET" with
      | Some s when String.trim s <> "" && String.trim s <> "null" -> None
      | _ -> Some "exlab-local-dev-default-master-key")

let generate_random_nonce_12 () =
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic 12)

let read_file_string path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        Some (really_input_string ic len))
  with _ -> None

let load_env_gcs_credentials () : Core.Object_storage.gcs_credentials option =
  let res =
    match Sys.getenv_opt "GCS_SIGNING_KEY_JSON" with
    | Some json when String.trim json <> "" ->
        Core.Object_storage.gcs_credentials_of_json_string json
    | _ -> (
        match Sys.getenv_opt "GCS_SIGNING_KEY_BASE64" with
        | Some b64 when String.trim b64 <> "" ->
            Core.Object_storage.gcs_credentials_of_base64 b64
        | _ -> (
            let file_path_opt =
              match Sys.getenv_opt "GCS_SIGNING_KEY_FILE" with
              | Some p when String.trim p <> "" -> Some (String.trim p)
              | _ -> (
                  match Sys.getenv_opt "HOME" with
                  | Some home ->
                      let default_path =
                        Filename.concat home
                          ".config/exlab/gcs-signing-key.json"
                      in
                      if Sys.file_exists default_path then Some default_path
                      else None
                  | None -> None)
            in
            match file_path_opt with
            | Some path -> (
                match read_file_string path with
                | Some content ->
                    Core.Object_storage.gcs_credentials_of_json_string content
                | None -> Error "unreadable")
            | None -> Error "unconfigured"))
  in
  match res with Ok creds -> Some creds | Error _ -> None

let open_stored_connections raw_json_opt =
  match resolve_master_secret () with
  | Some master_secret ->
      Core.Object_storage.open_stored_json ~master_secret raw_json_opt
  | None -> None

let load_storage_config () =
  let env_gcs = load_env_gcs_credentials () in
  let* setting_opt = Storage.Setting.get_by_key storage_connections_key in
  let raw_json_opt = Option.map (fun s -> s.Core.Types.value) setting_opt in
  let unsealed_opt = open_stored_connections raw_json_opt in
  Lwt_result.return
    (Core.Object_storage.config_of_stored_json ~env_gcs unsealed_opt)

(** {2 Admin Storage Connections Routes} *)

let get_storage_connections_handler _request =
  let result =
    let env_gcs_email =
      Option.map
        (fun (c : Core.Object_storage.gcs_credentials) -> c.client_email)
        (load_env_gcs_credentials ())
    in
    let* setting_opt = Storage.Setting.get_by_key storage_connections_key in
    let raw_json_opt = Option.map (fun s -> s.Core.Types.value) setting_opt in
    let unsealed_opt = open_stored_connections raw_json_opt in
    Lwt_result.return
      (Core.Object_storage.masked_summary_of_stored_json ~env_gcs_email
         unsealed_opt)
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_json result

let update_storage_connections_handler request =
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.StorageConnections.update_of_yojson
        request
    in
    let* master_secret =
      match resolve_master_secret () with
      | Some secret -> Lwt_result.return secret
      | None ->
          Lwt_result.fail
            (`Bad_Request
               "Server is missing EXLAB_ENCRYPTION_KEY environment variable \
                required to encrypt storage credentials at rest")
    in
    let* existing_opt = Storage.Setting.get_by_key storage_connections_key in
    let existing_raw_opt =
      Option.map (fun s -> s.Core.Types.value) existing_opt
    in
    let existing_fields =
      match
        Core.Object_storage.open_stored_json ~master_secret existing_raw_opt
      with
      | Some (`Assoc fs) -> fs
      | _ -> []
    in

    let existing_gcs_disabled =
      match List.assoc_opt "gcs_disabled" existing_fields with
      | Some (`Bool true) -> true
      | _ -> false
    in

    let* next_gcs_val, next_gcs_disabled =
      if req.clear_gcs then Lwt_result.return (`Null, true)
      else
        match req.gcs_service_account_json with
        | Some raw when String.trim raw <> "" -> (
            match Core.Object_storage.gcs_credentials_of_json_string raw with
            | Ok _ -> Lwt_result.return (`String raw, false)
            | Error msg -> Lwt_result.fail (`Bad_Request msg))
        | _ ->
            Lwt_result.return
              ( Option.value ~default:`Null
                  (List.assoc_opt "gcs_service_account_json" existing_fields),
                existing_gcs_disabled )
    in

    let* next_s3_val =
      if req.clear_s3 then Lwt_result.return `Null
      else
        match req.s3 with
        | Some s3_req -> (
            let s3_json =
              `Assoc
                [
                  ( "endpoint_url",
                    `String
                      (Option.value ~default:"https://s3.amazonaws.com"
                         s3_req.endpoint_url) );
                  ( "region",
                    `String (Option.value ~default:"us-east-1" s3_req.region) );
                  ("access_key_id", `String s3_req.access_key_id);
                  ("secret_access_key", `String s3_req.secret_access_key);
                ]
            in
            match Core.Object_storage.parse_s3_json s3_json with
            | Ok _ -> Lwt_result.return s3_json
            | Error msg -> Lwt_result.fail (`Bad_Request msg))
        | None ->
            Lwt_result.return
              (Option.value ~default:`Null
                 (List.assoc_opt "s3" existing_fields))
    in

    let plain_json =
      `Assoc
        [
          ("gcs_service_account_json", next_gcs_val);
          ("gcs_disabled", `Bool next_gcs_disabled);
          ("s3", next_s3_val);
        ]
    in
    let nonce = generate_random_nonce_12 () in
    let sealed_json =
      Core.Object_storage.seal_stored_json ~master_secret ~nonce plain_json
    in
    let* _ =
      Storage.Setting.upsert ~key:storage_connections_key ~value:sealed_json
        ~description:
          "External object storage signing credentials (AES-256-GCM encrypted)"
        ()
    in
    let env_gcs_email =
      Option.map
        (fun (c : Core.Object_storage.gcs_credentials) -> c.client_email)
        (load_env_gcs_credentials ())
    in
    Lwt_result.return
      (Core.Object_storage.masked_summary_of_stored_json ~env_gcs_email
         (Some plain_json))
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_json result

(** {2 Analysis Run Routes} *)

let create_analysis_run_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* req =
      Api_utils.parse_body_json Api_types.AnalysisRun.create_of_yojson request
    in
    let raw_tool_name = Option.value ~default:"Custom" req.tool_name in
    let* tool_name, (storage_base_uri, dataset_checksum) =
      match
        Core.Analysis_run.validate_create ~tool_name:raw_tool_name
          ~storage_base_uri:req.storage_base_uri
          ~dataset_checksum:req.dataset_checksum
      with
      | Ok triple -> Lwt_result.return triple
      | Error msg -> Lwt_result.fail (`Bad_Request msg)
    in
    let artifacts = Option.value ~default:(`Assoc []) req.artifacts in
    Storage.Analysis_run.add ~project_id:project.id ~tool_name
      ?tool_version:req.tool_version ~storage_base_uri ~dataset_checksum
      ?pipeline_name:req.pipeline_name ?pipeline_config:req.pipeline_config
      ~artifacts ?summary_metrics:req.summary_metrics
      ?plate_media:req.plate_media ?notes:req.notes ~created_by:user_id ()
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_analysis_run result

let fetch_optional_object_text ~config ~uri =
  match
    Core.Object_storage.resolve_download_url ~config ~uri ~now:(Unix.time ())
      ~expires_in:300
  with
  | Error _ -> Lwt.return None
  | Ok signed_url -> (
      match%lwt Http_client.get_string signed_url with
      | Ok (200, body) -> Lwt.return (Some body)
      | _ -> Lwt.return None)

let list_all_bucket_objects ~config ~folder_uri =
  let rec loop page_num token_opt acc =
    if page_num > 30 then Lwt.return (Ok (List.rev acc))
    else
      match
        Core.Object_storage.sign_list_objects_url ~config ~folder_uri
          ?continuation_token:token_opt ~now:(Unix.time ()) ~expires_in:300 ()
      with
      | Error msg -> Lwt.return (Error (`Bad_Request msg))
      | Ok list_url -> (
          match%lwt Http_client.get_string list_url with
          | Error msg -> Lwt.return (Error (`Bad_Request msg))
          | Ok (status, _body) when status <> 200 ->
              Lwt.return
                (Error
                   (`Bad_Request
                      (Printf.sprintf
                         "Bucket listing returned HTTP %d (verify folder URI \
                          and storage credentials)"
                         status)))
          | Ok (200, xml_body) -> (
              let page = Core.Object_storage.parse_list_bucket_xml xml_body in
              let next_acc = List.rev_append page.objects acc in
              match page.next_continuation_token with
              | Some tok when tok <> "" ->
                  loop (page_num + 1) (Some tok) next_acc
              | _ -> Lwt.return (Ok (List.rev next_acc)))
          | Ok (code, _) ->
              Lwt.return
                (Error
                   (`Bad_Request
                      (Printf.sprintf "Unexpected bucket status %d" code))))
  in
  loop 1 None []

let sync_analysis_run_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* req =
      Api_utils.parse_body_json Api_types.AnalysisRun.sync_request_of_yojson
        request
    in
    let base_uri =
      Core.Analysis_run.strip_trailing_slashes
        (String.trim req.storage_base_uri)
    in
    let* () =
      if base_uri = "" then
        Lwt_result.fail (`Bad_Request "storage_base_uri cannot be empty")
      else Lwt_result.return ()
    in
    let* config = load_storage_config () in
    let* all_objects = list_all_bucket_objects ~config ~folder_uri:base_uri in
    let* () =
      if all_objects = [] then
        Lwt_result.fail
          (`Not_Found ("No files found in bucket folder: " ^ base_uri))
      else Lwt_result.return ()
    in

    (* Optionally read pipeline config & metadata.csv if present in folder *)
    let has_suffix suffix =
      List.find_opt
        (fun (o : Core.Object_storage.listed_object) ->
          String.ends_with ~suffix o.key && o.size > 0 && o.size <= 60_000_000)
        all_objects
    in
    let%lwt pipeline_config =
      let pipe_obj_opt =
        match has_suffix "/pipeline.json.pht-pipe" with
        | Some o -> Some (base_uri ^ "/pipeline.json.pht-pipe", o)
        | None -> (
            match has_suffix "/pipeline.json" with
            | Some o -> Some (base_uri ^ "/pipeline.json", o)
            | None -> None)
      in
      match pipe_obj_opt with
      | Some (uri, o) when o.size <= 2_000_000 -> (
          match%lwt fetch_optional_object_text ~config ~uri with
          | Some txt -> (
              try Lwt.return (Some (Yojson.Safe.from_string txt))
              with _ -> Lwt.return None)
          | None -> Lwt.return None)
      | _ -> Lwt.return None
    in
    let has_overlays =
      List.exists
        (fun (o : Core.Object_storage.listed_object) ->
          Re.execp (Re.compile (Re.str "/overlays/")) o.key)
        all_objects
    in
    let%lwt metadata_csv =
      if not has_overlays then Lwt.return None
      else
        match has_suffix "/metadata.csv" with
        | Some _ ->
            fetch_optional_object_text ~config ~uri:(base_uri ^ "/metadata.csv")
        | None -> Lwt.return None
    in

    let manifest =
      Core.Analysis_run.build_manifest_from_objects ~folder_uri:base_uri
        ?tool_name:req.tool_name ?pipeline_config ?metadata_csv all_objects
    in
    let* () =
      match manifest.artifacts with
      | `Assoc [] ->
          Lwt_result.fail
            (`Not_Found "No deliverable files found in bucket folder")
      | _ -> Lwt_result.return ()
    in

    Storage.Analysis_run.add ~project_id:project.id
      ~tool_name:manifest.tool_name ?tool_version:manifest.tool_version
      ~storage_base_uri:manifest.storage_base_uri
      ~dataset_checksum:manifest.dataset_checksum
      ?pipeline_name:manifest.pipeline_name
      ?pipeline_config:manifest.pipeline_config ~artifacts:manifest.artifacts
      ?summary_metrics:manifest.summary_metrics
      ?plate_media:manifest.plate_media ?notes:req.notes ~created_by:user_id ()
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_analysis_run result

let get_canonical_analysis_run_handler request =
  let tool = Dream.query request "tool" in
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* run_opt =
      Storage.Analysis_run.get_canonical_by_project_id ?tool project.id
    in
    match run_opt with
    | Some run -> Lwt_result.return run
    | None ->
        Lwt_result.fail
          (`Not_Found "No canonical analysis run found for project")
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_analysis_run result

let get_project_analysis_runs_handler request =
  let tool = Dream.query request "tool" in
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* runs = Storage.Analysis_run.get_by_project_id ?tool project.id in
    let response : Api_types.AnalysisRun.list_response =
      { data = runs; count = List.length runs }
    in
    Lwt_result.return response
  in
  Api_utils.handle_response
    ~serializer:Api_types.AnalysisRun.yojson_of_list_response result

let get_project_analysis_run_by_version_handler request =
  let tool = Dream.query request "tool" in
  let result =
    let identifier = Dream.param request "identifier" in
    let version_str = Dream.param request "version" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* version_number =
      match int_of_string_opt version_str with
      | Some v when v >= 1 -> Lwt_result.return v
      | _ -> Lwt_result.fail (`Bad_Request "Invalid version number")
    in
    let* run_opt =
      Storage.Analysis_run.get_by_project_and_version ?tool
        ~project_id:project.id ~version_number ()
    in
    match run_opt with
    | Some run -> Lwt_result.return run
    | None -> Lwt_result.fail (`Not_Found "Analysis run version not found")
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_analysis_run result

let set_canonical_analysis_run_handler request =
  let tool = Dream.query request "tool" in
  let result =
    let identifier = Dream.param request "identifier" in
    let version_str = Dream.param request "version" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* version_number =
      match int_of_string_opt version_str with
      | Some v when v >= 1 -> Lwt_result.return v
      | _ -> Lwt_result.fail (`Bad_Request "Invalid version number")
    in
    let* run_opt =
      Storage.Analysis_run.set_canonical ?tool ~project_id:project.id
        ~version_number ()
    in
    match run_opt with
    | Some run -> Lwt_result.return run
    | None -> Lwt_result.fail (`Not_Found "Analysis run version not found")
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_analysis_run result

let can_delete_analysis_run ~user_id ~user_role (run : Core.Types.analysis_run)
    =
  match user_role with
  | Core.Types.Admin | Core.Types.Lab_manager | Core.Types.Project_manager ->
      true
  | Core.Types.Project_user -> run.created_by = Some user_id

let delete_analysis_run_handler request =
  let tool = Dream.query request "tool" in
  let result =
    let identifier = Dream.param request "identifier" in
    let version_str = Dream.param request "version" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* version_number =
      match int_of_string_opt version_str with
      | Some v when v >= 1 -> Lwt_result.return v
      | _ -> Lwt_result.fail (`Bad_Request "Invalid version number")
    in
    let* target_opt =
      Storage.Analysis_run.get_by_project_and_version ?tool
        ~project_id:project.id ~version_number ()
    in
    match target_opt with
    | None -> Lwt_result.fail (`Not_Found "Analysis run version not found")
    | Some target ->
        if not (can_delete_analysis_run ~user_id ~user_role target) then
          Lwt_result.fail
            (`Forbidden
               "Only managers, admins, or the run creator can delete an \
                analysis run")
        else
          let* deleted =
            Storage.Analysis_run.delete ~tool:target.tool_name
              ~project_id:project.id ~version_number:target.version_number ()
          in
          if deleted then Lwt_result.return ()
          else Lwt_result.fail (`Not_Found "Analysis run version not found")
  in
  Api_utils.handle_unit_result result

let get_all_analysis_runs_handler request =
  let canonical_only =
    match Dream.query request "canonical_only" with
    | Some "true" | Some "1" -> true
    | _ -> false
  in
  let tool = Dream.query request "tool" in
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* runs =
      Auth.fetch_scoped_list ~user_id ~user_role
        ~get_all_function:(Storage.Analysis_run.get_all ~canonical_only ?tool)
        ~get_all_user_function:
          (Storage.Analysis_run.get_all_for_user ~canonical_only ?tool)
    in
    let response : Api_types.AnalysisRun.list_response =
      { data = runs; count = List.length runs }
    in
    Lwt_result.return response
  in
  Api_utils.handle_response
    ~serializer:Api_types.AnalysisRun.yojson_of_list_response result

let build_signed_url_for_run ~(project : Core.Types.project)
    ~(run : Core.Types.analysis_run) ~artifact_key =
  match Core.Analysis_run.extract_artifact_uri ~artifact_key run.artifacts with
  | None ->
      Lwt_result.fail
        (`Not_Found
           (Printf.sprintf "Artifact '%s' not found in analysis run v%d"
              artifact_key run.version_number))
  | Some uri -> (
      let* config = load_storage_config () in
      let now = Unix.time () in
      match
        Core.Object_storage.resolve_download_url ~config ~uri ~now
          ~expires_in:default_expires_in
      with
      | Error msg -> Lwt_result.fail (`Bad_Request msg)
      | Ok signed_url ->
          let response : Api_types.AnalysisRun.signed_url_response =
            {
              project_short_id = project.short_id;
              version_number = run.version_number;
              artifact = artifact_key;
              uri;
              signed_url;
              expires_in = default_expires_in;
            }
          in
          Lwt_result.return response)

let respond_signed_url ~request result =
  let should_redirect =
    match Dream.query request "redirect" with
    | Some "true" | Some "1" -> true
    | _ -> false
  in
  match%lwt result with
  | Ok (res : Api_types.AnalysisRun.signed_url_response) when should_redirect ->
      Dream.respond ~status:`Found ~headers:[ ("Location", res.signed_url) ] ""
  | Ok res ->
      Api_utils.handle_response
        ~serializer:Api_types.AnalysisRun.yojson_of_signed_url_response
        (Lwt.return (Ok res))
  | Error err -> Api_utils.respond_with_error err

let get_canonical_signed_url_handler request =
  let tool = Dream.query request "tool" in
  let result =
    let identifier = Dream.param request "identifier" in
    let* artifact_key =
      match Dream.query request "artifact" with
      | Some k when String.trim k <> "" -> Lwt_result.return (String.trim k)
      | _ ->
          Lwt_result.fail
            (`Bad_Request "Missing required 'artifact' query parameter")
    in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* run_opt =
      Storage.Analysis_run.get_canonical_by_project_id ?tool project.id
    in
    match run_opt with
    | None ->
        Lwt_result.fail
          (`Not_Found "No canonical analysis run found for project")
    | Some run -> build_signed_url_for_run ~project ~run ~artifact_key
  in
  respond_signed_url ~request result

let get_version_signed_url_handler request =
  let tool = Dream.query request "tool" in
  let result =
    let identifier = Dream.param request "identifier" in
    let version_str = Dream.param request "version" in
    let* artifact_key =
      match Dream.query request "artifact" with
      | Some k when String.trim k <> "" -> Lwt_result.return (String.trim k)
      | _ ->
          Lwt_result.fail
            (`Bad_Request "Missing required 'artifact' query parameter")
    in
    let* version_number =
      match int_of_string_opt version_str with
      | Some v when v >= 1 -> Lwt_result.return v
      | _ -> Lwt_result.fail (`Bad_Request "Invalid version number")
    in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* run_opt =
      Storage.Analysis_run.get_by_project_and_version ?tool
        ~project_id:project.id ~version_number ()
    in
    match run_opt with
    | None -> Lwt_result.fail (`Not_Found "Analysis run version not found")
    | Some run -> build_signed_url_for_run ~project ~run ~artifact_key
  in
  respond_signed_url ~request result

let get_plate_media_handler request =
  let include_signed_urls =
    match Dream.query request "include_signed_urls" with
    | Some "true" | Some "1" -> true
    | _ -> false
  in
  let tool = Dream.query request "tool" in
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in
    let* run_opt =
      match tool with
      | Some _ ->
          Storage.Analysis_run.get_canonical_by_project_id ?tool
            plate.project_id
      | None -> (
          let* canonical_runs =
            Storage.Analysis_run.get_canonical_runs_by_project_id
              plate.project_id
          in
          let with_plate_media =
            List.find_opt
              (fun (r : Core.Types.analysis_run) ->
                Core.Analysis_run.extract_plate_media
                  ~plate_short_id:plate.short_id r.plate_media
                <> [])
              canonical_runs
          in
          match with_plate_media with
          | Some r -> Lwt_result.return (Some r)
          | None -> Lwt_result.return (List.nth_opt canonical_runs 0))
    in
    match run_opt with
    | None ->
        Lwt_result.fail
          (`Not_Found "No canonical analysis run found for project")
    | Some run ->
        let raw_media =
          Core.Analysis_run.extract_plate_media ~plate_short_id:plate.short_id
            run.plate_media
        in
        let* media =
          if not include_signed_urls then Lwt_result.return raw_media
          else
            let* config = load_storage_config () in
            Lwt_result.return
              (Core.Analysis_run.sign_media_points ~config ~now:(Unix.time ())
                 ~expires_in:default_expires_in raw_media)
        in
        let response : Api_types.AnalysisRun.plate_media_response =
          {
            plate_short_id = plate.short_id;
            tool_name = run.tool_name;
            version_number = run.version_number;
            dataset_checksum = run.dataset_checksum;
            media;
          }
        in
        Lwt_result.return response
  in
  Api_utils.handle_response
    ~serializer:Api_types.AnalysisRun.yojson_of_plate_media_response result

let routes =
  [
    Dream.get "/api/v1/admin/storage-connections"
      (Auth.role_required
         [ Core.Types.Admin; Core.Types.Lab_manager ]
         get_storage_connections_handler);
    Dream.put "/api/v1/admin/storage-connections"
      (Auth.role_required
         [ Core.Types.Admin; Core.Types.Lab_manager ]
         update_storage_connections_handler);
    Dream.get "/api/v1/analysis-runs" get_all_analysis_runs_handler;
    Dream.post "/api/v1/projects/:identifier/analysis-runs"
      create_analysis_run_handler;
    Dream.post "/api/v1/projects/:identifier/analysis-runs/sync"
      sync_analysis_run_handler;
    Dream.get "/api/v1/projects/:identifier/analysis-runs"
      get_project_analysis_runs_handler;
    Dream.get "/api/v1/projects/:identifier/analysis-runs/canonical"
      get_canonical_analysis_run_handler;
    Dream.get "/api/v1/projects/:identifier/analysis-runs/canonical/signed-url"
      get_canonical_signed_url_handler;
    Dream.get "/api/v1/projects/:identifier/analysis-runs/:version"
      get_project_analysis_run_by_version_handler;
    Dream.delete "/api/v1/projects/:identifier/analysis-runs/:version"
      delete_analysis_run_handler;
    Dream.get "/api/v1/projects/:identifier/analysis-runs/:version/signed-url"
      get_version_signed_url_handler;
    Dream.put "/api/v1/projects/:identifier/analysis-runs/:version/canonical"
      set_canonical_analysis_run_handler;
    Dream.get "/api/v1/plates/:identifier/media" get_plate_media_handler;
  ]
