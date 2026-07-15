(** * This module defines the API routes for managing Projects. * It provides
    endpoints for creating, retrieving, updating, and deleting * Projects,
    forming a complete CRUD interface. * * This module handles the HTTP layer,
    translating requests and responses * between the client and the underlying
    storage and core logic layers. It * uses helper functions from the [Utils]
    module for common tasks like * parameter parsing and response serialization.
    * * Key functionalities include: * - Listing all projects, with support for
    CSV export. * - Creating a new project. * - Retrieving a single project by
    its identifier. * - Updating an existing project. * - Archiving
    (soft-deleting) a project. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

(** Handles `GET /api/v1/projects`.

    Retrieves a list of all projects. This endpoint supports content negotiation
    via the `?format=csv` query parameter.

    - **Query**: ?format=csv If specified, returns the project list as a CSV
      file.

    @return
      If `format` is "csv", returns a CSV file attachment. Otherwise, returns a
      JSON response containing a list of all projects and a total count. *)
let get_all_projects_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    Auth.fetch_scoped_list ~user_id ~user_role
      ~get_all_function:Storage.Project.get_all
      ~get_all_user_function:Storage.Project.get_all_for_user
  in
  Api_utils.handle_response_with_export ~request ~filename:"projects.csv"
    ~csv_serializer:Core.Export_data.projects_to_csv
    ~json_serializer:(fun projects ->
      let response : Api_types.Project.list_response =
        Api_types.Project.{ data = projects; count = List.length projects }
      in
      Api_types.Project.yojson_of_list_response response)
    result

(** Handles `POST /api/v1/projects`.

    Creates a new project. The request body must contain the project's name and
    can include other optional details.

    - **Body**: (json) The project creation payload, defined by
      [Api_types.Project.create].

    @return
      A JSON response with the newly created project and a [201 Created] status.
*)
let create_project_handler request =
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.Project.create_of_yojson request
    in

    (* Fetch the metadata template and validate *)
    let* template_json_opt =
      Storage.Setting.get_by_key "project_metadata_template"
    in
    let template =
      match template_json_opt with
      | Some s -> (
          match s.value with
          | `List l ->
              List.filter_map
                (fun item ->
                  try Some (Core.Types.metadata_field_def_of_yojson item)
                  with _ -> None)
                l
          | _ -> [])
      | None -> []
    in

    let* () =
      Lwt.return
        (Core.Project.validate_metadata req.metadata template
        |> Result.map_error (fun e -> `Bad_Request e))
    in

    let status = Option.value ~default:Core.Types.Active req.status in

    Storage.Project.add ~name:req.name ~description:req.description ~status
      ~contact_name:req.contact_name ~owner:req.owner ?metadata:req.metadata
      ?short_id:req.short_id ()
  in

  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_project result

(** Handles `GET /api/v1/projects/:identifier`.

    Retrieves a specific project by its ID, UUID, or Short ID.

    @param identifier The ID, UUID, or Short ID of the project.
    @return A JSON response containing the detailed information of the project.
*)

let export_project_results_handler request =
  let result =
    let open Lwt_result.Syntax in
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in

    let* samples = Storage.Sample.get_by_project_id project.id in
    let* plates = Storage.Plate.get_by_project_id project.id in
    let* values = Storage.Result_value.get_by_project_id project.id in
    let* definitions = Storage.Result_definition.get_all () in
    let* strains = Storage.Strain.get_all () in

    let* wells =
      Lwt_list.map_p
        (fun (p : Core.Types.plate) -> Storage.Well.get_by_plate_id p.id)
        plates
      |> Lwt.map (fun results ->
          List.fold_left
            (fun acc -> function Ok ws -> ws @ acc | Error _ -> acc)
            [] results
          |> Result.ok)
    in
    Lwt.return (Ok (samples, plates, values, definitions, strains, wells))
  in
  match%lwt result with
  | Ok (samples, plates, values, definitions, strains, wells) ->
      let matrix_json =
        Core.Export_data.generate_longitudinal_matrix ~definitions ~samples
          ~plates ~wells ~strains values
      in
      let csv_content = Core.Export_data.matrix_json_to_csv matrix_json in
      let filename =
        Printf.sprintf "project_%s_results_long.csv"
          (Dream.param request "identifier")
      in
      Dream.respond
        ~headers:
          [
            ("Content-Type", "text/csv");
            ( "Content-Disposition",
              Printf.sprintf "attachment; filename=\"%s\"" filename );
          ]
        csv_content
  | Error err -> Api_utils.respond_with_error err

let get_project_dashboard_handler request =
  let result =
    let open Lwt_result.Syntax in
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in

    let* samples = Storage.Sample.get_by_project_id project.id in
    let* plates = Storage.Plate.get_by_project_id project.id in
    let* values = Storage.Result_value.get_by_project_id project.id in
    let* definitions = Storage.Result_definition.get_all () in
    let* strains = Storage.Strain.get_all () in

    let* wells =
      Lwt_list.map_p
        (fun (p : Core.Types.plate) -> Storage.Well.get_by_plate_id p.id)
        plates
      |> Lwt.map (fun results ->
          List.fold_left
            (fun acc -> function Ok ws -> ws @ acc | Error _ -> acc)
            [] results
          |> Result.ok)
    in

    let matrix_json =
      Core.Export_data.generate_longitudinal_matrix ~definitions ~samples
        ~plates ~wells ~strains values
    in
    Lwt.return (Ok matrix_json)
  in

  Api_utils.handle_response ~serializer:(fun x -> x) result

let get_project_handler request =
  let identifier = Dream.param request "identifier" in

  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    Lwt.return (Ok project)
  in

  Api_utils.handle_response ~serializer:Core.Types.yojson_of_project result

(** Handles `PUT /api/v1/projects/:identifier`.

    Updates an existing project. The project is identified by its ID, UUID, or
    Short ID. The request body should contain the fields to be updated.

    @param identifier
      The ID, UUID, or Short ID of the project to update.
      - **Body**: (json) The project update payload, defined by
        [Api_types.Project.update].
    @return A JSON response with the updated project details. *)
let update_project_handler request =
  let identifier = Dream.param request "identifier" in

  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in

    let* req =
      Api_utils.parse_body_json Api_types.Project.update_of_yojson request
    in

    (* Fetch the metadata template and validate *)
    let* template_json_opt =
      Storage.Setting.get_by_key "project_metadata_template"
    in
    let template =
      match template_json_opt with
      | Some s -> (
          match s.value with
          | `List l ->
              List.filter_map
                (fun item ->
                  try Some (Core.Types.metadata_field_def_of_yojson item)
                  with _ -> None)
                l
          | _ -> [])
      | None -> []
    in

    let* () =
      Lwt.return
        (Core.Project.validate_metadata req.metadata template
        |> Result.map_error (fun e -> `Bad_Request e))
    in

    let status = Option.value req.status ~default:project.status in

    Storage.Project.update ~short_id:project.short_id ~name:req.name
      ~description:req.description ~status ~contact_name:req.contact_name
      ~owner:req.owner ?metadata:req.metadata ()
  in

  Api_utils.handle_response ~serializer:Core.Types.yojson_of_project result

(** Handles `DELETE /api/v1/projects/:identifier`.

    Archives a project, effectively performing a soft delete. The project is not
    permanently removed from the database but is marked as archived.

    @param identifier The ID, UUID, or Short ID of the project to archive.
    @return An empty response with a [204 No Content] status on success. *)
let delete_project_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    Storage.Project.delete project.short_id
  in
  Api_utils.handle_unit_result result

(** Handles `GET /api/v1/projects/:identifier/users`.

    Retrieves all users assigned to a project.

    @param identifier The ID, UUID, or Short ID of the project.
    @return A JSON response containing a list of assigned users. *)
let get_project_users_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* user_ids =
      Storage.Project_user.get_users_for_project ~project_id:project.id
    in
    let* users =
      Lwt_list.map_p (fun id -> Storage.User.get_by_id id) user_ids
      |> Lwt.map (fun results ->
          let users =
            List.filter_map
              (function Ok (Some u) -> Some u | _ -> None)
              results
          in
          Ok (List.map Api_types.User.to_safe_user users))
    in
    let response = { Api_types.User.data = users; count = List.length users } in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response ~serializer:Api_types.User.yojson_of_list_response
    result

(** Handles `POST /api/v1/projects/:identifier/users`.

    Assigns a user to a project. Requires Admin or Lab Manager role.

    @param identifier
      The ID, UUID, or Short ID of the project.
      - **Body**: (json) The user assignment payload, containing the user's
        email.
    @return An empty response with a [204 No Content] status on success. *)
let assign_project_user_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* project = Api_utils.find_project_by_identifier identifier in
    let* req =
      Api_utils.parse_body_json Api_types.Project.project_user_request_of_yojson
        request
    in
    let* user = Api_utils.find_user_by_identifier req.email in
    Storage.Project_user.assign ~project_id:project.id ~user_id:user.id
  in
  Api_utils.handle_unit_result result

(** Handles `DELETE /api/v1/projects/:identifier/users/:user_identifier`.

    Removes a user from a project. Requires Admin or Lab Manager role.

    @param identifier The ID, UUID, or Short ID of the project.
    @param user_identifier The ID, UUID, or email of the user to remove.
    @return An empty response with a [204 No Content] status on success. *)
let remove_project_user_handler request =
  let identifier = Dream.param request "identifier" in
  let user_identifier = Dream.param request "user_identifier" in
  let result =
    let* project = Api_utils.find_project_by_identifier identifier in
    let* user = Api_utils.find_user_by_identifier user_identifier in
    Storage.Project_user.remove ~project_id:project.id ~user_id:user.id
  in
  Api_utils.handle_unit_result result

(** Handles `POST /api/v1/projects/:identifier/plates/plan`.

    Generates a bulk experimental run across multiple plates. *)
let plan_plates_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in

    let* req_payload =
      Api_utils.parse_body_json Api_types.Plate.plate_plan_request_of_yojson
        request
    in

    (* Validate input length *)
    let* () =
      if req_payload.replicates <= 0 then
        Lwt_result.fail (`Bad_Request "Replicates must be >= 1")
      else Lwt_result.return ()
    in
    let* () =
      if List.length req_payload.source_sample_short_ids = 0 then
        Lwt_result.fail (`Bad_Request "Must provide source samples")
      else Lwt_result.return ()
    in

    (* Extract and run logic *)
    let* source_samples =
      Storage.Sample.get_many_by_short_ids req_payload.source_sample_short_ids
    in
    let* () =
      if
        List.length source_samples
        <> List.length req_payload.source_sample_short_ids
      then
        Lwt_result.fail
          (`Bad_Request "One or more source sample short IDs are invalid")
      else Lwt_result.return ()
    in

    let* plate_format =
      match Core.Plate.validate_format req_payload.plate_format with
      | Ok f -> Lwt_result.return f
      | Error msg -> Lwt_result.fail (`Bad_Request msg)
    in

    let all_wells = Core.Plate.generate_well_coordinates plate_format in
    let reserved_set =
      List.fold_left
        (fun acc w ->
          match Core.Plate.normalize_coordinate plate_format w with
          | Ok cw -> Api_utils.StringSet.add cw acc
          | _ -> acc)
        Api_utils.StringSet.empty req_payload.reserved_wells
    in
    let usable_wells =
      List.filter
        (fun w -> not (Api_utils.StringSet.mem w reserved_set))
        all_wells
    in
    let num_usable_wells = List.length usable_wells in

    let* () =
      if num_usable_wells = 0 then
        Lwt_result.fail
          (`Bad_Request "No usable wells left after reserving wells")
      else Lwt_result.return ()
    in

    let strategy_str = req_payload.strategy in
    let is_round_robin = strategy_str = "round_robin" in

    let total_plates =
      if is_round_robin then
        let total_samples = List.length source_samples * req_payload.replicates in
        max 1 ((total_samples + num_usable_wells - 1) / num_usable_wells)
      else
        max 1 req_payload.num_plates
    in

    let create_sample_payloads =
      if is_round_robin then
        List.concat_map
          (fun source_sample ->
            List.init req_payload.replicates (fun _ ->
                {
                  Storage.Sample.sample_type =
                    source_sample.Core.Types.sample_type;
                  category = Core.Types.Experimental;
                  parent_sample_id = Some source_sample.id;
                  strain_id = source_sample.strain_id;
                  community_id = source_sample.Core.Types.community_id;
                  result_definition_ids = [];
                }))
          source_samples
      else
        (* For shuffled, we need 'replicates' per plate, across 'total_plates' *)
        List.concat_map
          (fun source_sample ->
            List.init (req_payload.replicates * total_plates) (fun _ ->
                {
                  Storage.Sample.sample_type =
                    source_sample.Core.Types.sample_type;
                  category = Core.Types.Experimental;
                  parent_sample_id = Some source_sample.id;
                  strain_id = source_sample.strain_id;
                  community_id = source_sample.Core.Types.community_id;
                  result_definition_ids = [];
                }))
          source_samples
    in

    let* created_samples =
      Storage.Sample.create_many ~project_id:project.id create_sample_payloads
    in

    let* distribution_plan =
      if is_round_robin then
        match
          Core.Plate_planner.distribute_round_robin ~total_plates ~usable_wells
            ~items:created_samples
        with
        | Ok plan -> Lwt_result.return plan
        | Error e -> Lwt_result.fail (`Bad_Request e)
      else
        let strategy = 
          if strategy_str = "neighbor_aware" then Core.Well_shuffled.Neighbor_aware
          else Core.Well_shuffled.Simple
        in
        
        let partitioned_by_plate =
           List.init total_plates (fun plate_idx ->
             List.concat_map (fun (src : Core.Types.sample) ->
                let src_samples = List.filter (fun (s : Core.Types.sample) -> s.parent_sample_id = Some src.id) created_samples in
                let start_idx = plate_idx * req_payload.replicates in
                let slice = List.filteri (fun i _ -> i >= start_idx && i < start_idx + req_payload.replicates) src_samples in
                List.map (fun s -> (src.short_id, s)) slice
             ) source_samples
           )
        in
        
        let empty_fixed_maps = List.init total_plates (fun _ -> []) in
        
        match Core.Plate_planner.generate_shuffled_layouts ~strategy ~format:plate_format ~reserved_wells:req_payload.reserved_wells ~fixed_maps:empty_fixed_maps ~items_per_plate:partitioned_by_plate ~num_blanks:req_payload.num_blanks with
        | Ok layouts -> Lwt_result.return layouts
        | Error e -> Lwt_result.fail (`Bad_Request e)
    in

    let plates_to_create =
      List.mapi
        (fun idx layout ->
          let name =
            Printf.sprintf "%s - %d" req_payload.plate_name_prefix (idx + 1)
          in
          let layout_items =
            List.map
              (fun (well, (s : Core.Types.sample)) -> (well, s.short_id))
              layout
          in
          { Storage.Plate.name; layouts = layout_items })
        distribution_plan
    in

    let* new_plates =
      Storage.Plate.create_many_with_layouts ~project_id:project.id
        ~plate_format plates_to_create
    in

    Lwt_result.return new_plates
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:(fun plates ->
      `Assoc [ ("plates", `List (List.map Core.Types.yojson_of_plate plates)) ])
    result

let generate_transfer_map_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in

    let* req_payload =
      Api_utils.parse_body_json Api_types.Plate.transfer_map_request_of_yojson
        request
    in

    let* source_plate = Storage.Plate.get_by_id req_payload.source_plate_id in
    let* source_plate_opt =
      match source_plate with
      | Some p -> Lwt_result.return p
      | None -> Lwt_result.fail (`Not_Found "Source plate not found")
    in
    
    let* dest_plates =
      let rec fetch_plates ids acc =
        match ids with
        | [] -> Lwt_result.return (List.rev acc)
        | id :: rest ->
            let* p_opt = Storage.Plate.get_by_id id in
            match p_opt with
            | Some p -> fetch_plates rest (p :: acc)
            | None -> Lwt_result.fail (`Bad_Request "One or more destination plates not found")
      in
      fetch_plates req_payload.destination_plate_ids []
    in

    let* source_wells = Storage.Well.get_by_plate_id source_plate_opt.id in
    
    (* Filter for source wells that have samples *)
    let valid_source_wells =
      List.filter (fun w -> Option.is_some w.Core.Types.sample_id) source_wells
    in
    
    let* source_samples =
      let rec fetch_samples ids acc =
        match ids with
        | [] -> Lwt_result.return (List.rev acc)
        | id :: rest ->
            let* s_opt = Storage.Sample.get_by_id id in
            match s_opt with
            | Some s -> fetch_samples rest (s :: acc)
            | None -> fetch_samples rest acc
      in
      fetch_samples (List.filter_map (fun (w : Core.Types.well) -> w.sample_id) valid_source_wells) []
    in
    
    let dest_plate_ids = List.map (fun (p : Core.Types.plate) -> p.id) dest_plates in
    
    (* Get all wells for the destination plates *)
    let* all_dest_wells_lists =
      let rec fetch_wells ids acc =
        match ids with
        | [] -> Lwt_result.return (List.rev acc)
        | id :: rest ->
            let* wells = Storage.Well.get_by_plate_id id in
            fetch_wells rest (wells :: acc)
      in
      fetch_wells dest_plate_ids []
    in
    let all_dest_wells = List.flatten all_dest_wells_lists in
    
    let valid_dest_wells =
      List.filter (fun (w : Core.Types.well) -> Option.is_some w.sample_id) all_dest_wells
    in
    
    let* dest_samples =
      let rec fetch_samples ids acc =
        match ids with
        | [] -> Lwt_result.return (List.rev acc)
        | id :: rest ->
            let* s_opt = Storage.Sample.get_by_id id in
            match s_opt with
            | Some s -> fetch_samples rest (s :: acc)
            | None -> fetch_samples rest acc
      in
      fetch_samples (List.filter_map (fun (w : Core.Types.well) -> w.sample_id) valid_dest_wells) []
    in
    
    (* Build the transfer map rows *)
    let transfer_map_rows =
      List.filter_map (fun (src_well : Core.Types.well) ->
        let src_sample_id = Option.get src_well.sample_id in
        let src_sample = List.find_opt (fun (s : Core.Types.sample) -> s.id = src_sample_id) source_samples in
        
        match src_sample with
        | None -> None
        | Some (src_s : Core.Types.sample) ->
            let root_source_id =
              match src_s.category with
              | Core.Types.Source -> Some src_s.id
              | Core.Types.Experimental -> src_s.parent_sample_id
            in
            
            match root_source_id with
            | None -> None
            | Some root_id ->
                (* Find all derived samples in the destination wells *)
                let derived_dest_samples =
                  List.filter (fun (ds : Core.Types.sample) ->
                    ds.parent_sample_id = Some root_id ||
                    ds.id = root_id (* If it's literally the exact same sample being transferred *)
                  ) dest_samples
                in
                
                let derived_rows =
                  List.filter_map (fun (ds : Core.Types.sample) ->
                    let dest_well_opt = List.find_opt (fun (dw : Core.Types.well) -> dw.sample_id = Some ds.id) valid_dest_wells in
                    match dest_well_opt with
                    | None -> None
                    | Some (dest_w : Core.Types.well) ->
                        let dest_p = List.find (fun (p : Core.Types.plate) -> p.id = dest_w.plate_id) dest_plates in
                        Some Core.Export_data.{
                          source_sample_short_id = src_s.short_id;
                          source_plate_name = source_plate_opt.name;
                          source_well = src_well.coordinate;
                          dest_plate_name = dest_p.name;
                          dest_well = dest_w.coordinate;
                          dest_sample_short_id = ds.short_id;
                        }
                  ) derived_dest_samples
                in
                
                if derived_rows = [] then None else Some derived_rows
      ) valid_source_wells
      |> List.flatten
    in

    Lwt_result.return transfer_map_rows
  in
  Api_utils.handle_response_with_export ~request ~filename:"transfer_map.csv"
    ~csv_serializer:Core.Export_data.generate_transfer_map_csv
    ~json_serializer:(fun _rows -> `Assoc [("message", `String "Success")])
    result

let routes =
  [
    Dream.get "/api/v1/projects" get_all_projects_handler;
    Dream.post "/api/v1/projects" create_project_handler;
    Dream.get "/api/v1/projects/:identifier" get_project_handler;
    Dream.put "/api/v1/projects/:identifier" update_project_handler;
    Dream.delete "/api/v1/projects/:identifier" delete_project_handler;
    Dream.get "/api/v1/projects/:identifier/users" get_project_users_handler;
    Dream.post "/api/v1/projects/:identifier/users"
      (Auth.role_required
         [ Core.Types.Admin; Core.Types.Lab_manager ]
         assign_project_user_handler);
    Dream.post "/api/v1/projects/:identifier/plates/plan" plan_plates_handler;
    Dream.post "/api/v1/projects/:identifier/plates/transfer-map" generate_transfer_map_handler;
    Dream.delete "/api/v1/projects/:identifier/users/:user_identifier"
      (Auth.role_required
         [ Core.Types.Admin; Core.Types.Lab_manager ]
         remove_project_user_handler);
    Dream.get "/api/v1/projects/:identifier/dashboard"
      get_project_dashboard_handler;
    Dream.get "/api/v1/projects/:identifier/results/export"
      export_project_results_handler;
  ]
