(** * This module defines the API routes for managing Samples. * It provides
    endpoints for creating, retrieving, updating, and deleting * Samples,
    forming a complete CRUD interface. * * This module handles the HTTP layer,
    translating requests and responses * between the client and the underlying
    storage and core logic layers. It * uses helper functions from the [Utils]
    module for common tasks like * parameter parsing and response serialization.
    * * Key functionalities include: * - Creating, retrieving, updating, and
    archiving (soft-deleting) samples. * - Bulk creation of multiple samples in
    a single request. * - Listing all samples or all samples for a specific
    project, with CSV * export support. * - Retrieving a detailed view of a
    sample, including its project and result * data. * - Finding all plate and
    well locations for a given sample. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

(** A helper function to transform the raw database result for sample locations
    into the API's DTO format.

    @param locations
      A list of tuples from the database, where each tuple contains plate ID,
      and well coordinate.
    @return A list of [Api_types.Sample.location] records. *)
let map_locations locations =
  List.map
    (fun (plate_id, _, _, well_coordinate) ->
      Api_types.Sample.{ plate_id; well_coordinate })
    locations

(** Handles `GET /api/v1/samples/:identifier`.

    Retrieves a detailed view of a specific sample by its ID, UUID, or Short ID.
    The response includes the sample itself, its parent project, and any
    associated result values.

    @param identifier The ID, UUID, or Short ID of the sample.
    @return A detailed JSON object for the sample. *)
let get_sample_detailed_handler request =
  let result =
    Auth.with_sample_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~sample ->
        let* project =
          let* project_opt = Storage.Project.get_by_id sample.project_id in
          match project_opt with
          | Some project -> Lwt.return (Ok project)
          | None ->
              Lwt.return
                (Error
                   (`Not_Found
                      ("Project not found for sample: "
                     ^ string_of_int sample.id)))
        in
        let* results = Storage.Result_value.get_by_sample_id sample.id in
        let response : Api_types.Sample.detailed =
          Api_types.Sample.{ sample; project; results }
        in
        Lwt.return (Ok response))
  in

  Api_utils.handle_response ~serializer:Api_types.Sample.yojson_of_detailed
    result

module StrainSet = Set.Make (struct
  type t = string * string * string * string option

  let compare = compare
end)

let create_samples_batch ~project_id ~dry_run ?default_category
    (items : Api_types.Sample.create list) =
  let open Lwt_result.Syntax in
  let%lwt _, storage_items_rev =
    Lwt_list.fold_left_s
      (fun (simulated_new_strains, acc) (item : Api_types.Sample.create) ->
        let category_str =
          match item.category with
          | Some c -> c
          | None -> Option.value ~default:"Experimental" default_category
        in
        match Core.Types.sample_category_of_string category_str with
        | Error msg ->
            Lwt.return (simulated_new_strains, Error (`Bad_Request msg) :: acc)
        | Ok category -> (
            let%lwt strain_id_res, strain_status, next_simulated_strains =
              match (item.genus, item.species, item.strain_name) with
              | Some genus, Some species, Some strain_name -> (
                  let key =
                    ( String.lowercase_ascii genus,
                      String.lowercase_ascii species,
                      String.lowercase_ascii strain_name,
                      Option.map String.lowercase_ascii item.genotype )
                  in
                  match%lwt
                    Storage.Strain.find_exact ~genus ~species ~strain_name
                      ~genotype:item.genotype
                  with
                  | Ok (Some (strain : Core.Types.strain)) ->
                      Lwt.return
                        (Ok (Some strain.id), `Linked, simulated_new_strains)
                  | Ok None -> (
                      if StrainSet.mem key simulated_new_strains then
                        Lwt.return (Ok (Some 0), `Linked, simulated_new_strains)
                      else if dry_run then
                        let next_strains =
                          StrainSet.add key simulated_new_strains
                        in
                        Lwt.return (Ok (Some 0), `Created, next_strains)
                      else
                        match%lwt
                          Storage.Strain.add ~genus ~species ~strain_name
                            ~genotype:item.genotype ~parent_strain_id:None
                            ~notes:None ()
                        with
                        | Ok (strain : Core.Types.strain) ->
                            let next_strains =
                              StrainSet.add key simulated_new_strains
                            in
                            Lwt.return
                              (Ok (Some strain.id), `Created, next_strains)
                        | Error e ->
                            Lwt.return (Error e, `None, simulated_new_strains))
                  | Error e -> Lwt.return (Error e, `None, simulated_new_strains)
                  )
              | _ -> Lwt.return (Ok item.strain_id, `None, simulated_new_strains)
            in

            let%lwt parent_sample_id_res =
              match item.parent_sample_short_id with
              | Some short_id -> (
                  match%lwt Storage.Sample.get_by_short_id short_id with
                  | Ok (Some parent) -> Lwt.return (Ok (Some parent.id))
                  | Ok None ->
                      Lwt.return
                        (Error
                           (`Not_Found
                              ("Parent sample short ID not found: " ^ short_id)))
                  | Error e -> Lwt.return (Error e))
              | None -> Lwt.return (Ok item.parent_sample_id)
            in

            match (strain_id_res, parent_sample_id_res) with
            | Error e, _ -> Lwt.return (next_simulated_strains, Error e :: acc)
            | _, Error e -> Lwt.return (next_simulated_strains, Error e :: acc)
            | Ok strain_id, Ok parent_sample_id ->
                let item_res =
                  Ok
                    ( {
                        Storage.Sample.sample_type = item.sample_type;
                        category;
                        parent_sample_id;
                        strain_id;
                        community_id = item.community_id;
                        result_definition_ids = item.result_definition_ids;
                      },
                      strain_status )
                in
                Lwt.return (next_simulated_strains, item_res :: acc)))
      (StrainSet.empty, []) items
  in
  let storage_items = List.rev storage_items_rev in
  let errors =
    List.filter_map (function Error e -> Some e | Ok _ -> None) storage_items
  in
  match errors with
  | e :: _ -> Lwt.return (Error e)
  | [] ->
      let items_with_status = List.filter_map Result.to_option storage_items in
      let items_to_create = List.map fst items_with_status in
      let linked_strains =
        List.fold_left
          (fun acc (_, status) -> if status = `Linked then acc + 1 else acc)
          0 items_with_status
      in
      let created_strains =
        List.fold_left
          (fun acc (_, status) -> if status = `Created then acc + 1 else acc)
          0 items_with_status
      in

      let* created_samples =
        if dry_run then Lwt.return (Ok [])
        else Storage.Sample.create_many ~project_id items_to_create
      in
      let summary : Api_types.Sample.bulk_create_summary =
        {
          created_samples =
            (if dry_run then List.length items_to_create
             else List.length created_samples);
          linked_strains;
          created_strains;
        }
      in
      Lwt.return (Ok (created_samples, summary))

(** Handles `GET /api/v1/projects/:identifier/samples`.

    Retrieves all samples associated with a specific project. This endpoint
    supports content negotiation for CSV export.

    @param identifier
      The ID, UUID, or Short ID of the project.
      - **Query**: ?format=csv If specified, returns the sample list as a CSV
        file.
    @return A JSON response with a list of samples and a count, or a CSV file.
*)
let get_project_samples_handler request =
  let result =
    Auth.with_project_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~project ->
        let search_term_opt = Dream.query request "search" in
        let category_filter = Dream.query request "category" in
        let type_filter = Dream.query request "sample_type" in
        let strain_filter = Dream.query request "strain_id" in

        let has_filters =
          Option.is_some search_term_opt
          || Option.is_some category_filter
          || Option.is_some type_filter
          || Option.is_some strain_filter
        in

        let* samples =
          if has_filters then
            let term = Option.value ~default:"" search_term_opt in
            let strain_id = Option.bind strain_filter int_of_string_opt in
            let filters : Storage.Sample.sample_filters =
              {
                term;
                project_id = Some project.id;
                category = category_filter;
                sample_type = type_filter;
                strain_id;
              }
            in
            Storage.Sample.search_with_filters filters
          else Storage.Sample.get_by_project_id project.id
        in
        Lwt.return (Ok samples))
  in
  Api_utils.handle_response_with_export ~request ~filename:"samples.csv"
    ~csv_serializer:Core.Export_data.samples_to_csv
    ~json_serializer:(fun samples ->
      let response : Api_types.Sample.list_response =
        Api_types.Sample.{ data = samples; count = List.length samples }
      in
      Api_types.Sample.yojson_of_list_response response)
    result

(** Handles `POST /api/v1/projects/:identifier/samples`.

    Creates a single new sample and associates it with a project.

    @param identifier
      The ID, UUID, or Short ID of the project.
      - **Body**: (json) The sample creation payload, defined by
        [Api_types.Sample.create].
    @return
      A JSON response with the newly created sample and a [201 Created] status.
*)
let create_sample_handler request =
  let result =
    Auth.with_project_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~project ->
        let* req =
          Api_utils.parse_body_json Api_types.Sample.create_of_yojson request
        in
        let category_str = Option.value ~default:"Experimental" req.category in
        let* category =
          match Core.Types.sample_category_of_string category_str with
          | Ok cat -> Lwt.return (Ok cat)
          | Error msg -> Lwt.return (Error (`Bad_Request msg))
        in
        Storage.Sample.add ~project_id:project.id ~sample_type:req.sample_type
          ~category ~parent_sample_id:req.parent_sample_id
          ~strain_id:req.strain_id ~community_id:req.community_id
          ~result_definition_ids:req.result_definition_ids ())
  in

  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_sample result

(** Handles `POST /api/v1/projects/:identifier/samples/bulk`.

    Creates multiple samples in a single request for a given project. This is
    more efficient than making multiple individual creation requests.

    @param identifier
      The ID, UUID, or Short ID of the project.
      - **Body**: (json) The bulk sample creation payload, defined by
        [Api_types.Sample.bulk_create_request].
    @return A JSON response containing a list of the newly created samples. *)
let bulk_create_samples_handler request =
  let result =
    Auth.with_project_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~project ->
        let* req =
          Api_utils.parse_body_json
            Api_types.Sample.bulk_create_request_of_yojson request
        in
        let dry_run = Dream.query request "dry_run" = Some "true" in
        let* samples, summary =
          create_samples_batch ~project_id:project.id ~dry_run
            ?default_category:None req.samples
        in
        Lwt.return (Ok Api_types.Sample.{ samples; summary }))
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Api_types.Sample.yojson_of_bulk_create_response result

(** Handles `POST /api/v1/projects/:identifier/samples/bulk-csv`.

    Creates multiple samples from a CSV payload for a given project.

    @param identifier
      The ID, UUID, or Short ID of the project.
      - **Body**: (csv) The bulk sample creation payload.
    @return A JSON response containing a list of the newly created samples. *)
let bulk_csv_create_samples_handler request =
  let result =
    Auth.with_project_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~project ->
        let dry_run = Dream.query request "dry_run" = Some "true" in
        let default_category = Dream.query request "default_category" in
        let* create_items =
          Api_utils.parse_body_csv Decoders.sample_create request
        in
        let* samples, summary =
          create_samples_batch ~project_id:project.id ~dry_run ?default_category
            create_items
        in
        Lwt.return (Ok Api_types.Sample.{ samples; summary }))
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Api_types.Sample.yojson_of_bulk_create_response result

(** Handles `PUT /api/v1/samples/:identifier`.

    Updates an existing sample.

    @param identifier
      The ID, UUID, or Short ID of the sample to update.
      - **Body**: (json) The sample update payload, defined by
        [Api_types.Sample.update].
    @return A JSON response with the updated sample details. *)
let update_sample_handler request =
  let result =
    Auth.with_sample_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~sample ->
        let* req =
          Api_utils.parse_body_json Api_types.Sample.update_of_yojson request
        in
        let status = Option.value req.status ~default:sample.status in
        Storage.Sample.update ~id:sample.id ~sample_type:req.sample_type
          ~category:req.category ~parent_sample_id:req.parent_sample_id
          ~strain_id:req.strain_id ~community_id:req.community_id ~status)
  in

  Api_utils.handle_response ~serializer:Core.Types.yojson_of_sample result

(** Handles `GET /api/v1/samples/:identifier/locations`.

    Retrieves all the locations (plate and well) where a specific sample is
    stored.

    @param identifier The ID, UUID, or Short ID of the sample.
    @return A JSON response with a list of the sample's locations and a count.
*)
let get_sample_locations_handler request =
  let result =
    Auth.with_sample_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~sample ->
        let* locations = Storage.Well.get_locations_by_sample_id sample.id in
        let location_data = map_locations locations in
        let response : Api_types.Sample.location_list_response =
          Api_types.Sample.
            { data = location_data; count = List.length location_data }
        in
        Lwt.return (Ok response))
  in

  Api_utils.handle_response
    ~serializer:Api_types.Sample.yojson_of_location_list_response result

(** Handles `DELETE /api/v1/samples/:identifier`.

    Archives a sample, performing a soft delete.

    @param identifier The ID, UUID, or Short ID of the sample to archive.
    @return An empty response with a [204 No Content] status on success. *)
let archive_sample_handler request =
  let result =
    Auth.with_sample_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~sample -> Storage.Sample.archive sample.id)
  in
  Api_utils.handle_unit_result result

(** Handles `GET /api/v1/samples/:identifier/children`.

    Retrieves all samples that have the specified sample as their parent.

    @param identifier The ID, UUID, or Short ID of the parent sample.
    @return A JSON response containing a list of child samples. *)

let get_sample_children_dashboard_handler request =
  let result =
    Auth.with_sample_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~sample ->
        let* children = Storage.Sample.get_by_parent_id sample.id in
        let child_ids =
          List.map (fun (s : Core.Types.sample) -> s.id) children
        in
        let* all_values =
          Storage.Result_value.get_by_project_id sample.project_id
        in
        let values =
          List.filter
            (fun (r : Core.Types.result_value) ->
              match r.sample_id with
              | Some s_id -> List.mem s_id child_ids
              | None -> false)
            all_values
        in
        let* definitions = Storage.Result_definition.get_all () in
        let matrix_json =
          Core.Export_data.generate_matrix ~definitions ~samples:children
            ~plates:[] values
        in
        Lwt.return (Ok matrix_json))
  in
  Api_utils.handle_response ~serializer:(fun x -> x) result

let get_sample_children_handler request =
  let result =
    Auth.with_sample_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~sample ->
        Storage.Sample.get_by_parent_id sample.id)
  in
  Api_utils.handle_response
    ~serializer:(fun samples ->
      let response : Api_types.Sample.list_response =
        Api_types.Sample.{ data = samples; count = List.length samples }
      in
      Api_types.Sample.yojson_of_list_response response)
    result

(** Handles `GET /api/v1/samples`.

    Retrieves a list of all samples in the system. Supports CSV export.

    - **Query**: ?format=csv If specified, returns the sample list as a CSV
      file.

    @return
      A JSON response with a list of all samples and a count, or a CSV file. *)
let get_all_samples_handler request =
  let result =
    let open Lwt_result.Syntax in
    let* user_id, user_role = Auth.get_session_user request in
    let search_term_opt = Dream.query request "search" in
    let category_filter = Dream.query request "category" in
    let type_filter = Dream.query request "sample_type" in
    let strain_filter = Dream.query request "strain_id" in
    let project_filter = Dream.query request "project_id" in

    let has_filters =
      Option.is_some search_term_opt
      || Option.is_some category_filter
      || Option.is_some type_filter
      || Option.is_some strain_filter
      || Option.is_some project_filter
    in

    let* samples =
      if has_filters then
        let term = Option.value ~default:"" search_term_opt in
        let project_id =
          match project_filter with
          | Some s -> int_of_string_opt s
          | None -> None
        in
        let strain_id =
          match strain_filter with
          | Some s -> int_of_string_opt s
          | None -> None
        in
        let filters : Storage.Sample.sample_filters =
          {
            term;
            project_id;
            category = category_filter;
            sample_type = type_filter;
            strain_id;
          }
        in
        Auth.fetch_scoped_list ~user_id ~user_role
          ~get_all_function:(fun () ->
            Storage.Sample.search_with_filters filters)
          ~get_all_user_function:(fun uid ->
            Storage.Sample.search_with_filters_for_user uid filters)
      else
        Auth.fetch_scoped_list ~user_id ~user_role
          ~get_all_function:Storage.Sample.get_all
          ~get_all_user_function:Storage.Sample.get_all_for_user
    in

    Lwt.return (Ok samples)
  in
  Api_utils.handle_response_with_export ~request ~filename:"all_samples.csv"
    ~csv_serializer:Core.Export_data.samples_to_csv
    ~json_serializer:(fun samples ->
      let response : Api_types.Sample.list_response =
        Api_types.Sample.{ data = samples; count = List.length samples }
      in
      Api_types.Sample.yojson_of_list_response response)
    result

let routes =
  [
    Dream.get "/api/v1/samples/:identifier" get_sample_detailed_handler;
    Dream.get "/api/v1/projects/:identifier/samples" get_project_samples_handler;
    Dream.post "/api/v1/projects/:identifier/samples" create_sample_handler;
    Dream.post "/api/v1/projects/:identifier/samples/bulk"
      bulk_create_samples_handler;
    Dream.post "/api/v1/projects/:identifier/samples/bulk-csv"
      bulk_csv_create_samples_handler;
    Dream.put "/api/v1/samples/:identifier" update_sample_handler;
    Dream.get "/api/v1/samples/:identifier/locations"
      get_sample_locations_handler;
    Dream.delete "/api/v1/samples/:identifier" archive_sample_handler;
    Dream.get "/api/v1/samples/:identifier/children" get_sample_children_handler;
    Dream.get "/api/v1/samples" get_all_samples_handler;
    Dream.get "/api/v1/samples/:identifier/children/dashboard"
      get_sample_children_dashboard_handler;
  ]
