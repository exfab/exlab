(** * This module defines the API routes for managing Plates. * It provides
    endpoints for creating, retrieving, and listing Plates. * * This module
    handles the HTTP layer, translating requests and responses * between the
    client and the underlying storage and core logic layers. It * uses helper
    functions from the [Utils] module for common tasks like * parameter parsing
    and response serialization. * * Key functionalities include: * - Listing all
    plates. * - Creating a new plate. * - Retrieving a specific plate and its
    wells, with CSV export support. * - Listing all plates associated with a
    specific project. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

(** Handles `GET /api/v1/plates`.

    Retrieves a list of all plates in the system.

    @return A JSON response containing a list of all plates and a total count.
*)
let get_all_plates_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* plates =
      Auth.fetch_scoped_list ~user_id ~user_role
        ~get_all_function:Storage.Plate.get_all
        ~get_all_user_function:Storage.Plate.get_all_for_user
    in
    let response : Api_types.Plate.list_response =
      Api_types.Plate.{ data = plates; count = List.length plates }
    in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response ~serializer:Api_types.Plate.yojson_of_list_response
    result

(** Handles `POST /api/v1/plates`.

    Creates a new plate and its associated wells. The request body must specify
    the plate's name, format, and the ID of the project it belongs to.

    - **Body**: (json) The plate creation payload, defined by
      [Api_types.Plate.create].

    @return
      A JSON response with the newly created plate and a [201 Created] status.
*)
let create_plate_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* req =
      Api_utils.parse_body_json Api_types.Plate.create_of_yojson request
    in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:req.project_id
    in

    Storage.Plate.add ?category:req.category ~name:req.name
      ~project_id:req.project_id ~product_id:req.product_id
      ~plate_format:req.plate_format ()
  in

  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_plate result

(** Handles `GET /api/v1/plates/:identifier`.

    Retrieves detailed information for a specific plate, identified by its ID,
    UUID, or Short ID.

    @param identifier The ID, UUID, or Short ID of the plate.
    @return Returns a detailed JSON object for the plate, including its wells.
*)

let get_plate_dashboard_handler request =
  let result =
    let open Lwt_result.Syntax in
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in

    let* wells = Storage.Well.get_by_plate_id plate.id in

    let* samples = Storage.Sample.get_by_plate_id plate.id in
    let* values = Storage.Result_value.get_by_plate_and_its_samples plate.id in
    let* definitions = Storage.Result_definition.get_all () in

    let matrix_json =
      Core.Export_data.generate_matrix ~definitions ~samples ~plates:[ plate ]
        values
    in
    Lwt.return (Ok matrix_json)
  in
  Api_utils.handle_response ~serializer:(fun x -> x) result

let get_plate_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let identifier = Dream.param request "identifier" in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in
    let* wells = Storage.Well.get_by_plate_id plate.id in

    let response_wells =
      wells
      |> List.sort (fun w1 w2 ->
          Core.Plate.compare_coordinates w1.Core.Types.coordinate
            w2.Core.Types.coordinate)
      |> List.map (fun (w : Core.Types.well) ->
          let is_edge =
            Core.Plate.is_edge_well plate.plate_format w.coordinate
          in
          Api_types.Plate.{ well = w; is_edge })
    in
    let response = Api_types.Plate.{ plate; wells = response_wells } in

    Lwt.return (Ok response)
  in

  Api_utils.handle_response ~serializer:Api_types.Plate.yojson_of_detailed
    result

(** Handles `GET /api/v1/plates/:identifier/export`.

    Exports the plate's well data in various CSV formats.

    @param identifier
      The ID, UUID, or Short ID of the plate.
      - **Query**: ?export_format The desired format: "numeric", "matrix", or
        default.
    @return A CSV file attachment. *)
let export_plate_handler request =
  let result =
    let identifier = Dream.param request "identifier" in

    let export_format = Dream.query request "export_format" in

    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* rows = Storage.Export.get_plate_wells_for_csv plate.id in

    let csv_content =
      match export_format with
      | Some "numeric" ->
          Core.Export_data.plate_wells_to_numeric_csv plate.plate_format rows
      | Some "matrix" ->
          Core.Export_data.plate_wells_to_matrix_csv plate.plate_format rows
      | _ -> Core.Export_data.plate_wells_to_csv_rows rows
    in

    let filename = Printf.sprintf "plate_%s.csv" plate.short_id in

    Dream.respond
      ~headers:
        [
          ("Content-Type", "text/csv");
          ( "Content-Disposition",
            Printf.sprintf "attachment; filename=\"%s\"" filename );
        ]
      csv_content
    |> Lwt.return_ok
  in
  match%lwt result with
  | Ok response -> response
  | Error err -> Api_utils.respond_with_error err

module CoordSet = Set.Make (String)

let process_layout_items plate_format
    (items : Api_types.Plate.well_layout_item list) =
  let rec process seen (acc : Api_types.Plate.well_layout_item list) = function
    | [] -> Lwt.return (Ok (List.rev acc))
    | (item : Api_types.Plate.well_layout_item) :: tail -> (
        let well_str = item.well in
        match Core.Plate.normalize_coordinate plate_format well_str with
        | Ok coord ->
            if CoordSet.mem coord seen then
              Lwt.return
                (Error
                   (`Bad_Request
                      (Printf.sprintf "Duplicate well coordinate '%s' specified"
                         coord)))
            else
              process (CoordSet.add coord seen)
                ({ item with well = coord } :: acc)
                tail
        | Error msg -> Lwt.return (Error (`Bad_Request msg)))
  in
  process CoordSet.empty [] items

let process_bulk_layout_items plate_format items =
  process_layout_items plate_format items

(** Validates layout items against the plate format, checks sample existence,
    and persists the update to the database.

    @return : (Ok status_response) or (Error (`Bad_Request msg)) *)
let update_plate_layout ~(plate : Core.Types.plate)
    ~(layout_items : Api_types.Plate.well_layout_item list) =
  let sample_short_ids =
    List.map
      (fun (item : Api_types.Plate.well_layout_item) -> item.sample_short_id)
      layout_items
    |> List.sort_uniq Exlab_core.String_utils.natural_compare
  in
  let* samples = Storage.Sample.get_many_by_short_ids sample_short_ids in

  let* () =
    if List.length samples <> List.length sample_short_ids then
      Lwt.return
        (Error (`Bad_Request "One or more sample short IDs are invalid"))
    else Lwt.return (Ok ())
  in

  let* existing_categories = Storage.Well.get_plate_categories plate.id in
  let incoming_categories =
    List.map
      (fun (item : Api_types.Plate.well_layout_item) ->
        let s =
          List.find
            (fun (s : Core.Types.sample) -> s.short_id = item.sample_short_id)
            samples
        in
        s.category)
      layout_items
  in
  let* () =
    match incoming_categories with
    | [] -> Lwt.return (Ok ())
    | first_cat :: rest -> (
        if List.exists (fun c -> c <> first_cat) rest then
          Lwt.return
            (Error
               (`Bad_Request
                  "Compliance Violation: Source and Experimental samples \
                   cannot be mixed on the same plate."))
        else
          match
            Core.Plate.validate_sample_mixture ~existing_categories
              ~new_category:first_cat
          with
          | Ok () -> Lwt.return (Ok ())
          | Error msg -> Lwt.return (Error (`Bad_Request msg)))
  in

  let sample_map =
    List.map (fun (s : Core.Types.sample) -> (s.short_id, s.id)) samples
    |> List.to_seq |> Hashtbl.of_seq
  in

  let layout =
    List.map
      (fun (item : Api_types.Plate.well_layout_item) ->
        (item.well, Hashtbl.find sample_map item.sample_short_id))
      layout_items
  in

  let* () = Storage.Well.update_layout ~plate_id:plate.id layout in

  Lwt.return
    (Ok
       ({ status = "Plate layout updated successfully" }
         : Api_types.Common.status_response))

(** Handles `GET /api/v1/projects/:identifier/plates`.

    Retrieves all plates associated with a specific project.

    @param identifier The ID, UUID, or Short ID of the project.
    @return
      A JSON response containing a list of the project's plates and a total
      count. *)
let get_project_plates_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in
    let* plates = Storage.Plate.get_by_project_id project.id in
    Lwt.return (Ok plates)
  in

  Api_utils.handle_response_with_export ~request ~filename:"plates.csv"
    ~csv_serializer:Core.Export_data.plates_to_csv
    ~json_serializer:(fun plates ->
      let response : Api_types.Plate.list_response =
        Api_types.Plate.{ data = plates; count = List.length plates }
      in
      Api_types.Plate.yojson_of_list_response response)
    result

(** Handles `POST /api/v1/plates/:identifier/layout-csv`.

    Updates the plate layout using a CSV payload.

    @param identifier
      The ID, UUID, or Short ID of the plate.
      - **Body**: (csv) The layout update data.
    @return A status response JSON object. *)
let update_plate_layout_csv_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in

    let* layout_items =
      Api_utils.parse_body_csv Decoders.well_layout_item request
    in
    let* processed_layout_items =
      process_layout_items plate.plate_format layout_items
    in
    update_plate_layout ~plate ~layout_items:processed_layout_items
  in
  Api_utils.handle_response
    ~serializer:Api_types.Common.yojson_of_status_response result

(** Handles `POST /api/v1/plates/:identifier/layout-json`.

    Updates the plate layout using a JSON payload.

    @param identifier
      The ID, UUID, or Short ID of the plate.
      - **Body**: (json) The layout update data.
    @return A status response JSON object. *)
let update_plate_layout_json_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in

    let* layout_items =
      Api_utils.parse_body_json Api_types.Plate.well_layout_of_yojson request
    in
    let* processed_layout_items =
      process_layout_items plate.plate_format layout_items
    in
    update_plate_layout ~plate ~layout_items:processed_layout_items
  in
  Api_utils.handle_response
    ~serializer:Api_types.Common.yojson_of_status_response result

(** Handles `POST /api/v1/plates/:identifier/layout-matrix-csv`.

    Updates the plate layout using a matrix CSV payload.

    @param identifier
      The ID, UUID, or Short ID of the plate.
      - **Body**: (csv) The layout update data in matrix format.
    @return A status response JSON object. *)
let update_plate_layout_matrix_csv_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in

    let* body = Lwt_result.ok (Dream.body request) in
    let* layout_items =
      match Csv_utils.parse_matrix body with
      | Ok items -> Lwt.return (Ok items)
      | Error err -> Lwt.return (Error err)
    in

    let* processed_layout_items =
      process_layout_items plate.plate_format layout_items
    in

    update_plate_layout ~plate ~layout_items:processed_layout_items
  in
  Api_utils.handle_response
    ~serializer:Api_types.Common.yojson_of_status_response result

(** Handles `POST /api/v1/projects/:identifier/plates/bulk-csv`.

    Creates multiple plates and assigns wells using a single CSV payload.

    @param identifier The ID, UUID, or Short ID of the project.
    @return A JSON response containing a list of the newly created plates. *)
let bulk_create_plates_csv_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in

    let plate_format_str =
      Option.value ~default:"96-well" (Dream.query request "plate_format")
    in
    let* plate_format =
      match Core.Plate.validate_format plate_format_str with
      | Ok format -> Lwt_result.return format
      | Error msg -> Lwt_result.fail (`Bad_Request msg)
    in

    let plate_category_str = Dream.query request "plate_type" in

    let* create_items =
      Api_utils.parse_body_csv Decoders.bulk_plate_action_item request
    in

    let unique_plate_names =
      List.map
        (fun (item : Api_types.Plate.bulk_action_item) ->
          match item with
          | Create_blank { plate_name } -> plate_name
          | Create_with_layout { plate_name; _ } -> plate_name)
        create_items
      |> List.sort_uniq Exlab_core.String_utils.natural_compare
    in

    let grouped_items =
      let map = Hashtbl.create (List.length unique_plate_names) in
      List.iter
        (fun (item : Api_types.Plate.bulk_action_item) ->
          match item with
          | Create_blank { plate_name } ->
              if not (Hashtbl.mem map plate_name) then
                Hashtbl.add map plate_name []
          | Create_with_layout { plate_name; well; sample_short_id } ->
              let current_list =
                match Hashtbl.find_opt map plate_name with
                | Some l -> l
                | None -> []
              in
              Hashtbl.replace map plate_name
                ((well, sample_short_id) :: current_list))
        create_items;
      map
    in

    let plates_with_layouts =
      List.map
        (fun plate_name ->
          let layouts = Hashtbl.find grouped_items plate_name in
          { Storage.Plate.name = plate_name; layouts = List.rev layouts })
        unique_plate_names
    in

    let* created_plates =
      Storage.Plate.create_many_with_layouts ?category:plate_category_str
        ~project_id:project.id ~plate_format plates_with_layouts
    in

    Lwt_result.return created_plates
  in

  Api_utils.handle_response ~status:`Created
    ~serializer:(fun plates ->
      `Assoc
        [
          ( "plates",
            `List (List.map Core.Types.yojson_of_plate (List.rev plates)) );
        ])
    result

let bulk_update_plate_layouts_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* project = Api_utils.find_project_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:project.id
    in

    let* layout_items =
      Api_utils.parse_body_csv Decoders.bulk_plate_layout_item request
    in

    let* project_plates = Storage.Plate.get_by_project_id project.id in

    let resolve_plate plate_ident =
      let trimmed = String.trim plate_ident in
      let lower = String.lowercase_ascii trimmed in
      (* 1. Exact match on short_id *)
      match
        List.find_opt
          (fun (p : Core.Types.plate) -> p.short_id = trimmed)
          project_plates
      with
      | Some p -> Ok p
      | None -> (
          (* 2. Match on uid or id *)
          match
            List.find_opt
              (fun (p : Core.Types.plate) ->
                p.uid = trimmed || string_of_int p.id = trimmed)
              project_plates
          with
          | Some p -> Ok p
          | None -> (
              (* 3. Case-insensitive match on name *)
              let matching_by_name =
                List.filter
                  (fun (p : Core.Types.plate) ->
                    String.lowercase_ascii p.name = lower)
                  project_plates
              in
              match matching_by_name with
              | [ single ] -> Ok single
              | [] ->
                  Error
                    (`Not_Found
                       (Printf.sprintf "Plate '%s' not found in this project."
                          plate_ident))
              | _ ->
                  Error
                    (`Bad_Request
                       (Printf.sprintf
                          "Multiple plates found with name '%s'. Please use \
                           plate_short_id instead."
                          plate_ident))))
    in

    (* Group items by plate identifier *)
    let grouped_items =
      List.fold_left
        (fun map item ->
          let current_list =
            match Hashtbl.find_opt map item.Api_types.Plate.plate_name with
            | Some l -> l
            | None -> []
          in
          Hashtbl.replace map item.Api_types.Plate.plate_name
            (item :: current_list);
          map)
        (Hashtbl.create 10) layout_items
    in

    (* Pre-validate all plate identifiers and coordinates *)
    let* plate_and_layout_list =
      Lwt_list.fold_left_s
        (fun acc_res plate_ident ->
          match acc_res with
          | Error e -> Lwt.return (Error e)
          | Ok acc -> (
              match resolve_plate plate_ident with
              | Error e -> Lwt.return (Error e)
              | Ok existing_plate ->
                  let items = Hashtbl.find grouped_items plate_ident in
                  let well_items =
                    List.map
                      (fun (item : Api_types.Plate.bulk_layout_item) ->
                        {
                          Api_types.Plate.well = item.well;
                          sample_short_id = item.sample_short_id;
                        })
                      items
                  in
                  let* processed_layout_items =
                    process_bulk_layout_items existing_plate.plate_format
                      well_items
                  in
                  Lwt.return
                    (Ok ((existing_plate, processed_layout_items) :: acc))))
        (Ok [])
        (Hashtbl.to_seq_keys grouped_items |> List.of_seq)
    in

    (* Pre-validate that all samples exist across all plates before applying updates *)
    let all_sample_short_ids =
      List.concat_map
        (fun (_, layout_items) ->
          List.map
            (fun (item : Api_types.Plate.well_layout_item) ->
              item.sample_short_id)
            layout_items)
        plate_and_layout_list
      |> List.sort_uniq Exlab_core.String_utils.natural_compare
    in
    let* samples = Storage.Sample.get_many_by_short_ids all_sample_short_ids in
    let* () =
      if List.length samples <> List.length all_sample_short_ids then
        Lwt_result.fail
          (`Bad_Request "One or more sample short IDs are invalid")
      else Lwt_result.return ()
    in

    let sample_map =
      List.map (fun (s : Core.Types.sample) -> (s.short_id, s.id)) samples
      |> List.to_seq |> Hashtbl.of_seq
    in

    let sample_entity_map =
      List.map (fun (s : Core.Types.sample) -> (s.short_id, s)) samples
      |> List.to_seq |> Hashtbl.of_seq
    in

    (* Pre-validate sample category compliance for each plate *)
    let* () =
      Lwt_list.fold_left_s
        (fun acc (existing_plate, layout_items) ->
          match acc with
          | Error e -> Lwt.return (Error e)
          | Ok () -> (
              let incoming_categories =
                List.map
                  (fun (item : Api_types.Plate.well_layout_item) ->
                    (Hashtbl.find sample_entity_map item.sample_short_id)
                      .category)
                  layout_items
              in
              match incoming_categories with
              | [] -> Lwt.return (Ok ())
              | first_cat :: rest -> (
                  if List.exists (fun c -> c <> first_cat) rest then
                    let msg =
                      Printf.sprintf
                        "Compliance Violation: Source and Experimental samples \
                         cannot be mixed on plate '%s'."
                        existing_plate.Core.Types.short_id
                    in
                    Lwt.return (Error (`Bad_Request msg))
                  else
                    let* existing_categories =
                      Storage.Well.get_plate_categories existing_plate.id
                    in
                    match
                      Core.Plate.validate_sample_mixture ~existing_categories
                        ~new_category:first_cat
                    with
                    | Ok () -> Lwt.return (Ok ())
                    | Error msg -> Lwt.return (Error (`Bad_Request msg)))))
        (Ok ()) plate_and_layout_list
    in

    let updates =
      List.map
        (fun ((existing_plate : Core.Types.plate), processed_layout_items) ->
          let layout =
            List.map
              (fun (item : Api_types.Plate.well_layout_item) ->
                (item.well, Hashtbl.find sample_map item.sample_short_id))
              processed_layout_items
          in
          (existing_plate.id, layout))
        plate_and_layout_list
    in

    let* () = Storage.Well.update_many_layouts updates in

    Lwt_result.return
      ({ status = "Bulk plate layouts updated successfully" }
        : Api_types.Common.status_response)
  in
  Api_utils.handle_response
    ~serializer:Api_types.Common.yojson_of_status_response result

(** Handles `POST /api/v1/plates/:identifier/auto-fill`.

    Generates and saves a well layout by pairing selected samples with selected
    wells.

    @param identifier The ID, UUID, or Short ID of the plate.
    @return A status response JSON object. *)
let auto_fill_plate_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in

    let* req_payload =
      Api_utils.parse_body_json Api_types.Plate.auto_fill_request_of_yojson
        request
    in

    let* layout_items =
      match
        Core.Plate_planner.generate_autofill_layout
          ~strategy:req_payload.strategy ~wells:req_payload.wells
          ~sample_ids:req_payload.sample_short_ids
      with
      | Ok items ->
          let api_items =
            List.map
              (fun (well, sample_short_id) ->
                ({ well; sample_short_id } : Api_types.Plate.well_layout_item))
              items
          in
          Lwt_result.return api_items
      | Error msg -> Lwt_result.fail (`Bad_Request msg)
    in

    let* processed_layout_items =
      process_layout_items plate.plate_format layout_items
    in
    update_plate_layout ~plate ~layout_items:processed_layout_items
  in
  Api_utils.handle_response
    ~serializer:Api_types.Common.yojson_of_status_response result

(** Handles `POST /api/v1/plates/:identifier/unassign-bulk`.

    Unassigns samples from multiple wells on a plate at once.

    @param identifier The ID, UUID, or Short ID of the plate.
    @return A status response JSON object. *)
let unassign_bulk_plate_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* user_id, user_role = Auth.get_session_user request in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in

    let* req_payload =
      Api_utils.parse_body_json Api_types.Plate.bulk_unassign_request_of_yojson
        request
    in

    (* Validate well boundaries *)
    let* () =
      Lwt_list.iter_s
        (fun well_str ->
          match Core.Plate.normalize_coordinate plate.plate_format well_str with
          | Ok _ -> Lwt.return_unit
          | Error msg -> Lwt.fail_with msg)
        req_payload.wells
      |> Lwt_result.ok
    in

    let* () = Storage.Well.unassign_bulk ~plate_id:plate.id req_payload.wells in

    Lwt_result.return
      ({ status = "Wells unassigned successfully" }
        : Api_types.Common.status_response)
  in
  Api_utils.handle_response
    ~serializer:Api_types.Common.yojson_of_status_response result

let routes =
  [
    Dream.get "/api/v1/plates" get_all_plates_handler;
    Dream.post "/api/v1/plates" create_plate_handler;
    Dream.get "/api/v1/plates/:identifier" get_plate_handler;
    Dream.get "/api/v1/plates/:identifier/export" export_plate_handler;
    Dream.get "/api/v1/projects/:identifier/plates" get_project_plates_handler;
    Dream.post "/api/v1/projects/:identifier/plates/bulk-csv"
      bulk_create_plates_csv_handler;
    Dream.patch "/api/v1/projects/:identifier/plates/bulk-csv"
      bulk_update_plate_layouts_handler;
    Dream.post "/api/v1/plates/:identifier/layout-csv"
      update_plate_layout_csv_handler;
    Dream.post "/api/v1/plates/:identifier/layout-json"
      update_plate_layout_json_handler;
    Dream.post "/api/v1/plates/:identifier/auto-fill" auto_fill_plate_handler;
    Dream.post "/api/v1/plates/:identifier/unassign-bulk"
      unassign_bulk_plate_handler;
    Dream.post "/api/v1/plates/:identifier/layout-matrix-csv"
      update_plate_layout_matrix_csv_handler;
    Dream.get "/api/v1/plates/:identifier/dashboard" get_plate_dashboard_handler;
  ]
