(** * This module defines the API routes for managing Results, which includes *
    result categories, result definitions, and result values. * * This module
    handles the HTTP layer, translating requests and responses * between the
    client and the underlying storage and core logic layers. It * uses helper
    functions from the [Utils] module for common tasks like * parameter parsing
    and response serialization. * * Key functionalities are grouped by resource
    type: * * **Result Categories**: * - List all categories. * - Create a new
    category. * * **Result Definitions**: * - List all definitions. * - Create a
    new definition. * - Retrieve a specific definition. * * **Result Values**: *
    \- List all results for a specific sample. * - Create a new result for a
    specific sample. * - List all results for a specific plate. * - Create a new
    result for a specific plate. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

(** {2 Result Category Routes} *)

(** Handles `GET /api/v1/result-categories`.

    Retrieves a list of all result categories.

    @return
      A JSON response containing a list of all categories and a total count. *)
let get_all_result_categories_handler _request =
  let result =
    let* categories = Storage.Result_category.get_all () in
    let response : Api_types.ResultCategory.list_response =
      Api_types.ResultCategory.
        { data = categories; count = List.length categories }
    in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response
    ~serializer:Api_types.ResultCategory.yojson_of_list_response result

(** Handles `POST /api/v1/result-categories`.

    Creates a new result category.

    - **Body**: (json) The category creation payload, defined by
      [Api_types.ResultCategory.create].

    @return
      A JSON response with the newly created category and a [201 Created]
      status. *)
let create_result_category_handler request =
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.ResultCategory.create_of_yojson
        request
    in
    Storage.Result_category.add ~name:req.name ~description:req.description
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_result_category result

(** Handles `GET /api/v1/result-categories/:identifier`.

    Retrieves a specific result category by its ID or UUID. *)
let get_result_category_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    Api_utils.find_result_category_by_identifier identifier
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_result_category
    result

(** Handles `PUT /api/v1/result-categories/:identifier`.

    Updates a specific result category by its ID or UUID. *)
let update_result_category_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* existing_cat =
      Api_utils.find_result_category_by_identifier identifier
    in
    let* req =
      Api_utils.parse_body_json Api_types.ResultCategory.update_of_yojson
        request
    in
    Storage.Result_category.update ~id:existing_cat.id ~name:req.name
      ~description:req.description
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_result_category
    result

(** {2 Result Definition Routes} *)

(** Handles `GET /api/v1/result-definitions`.

    Retrieves a list of all result definitions.

    @return
      A JSON response containing a list of all definitions and a total count. *)
let get_all_result_definitions_handler request =
  let search_term_opt = Dream.query request "search" in
  let result =
    let* defs =
      match search_term_opt with
      | Some term when String.length term > 0 ->
          Storage.Result_definition.search term
      | _ -> Storage.Result_definition.get_all ()
    in
    let response : Api_types.ResultDefinition.list_response =
      Api_types.ResultDefinition.{ data = defs; count = List.length defs }
    in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response_with_export ~request
    ~filename:"result_definitions.csv"
    ~csv_serializer:(fun res ->
      Core.Export_data.result_definitions_to_csv
        res.Api_types.ResultDefinition.data)
    ~json_serializer:Api_types.ResultDefinition.yojson_of_list_response result

(** Handles `POST /api/v1/result-definitions`.

    Creates a new result definition. This defines the schema for a type of
    result that can be recorded against a sample or plate.

    - **Body**: (json) The definition creation payload, defined by
      [Api_types.ResultDefinition.create].

    @return
      A JSON response with the newly created definition and a [201 Created]
      status. *)
let create_result_definition_handler request =
  let result =
    try
      let* req =
        Api_utils.parse_body_json Api_types.ResultDefinition.create_of_yojson
          request
      in
      Storage.Result_definition.add ~short_id:req.short_id ~name:req.name
        ~description:req.description ~data_type:req.data_type ~unit:req.unit
        ?category_id:req.category_id ~is_required:req.is_required ()
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid Payload: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON Error: " ^ msg)))
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_result_definition result

(** Handles `GET /api/v1/result-definitions/:identifier`.

    Retrieves a specific result definition by its ID, UUID, or Short ID.

    @param identifier The ID, UUID, or Short ID of the result definition.
    @return
      A JSON response containing the detailed information of the definition. *)
let get_result_definition_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    Api_utils.find_result_definition_by_identifier identifier
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_result_definition
    result

(** Handles `PUT /api/v1/result-definitions/:identifier`.

    Updates a specific result definition by its ID, UUID, or Short ID.

    @param identifier The ID, UUID, or Short ID of the result definition.
    @return A JSON response containing the updated definition. *)
let update_result_definition_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    let* existing_def =
      Api_utils.find_result_definition_by_identifier identifier
    in
    let* req =
      Api_utils.parse_body_json Api_types.ResultDefinition.update_of_yojson
        request
    in

    Storage.Result_definition.update ~id:existing_def.id ~short_id:req.short_id
      ~name:req.name ~description:req.description ~data_type:req.data_type
      ~unit:req.unit ?category_id:req.category_id ~is_required:req.is_required
      ()
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_result_definition
    result

(** {2 Result Value Routes} *)

(** Handles `GET /api/v1/samples/:id/results`.

    Retrieves all result values associated with a specific sample.

    @param id The integer ID of the sample.
    @return
      A JSON response containing a list of result values and a total count. *)
let get_sample_results_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let identifier = Dream.param request "identifier" in
    let* sample = Api_utils.find_sample_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role
        ~project_id:sample.project_id
    in
    let* values = Storage.Result_value.get_by_sample_id sample.id in

    let response : Api_types.ResultValue.list_response =
      Api_types.ResultValue.{ data = values; count = List.length values }
    in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response
    ~serializer:Api_types.ResultValue.yojson_of_list_response result

(** Handles `POST /api/v1/samples/:identifier/results`.

    Creates a new result value and associates it with a specific sample.

    @param identifier
      The ID, UUID, or Short ID of the sample.
      - **Body**: (json) The result value creation payload, defined by
        [Api_types.ResultValue.create].
    @return
      A JSON response with the newly created result value and a [201 Created]
      status. *)
let create_sample_result_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* req =
      Api_utils.parse_body_json Api_types.ResultValue.create_of_yojson request
    in
    let identifier = Dream.param request "identifier" in
    let* sample = Api_utils.find_sample_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role
        ~project_id:sample.project_id
    in

    let* definition =
      Api_utils.find_result_definition_by_identifier
        (string_of_int req.result_definition_id)
    in
    let* () =
      match Core.Result.validate_result definition req.value with
      | Ok () -> Lwt.return (Ok ())
      | Error msg -> Lwt.return (Error (`Bad_Request msg))
    in

    Storage.Result_value.add ~parent:(Storage.Result_value.Sample sample.id)
      ~result_definition_id:req.result_definition_id ~value:req.value
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_result_value result

(** Handles `GET /api/v1/plates/:identifier/results`.

    Retrieves all result values associated with a specific plate.

    @param identifier The ID, UUID, or Short ID of the plate.
    @return
      A JSON response containing a list of result values and a total count. *)
let get_plate_results_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let identifier = Dream.param request "identifier" in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in
    let* values = Storage.Result_value.get_by_plate_id plate.id in
    let* definitions = Storage.Result_definition.get_all () in
    Lwt.return (Ok (values, definitions))
  in
  Api_utils.handle_response_with_export ~request
    ~filename:
      (Printf.sprintf "plate_%s_results.csv" (Dream.param request "identifier"))
    ~csv_serializer:(fun (values, definitions) ->
      Core.Export_data.plate_results_to_csv ~definitions values)
    ~json_serializer:(fun (values, _) ->
      let response : Api_types.ResultValue.list_response =
        Api_types.ResultValue.{ data = values; count = List.length values }
      in
      Api_types.ResultValue.yojson_of_list_response response)
    result

(** Handles `POST /api/v1/plates/:identifier/results`.

    Creates a new result value and associates it with a specific plate.

    @param identifier
      The ID, UUID, or Short ID of the plate.
      - **Body**: (json) The result value creation payload, defined by
        [Api_types.ResultValue.create].
    @return
      A JSON response with the newly created result value and a [201 Created]
      status. *)
let create_plate_result_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* req =
      Api_utils.parse_body_json Api_types.ResultValue.create_of_yojson request
    in
    let identifier = Dream.param request "identifier" in
    let* plate = Api_utils.find_plate_by_identifier identifier in
    let* () =
      Auth.check_project_access ~user_id ~user_role ~project_id:plate.project_id
    in

    let* definition =
      Api_utils.find_result_definition_by_identifier
        (string_of_int req.result_definition_id)
    in
    let* () =
      match Core.Result.validate_result definition req.value with
      | Ok () -> Lwt.return (Ok ())
      | Error msg -> Lwt.return (Error (`Bad_Request msg))
    in

    Storage.Result_value.add ~parent:(Storage.Result_value.Plate plate.id)
      ~result_definition_id:req.result_definition_id ~value:req.value
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_result_value result

(* Bulk CSV Results *)

(** Handles `PUT /api/v1/results/:identifier`.

    Updates an existing result value.

    - **Body**: (json) The result value update payload, defined by
      [Api_types.ResultValue.update].

    @return A JSON response with the updated result value. *)
let update_result_value_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* req =
      Api_utils.parse_body_json Api_types.ResultValue.update_of_yojson request
    in
    let identifier = Dream.param request "identifier" in

    let* res_val_opt =
      match Api_types.Identifier.of_string identifier with
      | Id id -> Storage.Result_value.get_by_id id
      | Uuid uid -> Storage.Result_value.get_by_uid uid
      | Short_id s ->
          Lwt_result.fail
            (`Bad_Request ("Unsupported identifier type for result value: " ^ s))
    in

    match res_val_opt with
    | None -> Lwt_result.fail (`Not_Found "Result value not found")
    | Some res_val ->
        let* () =
          (* check project access logic based on parent sample or plate *)
          match (res_val.sample_id, res_val.plate_id) with
          | Some sid, _ ->
              let* sample =
                Api_utils.find_sample_by_identifier (string_of_int sid)
              in
              Auth.check_project_access ~user_id ~user_role
                ~project_id:sample.project_id
          | _, Some pid ->
              let* plate =
                Api_utils.find_plate_by_identifier (string_of_int pid)
              in
              Auth.check_project_access ~user_id ~user_role
                ~project_id:plate.project_id
          | _ -> Lwt_result.return ()
        in
        Storage.Result_value.update ~id:res_val.id ~value:req.value
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_result_value result

(** Handles `PATCH /api/v1/results/:identifier/append`.

    Appends a single data point to an existing TimeSeries result.

    - **Body**: (json) The append payload, defined by
      [Api_types.ResultValue.patch_append].

    @return A JSON response with the updated result value. *)
let append_result_value_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in
    let* req =
      Api_utils.parse_body_json Api_types.ResultValue.patch_append_of_yojson
        request
    in
    let identifier = Dream.param request "identifier" in

    let* res_val_opt =
      match Api_types.Identifier.of_string identifier with
      | Id id -> Storage.Result_value.get_by_id id
      | Uuid uid -> Storage.Result_value.get_by_uid uid
      | Short_id s ->
          Lwt_result.fail
            (`Bad_Request ("Unsupported identifier type for result value: " ^ s))
    in

    match res_val_opt with
    | None -> Lwt_result.fail (`Not_Found "Result value not found")
    | Some res_val ->
        let* () =
          (* check project access logic based on parent sample or plate *)
          match (res_val.sample_id, res_val.plate_id) with
          | Some sid, _ ->
              let* sample =
                Api_utils.find_sample_by_identifier (string_of_int sid)
              in
              Auth.check_project_access ~user_id ~user_role
                ~project_id:sample.project_id
          | _, Some pid ->
              let* plate =
                Api_utils.find_plate_by_identifier (string_of_int pid)
              in
              Auth.check_project_access ~user_id ~user_role
                ~project_id:plate.project_id
          | _ -> Lwt_result.return ()
        in
        Storage.Result_value.append_timeseries_point ~id:res_val.id
          ~time:req.time ~scalar_value:req.value
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_result_value result

(* Bulk Result Values *)

(* 1. Helper to validate headers and fetch Result Definitions *)
let resolve_bulk_csv_headers headers =
  let%lwt defs_res =
    Lwt_list.map_p
      (fun def_id_str ->
        let def_id = String.trim def_id_str in
        match%lwt Api_utils.find_result_definition_by_identifier def_id with
        | Ok def -> Lwt.return (Ok def)
        | Error _ ->
            Lwt.return
              (Error (Printf.sprintf "Result definition %s not found" def_id)))
      headers
  in
  let def_errors =
    List.filter_map (function Error e -> Some e | Ok _ -> None) defs_res
  in
  if def_errors <> [] then
    Lwt.return (Error (`Not_Found (String.concat "; " def_errors)))
  else
    let defs =
      List.map (function Ok d -> d | _ -> failwith "impossible") defs_res
    in
    Lwt.return (Ok defs)

(* 2. Helper to fetch the target Sample/Plate and check Auth *)
let resolve_bulk_csv_parent ~user_id ~user_role ~is_sample target_id =
  if is_sample then
    match%lwt Api_utils.find_sample_by_identifier target_id with
    | Ok s -> (
        match%lwt
          Auth.check_project_access ~user_id ~user_role
            ~project_id:s.Core.Types.project_id
        with
        | Ok () -> Lwt.return (Ok (Storage.Result_value.Sample s.Core.Types.id))
        | Error e -> Lwt.return (Error e))
    | Error _ ->
        Lwt.return
          (Error (`Not_Found (Printf.sprintf "Sample %s not found" target_id)))
  else
    match%lwt Api_utils.find_plate_by_identifier target_id with
    | Ok p -> (
        match%lwt
          Auth.check_project_access ~user_id ~user_role
            ~project_id:p.Core.Types.project_id
        with
        | Ok () -> Lwt.return (Ok (Storage.Result_value.Plate p.Core.Types.id))
        | Error e -> Lwt.return (Error e))
    | Error _ ->
        Lwt.return
          (Error (`Not_Found (Printf.sprintf "Plate %s not found" target_id)))

(* 3. Helper to validate a single row against the definitions *)
let parse_bulk_csv_row ~user_id ~user_role ~is_sample ~defs row_num target_id
    row_values =
  match%lwt
    resolve_bulk_csv_parent ~user_id ~user_role ~is_sample target_id
  with
  | Error e -> Lwt.return (Error [ e ])
  | Ok parent ->
      let cell_results =
        List.mapi
          (fun col_i val_str ->
            let val_str = String.trim val_str in
            if val_str = "" then None
            else if col_i >= List.length defs then
              Some
                (Error
                   (`Bad_Request
                      (Printf.sprintf "Row %d has more columns than the header"
                         row_num)))
            else
              let (def : Core.Types.result_definition) = List.nth defs col_i in
              match Core.Result.payload_of_string def.data_type val_str with
              | Ok p -> Some (Ok (parent, def.id, p))
              | Error e ->
                  Some
                    (Error
                       (`Bad_Request
                          (Printf.sprintf "Row %d, Col %d: %s" row_num
                             (col_i + 2) e))))
          row_values
      in
      let errors =
        List.filter_map
          (function Some (Error e) -> Some e | _ -> None)
          cell_results
      in
      if errors <> [] then Lwt.return (Error errors)
      else
        let valid_ops =
          List.filter_map
            (function Some (Ok op) -> Some op | _ -> None)
            cell_results
        in
        Lwt.return (Ok valid_ops)

(* 4. The Orchestrator *)
let create_results_batch_csv ~user_id ~user_role body =
  let csv_data = Csv.of_string body |> Csv.input_all in
  match csv_data with
  | [] | [ _ ] ->
      Lwt.return
        (Error
           (`Bad_Request
              "CSV must contain a header row and at least one data row"))
  | header :: rows -> (
      let target_header = String.trim (List.hd header) in
      let is_sample = target_header = "sample_short_id" in
      let is_plate = target_header = "plate_short_id" in

      if not (is_sample || is_plate) then
        Lwt.return
          (Error
             (`Bad_Request
                "First column header must be 'sample_short_id' or \
                 'plate_short_id'"))
      else
        let def_headers = List.tl header in
        match%lwt resolve_bulk_csv_headers def_headers with
        | Error e -> Lwt.return (Error e)
        | Ok defs -> (
            let%lwt validation_results =
              Lwt_list.mapi_p
                (fun i row ->
                  let row_num = i + 2 in
                  let target_id = String.trim (List.hd row) in
                  let values = List.tl row in
                  parse_bulk_csv_row ~user_id ~user_role ~is_sample ~defs
                    row_num target_id values)
                rows
            in

            let all_errors =
              List.fold_left
                (fun acc res ->
                  match res with Error errs -> acc @ errs | Ok _ -> acc)
                [] validation_results
            in

            if all_errors <> [] then
              let has_forbidden =
                List.exists
                  (function `Forbidden _ -> true | _ -> false)
                  all_errors
              in
              let has_not_found =
                List.exists
                  (function `Not_Found _ -> true | _ -> false)
                  all_errors
              in
              let msgs =
                List.map
                  (function
                    | `Forbidden m -> m
                    | #Caqti_error.t as err ->
                        "DB Error: " ^ Caqti_error.show err
                    | `Not_Found m -> m
                    | `Bad_Request m -> m)
                  all_errors
              in
              if has_forbidden then
                Lwt.return (Error (`Forbidden (String.concat "; " msgs)))
              else if has_not_found then
                Lwt.return (Error (`Not_Found (String.concat "; " msgs)))
              else Lwt.return (Error (`Bad_Request (String.concat "; " msgs)))
            else
              let valid_ops =
                List.fold_left
                  (fun acc res ->
                    match res with Ok ops -> acc @ ops | Error _ -> acc)
                  [] validation_results
              in
              let%lwt final_results =
                Lwt_list.fold_left_s
                  (fun acc_result (parent, def_id, parsed_value) ->
                    match acc_result with
                    | Error e -> Lwt.return (Error e)
                    | Ok acc -> (
                        match%lwt
                          Storage.Result_value.add ~parent
                            ~result_definition_id:def_id
                            ~value:(Some parsed_value)
                        with
                        | Ok result -> Lwt.return (Ok (result :: acc))
                        | Error e -> Lwt.return (Error e)))
                  (Ok []) valid_ops
              in
              match final_results with
              | Ok list -> Lwt.return (Ok (List.rev list))
              | Error e -> Lwt.return (Error e)))

(** Handles `POST /api/v1/results/bulk-csv`.

    Creates multiple result values from a CSV payload.

    - **Body**: (csv) The bulk result value creation payload.

    @return
      A JSON response containing a list of the newly created result values. *)

let create_bulk_csv_results_handler request =
  let result =
    let* user_id, user_role = Auth.get_session_user request in

    let* body = Dream.body request |> Lwt_result.ok in

    create_results_batch_csv ~user_id ~user_role body
  in

  Api_utils.handle_response ~status:`Created
    ~serializer:(fun results ->
      `List (List.map Core.Types.yojson_of_result_value results))
    result

let routes =
  [
    Dream.get "/api/v1/result-categories" get_all_result_categories_handler;
    Dream.post "/api/v1/result-categories" create_result_category_handler;
    Dream.get "/api/v1/result-categories/:identifier"
      get_result_category_handler;
    Dream.put "/api/v1/result-categories/:identifier"
      update_result_category_handler;
    Dream.get "/api/v1/result-definitions" get_all_result_definitions_handler;
    Dream.post "/api/v1/result-definitions" create_result_definition_handler;
    Dream.get "/api/v1/result-definitions/:identifier"
      get_result_definition_handler;
    Dream.put "/api/v1/result-definitions/:identifier"
      update_result_definition_handler;
    Dream.get "/api/v1/samples/:identifier/results" get_sample_results_handler;
    Dream.post "/api/v1/samples/:identifier/results"
      create_sample_result_handler;
    Dream.get "/api/v1/plates/:identifier/results" get_plate_results_handler;
    Dream.post "/api/v1/plates/:identifier/results" create_plate_result_handler;
    Dream.put "/api/v1/results/:identifier" update_result_value_handler;
    Dream.patch "/api/v1/results/:identifier/append" append_result_value_handler;
    Dream.post "/api/v1/results/bulk-csv" create_bulk_csv_results_handler;
  ]
