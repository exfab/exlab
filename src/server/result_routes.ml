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
    Auth.with_sample_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~sample ->
        let* values = Storage.Result_value.get_by_sample_id sample.id in
        let response : Api_types.ResultValue.list_response =
          Api_types.ResultValue.{ data = values; count = List.length values }
        in
        Lwt.return (Ok response))
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
    let* req =
      Api_utils.parse_body_json Api_types.ResultValue.create_of_yojson request
    in
    Auth.with_sample_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~sample ->
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
          ~result_definition_id:req.result_definition_id ~value:req.value)
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
    Auth.with_plate_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~plate ->
        let* values = Storage.Result_value.get_by_plate_id plate.id in
        let* definitions = Storage.Result_definition.get_all () in
        Lwt.return (Ok (values, definitions)))
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
    let* req =
      Api_utils.parse_body_json Api_types.ResultValue.create_of_yojson request
    in
    Auth.with_plate_access request "identifier"
      (fun ~user_id:_ ~user_role:_ ~plate ->
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
          ~result_definition_id:req.result_definition_id ~value:req.value)
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
let resolve_single_header idx def_id_str =
  match Core.String_utils.non_empty def_id_str with
  | None ->
      Lwt_result.fail
        (`Bad_Request
           (Printf.sprintf
              "Header column %d is blank; expected a result definition \
               identifier"
              (idx + 2)))
  | Some def_id -> (
      match%lwt Api_utils.find_result_definition_by_identifier def_id with
      | Ok def -> Lwt_result.return def
      | Error _ ->
          Lwt_result.fail
            (`Not_Found (Printf.sprintf "Result definition %s not found" def_id))
      )

let resolve_bulk_csv_headers = function
  | [] ->
      Lwt_result.fail
        (`Bad_Request "CSV must include at least one result definition column")
  | headers -> (
      let%lwt results = Lwt_list.mapi_s resolve_single_header headers in
      let errors, defs =
        List.partition_map
          (function Ok d -> Either.Right d | Error e -> Either.Left e)
          results
      in
      match errors with
      | [] -> Lwt_result.return defs
      | _ ->
          let has_bad_request =
            List.exists (function `Bad_Request _ -> true | _ -> false) errors
          in
          let msg =
            errors
            |> List.map (function `Bad_Request m | `Not_Found m -> m)
            |> String.concat "; "
          in
          if has_bad_request then Lwt_result.fail (`Bad_Request msg)
          else Lwt_result.fail (`Not_Found msg))

(* 2. Helper to fetch the target Sample/Plate and check Auth *)
let resolve_bulk_csv_parent ~user_id ~user_role ~is_sample target_id =
  if is_sample then
    match%lwt Api_utils.find_sample_by_identifier target_id with
    | Error _ ->
        Lwt_result.fail
          (`Not_Found (Printf.sprintf "Sample %s not found" target_id))
    | Ok s ->
        let* () =
          Auth.check_project_access ~user_id ~user_role
            ~project_id:s.Core.Types.project_id
        in
        Lwt_result.return (Storage.Result_value.Sample s.Core.Types.id)
  else
    match%lwt Api_utils.find_plate_by_identifier target_id with
    | Error _ ->
        Lwt_result.fail
          (`Not_Found (Printf.sprintf "Plate %s not found" target_id))
    | Ok p ->
        let* () =
          Auth.check_project_access ~user_id ~user_role
            ~project_id:p.Core.Types.project_id
        in
        Lwt_result.return (Storage.Result_value.Plate p.Core.Types.id)

(* 3. Helpers to validate a single row against the definitions *)
let parse_bulk_csv_cell ~parent ~defs row_num col_i raw_val =
  match Core.String_utils.non_empty raw_val with
  | None -> None
  | Some val_str when col_i >= List.length defs ->
      Some
        (Error
           (`Bad_Request
              (Printf.sprintf "Row %d has more columns than the header" row_num)))
  | Some val_str -> (
      let (def : Core.Types.result_definition) = List.nth defs col_i in
      match Core.Result.payload_of_string def.data_type val_str with
      | Ok p -> Some (Ok (parent, def, Some p))
      | Error e ->
          Some
            (Error
               (`Bad_Request
                  (Printf.sprintf "Row %d, Col %d: %s" row_num (col_i + 2) e))))

let parse_bulk_csv_row ~parent_cache ~user_id ~user_role ~is_sample ~defs
    row_num target_id row_values =
  let%lwt parent_res =
    match Hashtbl.find_opt parent_cache target_id with
    | Some cached -> Lwt.return cached
    | None ->
        let%lwt resolved =
          resolve_bulk_csv_parent ~user_id ~user_role ~is_sample target_id
        in
        Hashtbl.replace parent_cache target_id resolved;
        Lwt.return resolved
  in
  match parent_res with
  | Error e -> Lwt_result.fail [ e ]
  | Ok parent -> (
      let errors, valid_ops =
        row_values
        |> List.mapi (parse_bulk_csv_cell ~parent ~defs row_num)
        |> List.filter_map Fun.id
        |> List.partition_map (function
          | Ok op -> Either.Right op
          | Error e -> Either.Left e)
      in
      match errors with
      | [] -> Lwt_result.return valid_ops
      | _ -> Lwt_result.fail errors)

let validate_bulk_csv_row ~parent_cache ~user_id ~user_role ~is_sample ~defs
    (row_num, row) =
  match row with
  | [] -> Lwt_result.return []
  | raw_target :: values -> (
      match Core.String_utils.non_empty raw_target with
      | None ->
          Lwt_result.fail
            [
              `Bad_Request
                (Printf.sprintf "Row %d: Missing target identifier" row_num);
            ]
      | Some target_id ->
          parse_bulk_csv_row ~parent_cache ~user_id ~user_role ~is_sample ~defs
            row_num target_id values)

let combine_batch_errors all_errors =
  let has_forbidden =
    List.exists (function `Forbidden _ -> true | _ -> false) all_errors
  in
  let has_not_found =
    List.exists (function `Not_Found _ -> true | _ -> false) all_errors
  in
  let msg =
    all_errors
    |> List.map (function
      | `Forbidden m | `Not_Found m | `Bad_Request m -> m
      | #Caqti_error.t as err -> "DB Error: " ^ Caqti_error.show err)
    |> String.concat "; "
  in
  if has_forbidden then `Forbidden msg
  else if has_not_found then `Not_Found msg
  else `Bad_Request msg

(* 4. The Orchestrator *)
let create_results_batch_csv ~user_id ~user_role body =
  let* csv_data =
    match Csv_utils.read_rows body with
    | Ok rows -> Lwt_result.return rows
    | Error msg -> Lwt_result.fail (`Bad_Request msg)
  in
  let* header, rows =
    match csv_data with
    | header :: (_ :: _ as rows) -> Lwt_result.return (header, rows)
    | _ ->
        Lwt_result.fail
          (`Bad_Request
             "CSV must contain a header row and at least one data row")
  in
  let* is_sample, def_headers =
    match header with
    | target_col :: def_headers -> (
        match String.trim target_col with
        | "sample_short_id" -> Lwt_result.return (true, def_headers)
        | "plate_short_id" -> Lwt_result.return (false, def_headers)
        | _ ->
            Lwt_result.fail
              (`Bad_Request
                 "First column header must be 'sample_short_id' or \
                  'plate_short_id'"))
    | [] ->
        Lwt_result.fail
          (`Bad_Request
             "First column header must be 'sample_short_id' or 'plate_short_id'")
  in
  let indexed_rows =
    List.mapi (fun i row -> (i + 2, row)) rows
    |> List.filter (fun (_, row) -> not (Csv_utils.is_blank_row row))
  in
  let* () =
    if indexed_rows = [] then
      Lwt_result.fail
        (`Bad_Request "CSV must contain a header row and at least one data row")
    else Lwt_result.return ()
  in
  let* defs = resolve_bulk_csv_headers def_headers in
  let parent_cache = Hashtbl.create (List.length indexed_rows) in
  let%lwt validation_results =
    Lwt_list.map_s
      (validate_bulk_csv_row ~parent_cache ~user_id ~user_role ~is_sample ~defs)
      indexed_rows
  in
  let error_lists, op_lists =
    List.partition_map
      (function Ok ops -> Either.Right ops | Error errs -> Either.Left errs)
      validation_results
  in
  match List.concat error_lists with
  | [] -> Storage.Result_value.add_many (List.concat op_lists)
  | all_errors -> Lwt_result.fail (combine_batch_errors all_errors)

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
