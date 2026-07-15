(** This module provides a suite of server-side utilities designed to streamline
    request and response handling within the Dream web framework. It
    standardizes common tasks such as parameter extraction, JSON body parsing,
    error handling, and resource lookups. By offering a consistent interface for
    these operations, it helps reduce boilerplate code and improve the
    robustness of API endpoints.

    Key features include:
    - {b Safe Parameter Extraction}: Functions for safely parsing integer
      parameters from URLs, with built-in error handling.
    - {b Uniform JSON Responses}: Helpers for generating consistent JSON error
      messages and handling successful responses.
    - {b Generic Resource Lookups}: A polymorphic function for finding resources
      by different types of identifiers (ID, UUID, Short ID), reducing code
      duplication.
    - {b Content Negotiation}: A utility for serving responses in either JSON or
      CSV format based on query parameters.

    The module leverages [Lwt_result] for monadic control flow, allowing for
    clean and composable error handling pipelines. *)

module Core = Exlab_core
module Storage = Exlab_storage
module StringSet = Set.Make (String)
open Lwt_result.Syntax

(** Wraps [Dream.json] to append a newline character to the JSON output.

    @param status The HTTP status code for the response (defaults to [200 OK]).
    @param body The JSON string to be sent in the response.
    @return A Dream response with the JSON body and a trailing newline. *)
let json_nl ?status body = Dream.json ?status (body ^ "\n")

(** Extracts a named integer parameter from the URL path. It safely attempts to
    convert the parameter to an integer and returns a [`Bad_Request] error if
    the parameter is missing or not a valid integer.

    @param request The Dream request object.
    @param param_name The name of the URL parameter (e.g., ":id").
    @return
      A [Lwt_result.t] containing either the integer value or a [`Bad_Request]
      error.

    {[
      match%lwt get_int_param request "user_id" with
      | Ok id -> ...
      | Error err -> ...
    ]} *)
let get_int_param request param_name =
  try Lwt.return (Ok (int_of_string (Dream.param request param_name)))
  with Failure _ ->
    Lwt.return
      (Error
         (`Bad_Request ("Invalid " ^ param_name ^ " in URL. Must be an integer.")))

(** Parses the HTTP request body as JSON and decodes it into a specified type.
    This function handles potential JSON parsing errors and Yojson conversion
    errors, returning a structured [Bad_Request] error if parsing fails.

    @param t_of_yojson
      The decoder function (e.g., [my_type_of_yojson]) to convert the JSON into
      the target OCaml type.
    @param request The Dream request object.
    @return
      A [Lwt_result.t] containing either the decoded value or a [`Bad_Request]
      error.

    {[
      let%lwt result =
      let%lwt parsed_body = parse_body_json user_of_yojson request in
      ...
      in
    ]} *)
let parse_body_json t_of_yojson request =
  let%lwt body = Dream.body request in
  try
    let json = Yojson.Safe.from_string body in
    Lwt.return (Ok (t_of_yojson json))
  with
  | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
      let msg = Printexc.to_string exn in
      Lwt.return (Error (`Bad_Request ("Invalid Payload: " ^ msg)))
  | Yojson.Json_error msg ->
      Lwt.return (Error (`Bad_Request ("JSON Error: " ^ msg)))

let parse_body_csv decoder request =
  let%lwt body = Dream.body request in
  try
    match Csv_utils.parse body decoder with
    | Ok items ->
        if items = [] then
          Lwt.return (Error (`Bad_Request "CSV contains no data rows"))
        else Lwt.return (Ok items)
    | Error msg -> Lwt.return (Error (`Bad_Request msg))
  with
  | Csv.Failure (line_n, _, msg) ->
      let error_msg =
        Printf.sprintf "CSV Syntax Error on line %d: %s" line_n msg
      in
      Lwt.return (Error (`Bad_Request error_msg))
  | exn ->
      let error_msg = "Parsing Error: " ^ Printexc.to_string exn in
      Lwt.return (Error (`Bad_Request error_msg))

(** Standardizes error responses into a uniform JSON format:
    [{"error": "message"}].

    @param error The error variant to be handled.
    @return A Dream response object with the appropriate status code. *)
let respond_with_error error =
  match error with
  | `Bad_Request msg ->
      let json = `Assoc [ ("error", `String msg) ] in
      json_nl ~status:`Bad_Request (Yojson.Safe.to_string json)
  | `Not_Found msg ->
      let json = `Assoc [ ("error", `String msg) ] in
      json_nl ~status:`Not_Found (Yojson.Safe.to_string json)
  | `Msg msg ->
      let json = `Assoc [ ("error", `String msg) ] in
      json_nl ~status:`Bad_Request (Yojson.Safe.to_string json)
  | `Unauthorized msg ->
      let json = `Assoc [ ("error", `String msg) ] in
      json_nl ~status:`Unauthorized (Yojson.Safe.to_string json)
  | `Forbidden msg ->
      let json = `Assoc [ ("error", `String msg) ] in
      json_nl ~status:`Forbidden (Yojson.Safe.to_string json)
  | `Internal_Server_Error msg ->
      let json = `Assoc [ ("error", `String msg) ] in
      json_nl ~status:`Internal_Server_Error (Yojson.Safe.to_string json)
  | #Caqti_error.t as err ->
      Dream.log "DB Error: %s" (Caqti_error.show err);
      json_nl ~status:`Internal_Server_Error
        {|{"error": "Internal Server Error"}|}

(** Handles results of type [unit Lwt_result.t]. On success ([Ok ()]), it
    returns an empty response with a [204 No Content] status, which is
    appropriate for operations like DELETE or updates that do not return data.
    On failure, it delegates to [respond_with_error].

    @param result_promise The [unit Lwt_result.t] to wait for and handle.
    @return A Dream response. *)
let handle_unit_result result_promise =
  match%lwt result_promise with
  | Ok () -> Dream.respond ~status:`No_Content ""
  | Error err -> respond_with_error err

(** A standard handler for processing a [Lwt_result.t] promise and returning a
    JSON response. If the promise resolves to [Ok data], the data is serialized
    to JSON. If it resolves to [Error err], the error is handled by
    [respond_with_error].

    @param ?status
      The HTTP status for a successful response (defaults to [`OK]).
    @param serializer
      A function that converts the data of type ['a] into [Yojson.Safe.t].
    @param result_promise
      A promise that will resolve to [('a, 'b) Lwt_result.t].
    @return A Dream response. *)
let handle_response ?(status = `OK) ~serializer result_promise =
  match%lwt result_promise with
  | Ok data ->
      let json = serializer data in
      json_nl ~status (Yojson.Safe.to_string json)
  | Error err -> respond_with_error err

(** Handles responses that can be returned as either JSON or CSV. It inspects
    the [?format=csv] query parameter to decide the output format. If the
    parameter is present, it uses the [csv_serializer] to generate a CSV file
    for download. Otherwise, it defaults to JSON output.

    @param status The HTTP status for a successful JSON response.
    @param request The Dream request object.
    @param filename
      The base name for the downloaded CSV file (e.g., "projects.csv").
    @param csv_serializer
      A function to convert the data of type ['a] into a raw CSV string.
    @param json_serializer
      A function to convert the data of type ['a] into [Yojson.Safe.t].
    @param result_promise
      A promise that will resolve to [('a, 'b) Lwt_result.t].
    @return A Dream response, either as JSON or a CSV file attachment. *)
let handle_response_with_export ?(status = `OK) ~request ~filename
    ~csv_serializer (* Function: 'a -> string *) ~json_serializer
    (* Function: 'a -> Yojson.Safe.t *) result_promise =
  match%lwt result_promise with
  | Ok data -> (
      (* Check the Query Parameter *)
      match Dream.query request "format" with
      | Some "csv" ->
          let csv_content = csv_serializer data in
          Dream.respond
            ~headers:
              [
                ("Content-Type", "text/csv");
                ( "Content-Disposition",
                  Printf.sprintf "attachment; filename=\"%s\"" filename );
              ]
            csv_content
      | _ ->
          (* Default to JSON *)
          let json = json_serializer data in
          json_nl ~status (Yojson.Safe.to_string json))
  (* If the database operation fails, return a JSON error even if CSV was requested. *)
  | Error err -> respond_with_error err

(** A generic dispatcher that standardizes resource lookup logic. It parses an
    [identifier] string to determine if it is an integer ID, a UUID, or a Short
    ID, and then calls the appropriate data access function. This allows API
    endpoints to fetch resources using any of these identifier types through a
    single, unified interface.

    @param identifier
      The raw string identifier from the URL (e.g., "101", "u-123", "99a8c...").
    @param resource_name
      The name of the resource, used for clear error messages (e.g., "Sample",
      "Project").
    @param get_by_id A function to fetch the resource by its integer ID.
    @param get_by_uid A function to fetch the resource by its UUID.
    @param get_by_short_id A function to fetch the resource by its Short ID.
    @return
      A [Lwt_result.t] containing either the resource or a [`Not_Found] error.
*)
let find_resource_by_identifier (identifier : string)
    ?(get_by_id : (int -> ('a option, 'e) Lwt_result.t) option)
    ?(get_by_uid : (string -> ('a option, 'e) Lwt_result.t) option)
    ?(get_by_short_id : (string -> ('a option, 'e) Lwt_result.t) option)
    (resource_name : string) =
  let id_type = Api_types.Identifier.of_string identifier in

  let resource_lwt_opt =
    match id_type with
    | Api_types.Identifier.Id id -> Option.map (fun f -> f id) get_by_id
    | Api_types.Identifier.Uuid uid -> Option.map (fun f -> f uid) get_by_uid
    | Api_types.Identifier.Short_id short_id ->
        Option.map (fun f -> f short_id) get_by_short_id
  in

  match resource_lwt_opt with
  | Some resource_lwt -> (
      let* resource_opt = resource_lwt in
      match resource_opt with
      | Some r -> Lwt.return (Ok r)
      | None ->
          let msg =
            Printf.sprintf "%s not found with identifier %s" resource_name
              identifier
          in
          Lwt.return (Error (`Not_Found msg)))
  | None ->
      let id_type_str =
        match id_type with
        | Id _ -> "ID"
        | Uuid _ -> "UID"
        | Short_id _ -> "Short ID"
      in
      let msg =
        Printf.sprintf "Lookup by %s is not supported for %s" id_type_str
          resource_name
      in
      Lwt.return (Error (`Bad_Request msg))

(** {2 Resource Lookups}
    Partially applied versions of [find_resource_by_identifier] for specific
    types. *)

(** Specialized lookup for Projects. See {!find_resource_by_identifier}. *)
let find_project_by_identifier identifier =
  find_resource_by_identifier identifier ~get_by_id:Storage.Project.get_by_id
    ~get_by_uid:Storage.Project.get_by_uid
    ~get_by_short_id:Storage.Project.get_by_short_id "Project"

(** Specialized lookup for Samples. See {!find_resource_by_identifier}. *)
let find_sample_by_identifier identifier =
  find_resource_by_identifier identifier ~get_by_id:Storage.Sample.get_by_id
    ~get_by_uid:Storage.Sample.get_by_uid
    ~get_by_short_id:Storage.Sample.get_by_short_id "Sample"

(** Specialized lookup for Products. See {!find_resource_by_identifier}. *)
let find_product_by_identifier identifier =
  find_resource_by_identifier identifier ~get_by_id:Storage.Product.get_by_id
    ~get_by_uid:Storage.Product.get_by_uid
    ~get_by_short_id:Storage.Product.get_by_short_id "Product"

(** Specialized lookup for Plates. See {!find_resource_by_identifier}. *)
let find_plate_by_identifier identifier =
  find_resource_by_identifier identifier ~get_by_id:Storage.Plate.get_by_id
    ~get_by_uid:Storage.Plate.get_by_uid
    ~get_by_short_id:Storage.Plate.get_by_short_id "Plate"

(** Specialized lookup for Result Definitions. See
    {!find_resource_by_identifier}. *)
let find_result_definition_by_identifier identifier =
  find_resource_by_identifier identifier
    ~get_by_id:Storage.Result_definition.get_by_id
    ~get_by_uid:Storage.Result_definition.get_by_uid
    ~get_by_short_id:Storage.Result_definition.get_by_short_id
    "Result Definition"

(** Specialized lookup for Result Categories. See
    {!find_resource_by_identifier}. *)
let find_result_category_by_identifier identifier =
  find_resource_by_identifier identifier
    ~get_by_id:Storage.Result_category.get_by_id
    ~get_by_uid:Storage.Result_category.get_by_uid "Result Category"

(** Specialized lookup for Strain. See {!find_resource_by_identifier}. *)
let find_strain_by_identifier identifier =
  find_resource_by_identifier identifier ~get_by_id:Storage.Strain.get_by_id
    ~get_by_uid:Storage.Strain.get_by_uid "Strain"

(** Specialized lookup for Communities. See {!find_resource_by_identifier}. *)
let find_community_by_identifier identifier =
  find_resource_by_identifier identifier ~get_by_id:Storage.Community.get_by_id
    ~get_by_uid:Storage.Community.get_by_uid "Community"

(** Specialized lookup for Users. See {!find_resource_by_identifier}. *)
let find_user_by_identifier identifier =
  find_resource_by_identifier identifier ~get_by_id:Storage.User.get_by_id
    ~get_by_short_id:Storage.User.get_by_email "User"
