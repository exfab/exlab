(** * This module defines the API routes for managing External Database
    Definitions. * These definitions allow ExLab to link out to external
    resources like NCBI, * internal wikis, or other databases, providing a way
    to integrate with other * systems. * * This module handles the HTTP layer,
    translating requests and responses * between the client and the underlying
    storage and core logic layers. It * uses helper functions from the [Utils]
    module for common tasks like * parameter parsing and response serialization.
    * * Key functionalities include: * - Listing all external database
    definitions. * - Retrieving a single definition by its ID. * - Creating a
    new definition. * - Updating an existing definition. *)

module Storage = Exlab_storage
module Core = Exlab_core
module Types = Exlab_core.Types
module Api = Api_types
module Db = Exlab_storage.External_db_definition
open Lwt_result.Syntax

(** Handles `GET /api/v1/external-db-definitions`.

    Retrieves a list of all external database definitions.

    @return
      A JSON response containing a list of all definitions and a total count. *)
let get_all_external_db_definitions_handler _req =
  let result =
    let* definitions = Db.get_all () in
    let response =
      { Api.ExternalDb.data = definitions; count = List.length definitions }
    in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response ~serializer:Api.ExternalDb.yojson_of_list_response
    result

(** Handles `GET /api/v1/external-db-definitions/:id`.

    Retrieves a specific external database definition by its ID.

    @param id The integer ID of the definition.
    @return
      A JSON response containing the detailed information of the definition. *)
let get_external_db_definition_handler req =
  let result =
    let* id = Api_utils.get_int_param req "id" in
    let* definition_opt = Db.get_by_id id in
    match definition_opt with
    | Some definition -> Lwt.return (Ok definition)
    | None ->
        Lwt.return
          (Error
             (`Not_Found
                ("External DB Definition not found: " ^ string_of_int id)))
  in
  Api_utils.handle_response ~serializer:Types.yojson_of_external_db_definition
    result

(** Handles `POST /api/v1/external-db-definitions`.

    Creates a new external database definition. The request body must contain a
    name for the definition and a URL template.

    - **Body**: (json) The definition creation payload, defined by
      [Api.ExternalDb.create].

    @return
      A JSON response with the newly created definition and a [201 Created]
      status. *)
let create_external_db_definition_handler req =
  let%lwt body = Dream.body req in
  let result =
    try
      let json = Yojson.Safe.from_string body in
      let create_req = Api.ExternalDb.create_of_yojson json in
      Db.add ~name:create_req.name ~url_template:create_req.url_template
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid Payload: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON Error: " ^ msg)))
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Types.yojson_of_external_db_definition result

(** Handles `PUT /api/v1/external-db-definitions/:id`.

    Updates an existing external database definition.

    @param id
      The integer ID of the definition to update.
      - **Body**: (json) The definition update payload, defined by
        [Api.ExternalDb.update].
    @return A JSON response with the updated definition details. *)
let update_external_db_definition_handler req =
  let%lwt body = Dream.body req in
  let result =
    try
      let* id = Api_utils.get_int_param req "id" in
      let* definition_opt = Db.get_by_id id in
      match definition_opt with
      | None ->
          Lwt.return
            (Error (`Not_Found (Printf.sprintf "Definition %d not found" id)))
      | Some _ ->
          let json = Yojson.Safe.from_string body in
          let update_req = Api.ExternalDb.update_of_yojson json in
          Db.update ~id ~name:update_req.name
            ~url_template:update_req.url_template
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid Payload: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON Error: " ^ msg)))
  in
  Api_utils.handle_response ~serializer:Types.yojson_of_external_db_definition
    result

let routes =
  [
    Dream.get "/api/v1/external-db-definitions"
      get_all_external_db_definitions_handler;
    Dream.get "/api/v1/external-db-definitions/:id"
      get_external_db_definition_handler;
    Dream.post "/api/v1/external-db-definitions"
      create_external_db_definition_handler;
    Dream.put "/api/v1/external-db-definitions/:id"
      update_external_db_definition_handler;
  ]
