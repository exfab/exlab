(** * This module defines the API routes for managing links between Strains and
    * External Databases. These links allow users to associate a specific strain
    * with an entry in an external resource, such as a sequence in GenBank or a
    * record in a private database. * * This module handles the HTTP layer,
    translating requests and responses * between the client and the underlying
    storage and core logic layers. It * uses helper functions from the [Utils]
    module for common tasks like * parameter parsing and response serialization.
    * * Key functionalities include: * - Listing all strain-to-database links. *
    \- Retrieving a single link by its ID. * - Creating a new link. * - Updating
    an existing link. *)

module Storage = Exlab_storage
module Core = Exlab_core
module Types = Exlab_core.Types
module Api = Api_types
module Db = Exlab_storage.Strain_external_link
open Lwt_result.Syntax

(** Handles `GET /api/v1/strain-external-links`.

    Retrieves a list of all links between strains and external databases.

    @return A JSON response containing a list of all links and a total count. *)
let get_all_strain_external_links_handler _req =
  let result =
    let* links = Db.get_all () in
    let response = { Api.StrainLink.data = links; count = List.length links } in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response ~serializer:Api.StrainLink.yojson_of_list_response
    result

(** Handles `GET /api/v1/strain-external-links/:id`.

    Retrieves a specific strain-to-database link by its ID.

    @param id The integer ID of the link.
    @return A JSON response containing the detailed information of the link. *)
let get_strain_external_link_handler req =
  let result =
    let* id = Api_utils.get_int_param req "id" in
    let* link_opt = Db.get_by_id id in
    match link_opt with
    | Some link -> Lwt.return (Ok link)
    | None ->
        Lwt.return
          (Error
             (`Not_Found ("Strain External Link not found: " ^ string_of_int id)))
  in
  Api_utils.handle_response ~serializer:Types.yojson_of_strain_external_link
    result

(** Handles `POST /api/v1/strain-external-links`.

    Creates a new link between a strain and an external database entry.

    - **Body**: (json) The link creation payload, defined by
      [Api.StrainLink.create].

    @return
      A JSON response with the newly created link and a [201 Created] status. *)
let create_strain_external_link_handler req =
  let%lwt body = Dream.body req in
  let result =
    try
      let json = Yojson.Safe.from_string body in
      let create_req = Api.StrainLink.create_of_yojson json in
      Db.add ~strain_id:create_req.strain_id
        ~external_db_definition_id:create_req.external_db_definition_id
        ~value:create_req.value
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid Payload: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON Error: " ^ msg)))
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Types.yojson_of_strain_external_link result

(** Handles `PUT /api/v1/strain-external-links/:id`.

    Updates an existing link between a strain and an external database.

    @param id
      The integer ID of the link to update.
      - **Body**: (json) The link update payload, defined by
        [Api.StrainLink.update].
    @return A JSON response with the updated link details. *)
let update_strain_external_link_handler req =
  let%lwt body = Dream.body req in
  let result =
    try
      let* id = Api_utils.get_int_param req "id" in
      let json = Yojson.Safe.from_string body in
      let update_req = Api.StrainLink.update_of_yojson json in

      let* existing_link = Db.get_by_id id in
      match existing_link with
      | None -> Lwt.return (Error (`Not_Found "Link not found"))
      | Some current ->
          Db.update ~id ~strain_id:current.strain_id
            ~external_db_definition_id:
              (Option.value update_req.external_db_definition_id
                 ~default:current.external_db_definition_id)
            ~value:(Option.value update_req.value ~default:current.value)
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid Payload: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON Error: " ^ msg)))
  in
  Api_utils.handle_response ~serializer:Types.yojson_of_strain_external_link
    result

(** Handles `DELETE /api/v1/strain-external-links/:id`.

    Removes a link permanently.

    @param id The integer ID of the link to delete.
    @return An empty response with a [204 No Content] status on success. *)
let delete_strain_external_link_handler req =
  let result =
    let* id = Api_utils.get_int_param req "id" in
    Db.delete id
  in
  Api_utils.handle_unit_result result

let routes =
  [
    Dream.get "/api/v1/strain-external-links"
      get_all_strain_external_links_handler;
    Dream.get "/api/v1/strain-external-links/:id"
      get_strain_external_link_handler;
    Dream.post "/api/v1/strain-external-links"
      create_strain_external_link_handler;
    Dream.put "/api/v1/strain-external-links/:id"
      update_strain_external_link_handler;
    Dream.delete "/api/v1/strain-external-links/:id"
      delete_strain_external_link_handler;
  ]
