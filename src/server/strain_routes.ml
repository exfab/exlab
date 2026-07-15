(** * This module defines the API routes for managing Strains. * It provides
    endpoints for creating, retrieving, updating, and deleting * Strains,
    forming a complete CRUD interface. * * This module handles the HTTP layer,
    translating requests and responses * between the client and the underlying
    storage and core logic layers. It * uses helper functions from the [Utils]
    module for common tasks like * parameter parsing and response serialization.
    * * Key functionalities include: * - Listing all strains. * - Creating a new
    strain. * - Retrieving a single strain by its ID, including its external
    database links. * - Updating an existing strain. *)

module Storage = Exlab_storage
module Core = Exlab_core
module Api = Api_types
open Lwt_result.Syntax

let create_strains_batch (items : Api_types.Strain.create list) =
  let process_single_item (item : Api_types.Strain.create) =
    let* new_strain =
      Storage.Strain.add ~genus:item.genus ~species:item.species
        ~strain_name:item.strain_name ~genotype:item.genotype
        ~parent_strain_id:item.parent_strain_id ~notes:item.notes
    in

    let* _ =
      Lwt_list.map_p
        (fun (link : Api_types.Strain.link_request) ->
          Storage.Strain_external_link.add ~strain_id:new_strain.id
            ~external_db_definition_id:link.external_db_definition_id
            ~value:link.value)
        item.links
      |> Lwt.map (fun _ -> Ok ())
    in

    Lwt.return (Ok new_strain)
  in

  let rec map_sequentially acc = function
    | [] -> Lwt.return (Ok (List.rev acc))
    | item :: rest -> (
        match%lwt process_single_item item with
        | Ok strain -> map_sequentially (strain :: acc) rest
        | Error e -> Lwt.return (Error e))
  in

  map_sequentially [] items

(** Handles `GET /api/v1/strains`.

    Retrieves a list of all strains. Optional `search` query parameter filters
    the results.

    @return A JSON response containing a list of all strains and a total count.
*)
let get_all_strains_handler request =
  let search_term_opt = Dream.query request "search" in
  let genus_filter = Dream.query request "genus" in
  let species_filter = Dream.query request "species" in

  let has_filters =
    Option.is_some search_term_opt
    || Option.is_some genus_filter
    || Option.is_some species_filter
  in

  let result =
    if has_filters then
      let term = Option.value ~default:"" search_term_opt in
      let filters : Storage.Strain.strain_filters =
        {
          filter_term = term;
          filter_genus = genus_filter;
          filter_species = species_filter;
        }
      in
      Storage.Strain.search_with_filters filters
    else Storage.Strain.get_all ()
  in
  Api_utils.handle_response_with_export ~request ~filename:"all_strains.csv"
    ~csv_serializer:Core.Export_data.strains_to_csv
    ~json_serializer:(fun strains ->
      let response =
        { Api.Strain.data = strains; count = List.length strains }
      in
      Api.Strain.yojson_of_list_response response)
    result

(** Handles `GET /api/v1/strains/:identifier`.

    Retrieves a specific strain by its ID. The response includes detailed
    information about the strain and any associated external database links.

    @param identifier The ID or UUID of the strain.
    @return A detailed JSON object for the strain. *)
let get_strain_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* strain = Api_utils.find_strain_by_identifier identifier in
    let raw_links = Option.value strain.external_links ~default:[] in

    let* detailed_links =
      Lwt_list.map_p
        (fun (link : Core.Types.strain_external_link) ->
          let* def_opt =
            Storage.External_db_definition.get_by_id
              link.external_db_definition_id
          in

          match def_opt with
          | None -> Lwt.return (Ok None)
          | Some def ->
              let url = Core.Strain_link.resolve_url def link in

              Lwt.return
                (Ok
                   (Some
                      {
                        Api.StrainLink.id = link.id;
                        external_db_definition_id = def.id;
                        db_name = def.name;
                        value = link.value;
                        resolved_url = url;
                      })))
        raw_links
      |> Lwt.map (fun list_of_results ->
          let clean_list =
            List.filter_map
              (function
                | Ok (Some item) -> Some item
                | Ok None -> None
                | Error _ -> None)
              list_of_results
          in
          Ok clean_list)
    in

    let* source_samples_with_projects =
      Storage.Sample.get_source_with_project_by_strain_id strain.id
    in
    let source_samples =
      List.map
        (fun (sample, project) -> { Api.Strain.sample; project })
        source_samples_with_projects
    in

    let response =
      { Api.Strain.strain; external_links = detailed_links; source_samples }
    in

    Lwt.return (Ok response)
  in
  Api_utils.handle_response ~serializer:Api.Strain.yojson_of_detailed result

(** Handles `POST /api/v1/strains`.

    Creates a new strain.

    - **Body**: (json) The strain creation payload, defined by
      [Api_types.Strain.create].

    @return
      A JSON response with the newly created strain and a [201 Created] status.
*)
let create_strain_handler request =
  let result =
    let* req = Api_utils.parse_body_json Api.Strain.create_of_yojson request in

    match
      Core.Strain.validate_creation ~genus:req.genus ~species:req.species
        ~strain_name:req.strain_name ~genotype:req.genotype
        ~parent_strain_id:req.parent_strain_id
    with
    | Error msg -> Lwt.return (Error (`Bad_Request msg))
    | Ok (clean_genus, clean_species, clean_nm, clean_geno, valid_pid) ->
        let* new_strain =
          Storage.Strain.add ~genus:clean_genus ~species:clean_species
            ~strain_name:clean_nm ~genotype:clean_geno
            ~parent_strain_id:valid_pid ~notes:req.notes
        in

        let* _ =
          Lwt_list.map_p
            (fun (link_req : Api.Strain.link_request) ->
              Storage.Strain_external_link.add ~strain_id:new_strain.id
                ~external_db_definition_id:link_req.external_db_definition_id
                ~value:link_req.value)
            req.links
          |> Lwt.map (fun _ -> Ok ())
        in
        Lwt.return (Ok new_strain)
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_strain result

(** Handles `PUT /api/v1/strains/:identifier`.

    Updates an existing strain.

    @param identifier
      The ID or uuid of the strain to update.
      - **Body**: (json) The strain update payload, defined by
        [Api_types.Strain.update].
    @return A JSON response with the updated strain details. *)
let update_strain_handler request =
  let%lwt body = Dream.body request in

  let result =
    try
      let identifier = Dream.param request "identifier" in
      let* strain = Api_utils.find_strain_by_identifier identifier in

      let json = Yojson.Safe.from_string body in
      let req = Api.Strain.update_of_yojson json in

      Storage.Strain.update ~id:strain.id ~genus:req.genus ~species:req.species
        ~strain_name:req.strain_name ~genotype:req.genotype
        ~parent_strain_id:req.parent_strain_id ~notes:req.notes
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid payload: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON parsing error: " ^ msg)))
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_strain result

(** Handles `POST /api/v1/strains/bulk`.

    Creates multiple strains in a single JSON request.

    - **Body**: (json) The bulk strain creation payload.

    @return A JSON response containing the list of newly created strains. *)
let bulk_create_strains_handler request =
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.Strain.bulk_create_request_of_yojson
        request
    in
    create_strains_batch req.strains
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:(fun list ->
      `Assoc [ ("strains", `List (List.map Core.Types.yojson_of_strain list)) ])
    result

(** Handles `POST /api/v1/strains/bulk-csv`.

    Creates multiple strains from a CSV payload.

    - **Body**: (csv) The bulk strain creation payload.

    @return A JSON response containing the list of newly created strains. *)
let bulk_csv_create_strains_handler request =
  let result =
    let* create_items =
      Api_utils.parse_body_csv Decoders.strain_create request
    in
    create_strains_batch create_items
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:(fun list ->
      `Assoc [ ("strains", `List (List.map Core.Types.yojson_of_strain list)) ])
    result

let routes =
  [
    Dream.get "/api/v1/strains" get_all_strains_handler;
    Dream.get "/api/v1/strains/:identifier" get_strain_handler;
    Dream.post "/api/v1/strains" create_strain_handler;
    Dream.put "/api/v1/strains/:identifier" update_strain_handler;
    Dream.post "/api/v1/strains/bulk" bulk_create_strains_handler;
    Dream.post "/api/v1/strains/bulk-csv" bulk_csv_create_strains_handler;
  ]
