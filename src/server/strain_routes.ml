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

let load_taxonomic_groups () =
  let* setting_opt = Storage.Setting.get_by_key "strain_taxonomic_groups" in
  let groups =
    match setting_opt with
    | Some { Core.Types.value = `List items; _ } ->
        let parsed =
          List.filter_map
            (function
              | `String s ->
                  let t = String.trim s in
                  if t = "" then None else Some t
              | _ -> None)
            items
        in
        if parsed = [] then Core.Strain.default_taxonomic_groups else parsed
    | _ -> Core.Strain.default_taxonomic_groups
  in
  Lwt_result.return groups

let create_strains_batch (items : Api_types.Strain.create list) =
  let* allowed_groups = load_taxonomic_groups () in
  let validate_item (item : Api_types.Strain.create) =
    match
      Core.Strain.validate_creation ~genus:item.genus ~species:item.species
        ~strain_name:item.strain_name ~genotype:item.genotype
        ~parent_strain_id:item.parent_strain_id
    with
    | Error msg -> Error (`Bad_Request msg)
    | Ok (clean_genus, clean_species, clean_nm, clean_geno, valid_pid) -> (
        match
          Core.Strain.validate_classification ~allowed_groups
            ~domain:item.domain ~taxonomic_group:item.taxonomic_group
        with
        | Error msg -> Error (`Bad_Request msg)
        | Ok (domain, taxonomic_group) ->
            Ok
              ( clean_genus,
                clean_species,
                clean_nm,
                domain,
                taxonomic_group,
                clean_geno,
                valid_pid,
                item.notes,
                item.links ))
  in
  let rec validate_all acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest -> (
        match validate_item item with
        | Ok validated -> validate_all (validated :: acc) rest
        | Error _ as err -> err)
  in
  let* validated_items = Lwt.return (validate_all [] items) in

  let process_single_item
      ( clean_genus,
        clean_species,
        clean_nm,
        domain,
        taxonomic_group,
        clean_geno,
        valid_pid,
        notes,
        links ) =
    let* new_strain =
      Storage.Strain.add ~genus:clean_genus ~species:clean_species
        ~strain_name:clean_nm ~domain ?taxonomic_group ~genotype:clean_geno
        ~parent_strain_id:valid_pid ~notes ()
    in

    let* _ =
      Lwt_list.map_p
        (fun (link : Api_types.Strain.link_request) ->
          Storage.Strain_external_link.add ~strain_id:new_strain.id
            ~external_db_definition_id:link.external_db_definition_id
            ~value:link.value)
        links
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

  map_sequentially [] validated_items

(** Handles `GET /api/v1/strains`.

    Retrieves a list of all strains. Optional `search`, `genus`, `species`,
    `domain`, and `taxonomic_group` query parameters filter the results.

    @return A JSON response containing a list of all strains and a total count.
*)
let get_all_strains_handler request =
  let non_empty_query name =
    match Dream.query request name with
    | Some s when String.trim s <> "" -> Some (String.trim s)
    | _ -> None
  in
  let search_term_opt = Dream.query request "search" in
  let genus_filter = non_empty_query "genus" in
  let species_filter = non_empty_query "species" in
  let raw_domain_filter = non_empty_query "domain" in
  let taxonomic_group_filter = non_empty_query "taxonomic_group" in

  let has_filters =
    Option.is_some search_term_opt
    || Option.is_some genus_filter
    || Option.is_some species_filter
    || Option.is_some raw_domain_filter
    || Option.is_some taxonomic_group_filter
  in

  let result =
    if has_filters then
      let* domain_filter =
        match raw_domain_filter with
        | None -> Lwt_result.return None
        | Some raw -> (
            match Core.Types.strain_domain_of_string raw with
            | Ok d ->
                Lwt_result.return (Some (Core.Types.string_of_strain_domain d))
            | Error msg -> Lwt_result.fail (`Bad_Request msg))
      in
      let term = Option.value ~default:"" search_term_opt in
      let filters : Storage.Strain.strain_filters =
        {
          filter_term = term;
          filter_genus = genus_filter;
          filter_species = species_filter;
          filter_domain = domain_filter;
          filter_taxonomic_group = taxonomic_group_filter;
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
    let* allowed_groups = load_taxonomic_groups () in

    match
      Core.Strain.validate_creation ~genus:req.genus ~species:req.species
        ~strain_name:req.strain_name ~genotype:req.genotype
        ~parent_strain_id:req.parent_strain_id
    with
    | Error msg -> Lwt.return (Error (`Bad_Request msg))
    | Ok (clean_genus, clean_species, clean_nm, clean_geno, valid_pid) -> (
        match
          Core.Strain.validate_classification ~allowed_groups ~domain:req.domain
            ~taxonomic_group:req.taxonomic_group
        with
        | Error msg -> Lwt_result.fail (`Bad_Request msg)
        | Ok (domain, taxonomic_group) ->
            let* new_strain =
              Storage.Strain.add ~genus:clean_genus ~species:clean_species
                ~strain_name:clean_nm ~domain ?taxonomic_group
                ~genotype:clean_geno ~parent_strain_id:valid_pid
                ~notes:req.notes ()
            in

            let* _ =
              Lwt_list.map_p
                (fun (link_req : Api.Strain.link_request) ->
                  Storage.Strain_external_link.add ~strain_id:new_strain.id
                    ~external_db_definition_id:
                      link_req.external_db_definition_id ~value:link_req.value)
                req.links
              |> Lwt.map (fun _ -> Ok ())
            in
            Lwt.return (Ok new_strain))
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
  let result =
    let identifier = Dream.param request "identifier" in
    let* strain = Api_utils.find_strain_by_identifier identifier in
    let* req = Api_utils.parse_body_json Api.Strain.update_of_yojson request in
    let* allowed_groups = load_taxonomic_groups () in
    let effective_groups =
      match strain.taxonomic_group with
      | Some existing
        when not
               (List.exists
                  (fun g ->
                    String.lowercase_ascii g = String.lowercase_ascii existing)
                  allowed_groups) ->
          allowed_groups @ [ existing ]
      | _ -> allowed_groups
    in
    let* clean_genus, clean_species, clean_nm, clean_geno, valid_pid =
      match
        Core.Strain.validate_creation ~genus:req.genus ~species:req.species
          ~strain_name:req.strain_name ~genotype:req.genotype
          ~parent_strain_id:req.parent_strain_id
      with
      | Ok tuple -> Lwt_result.return tuple
      | Error msg -> Lwt_result.fail (`Bad_Request msg)
    in
    let* domain =
      match req.domain with
      | None -> Lwt_result.return strain.domain
      | Some raw -> (
          match Core.Types.strain_domain_of_string raw with
          | Ok d -> Lwt_result.return d
          | Error msg -> Lwt_result.fail (`Bad_Request msg))
    in
    let* taxonomic_group =
      match
        Core.Strain.validate_taxonomic_group ~allowed_groups:effective_groups
          req.taxonomic_group
      with
      | Ok grp -> Lwt_result.return grp
      | Error msg -> Lwt_result.fail (`Bad_Request msg)
    in
    Storage.Strain.update ~id:strain.id ~genus:clean_genus
      ~species:clean_species ~strain_name:clean_nm ~domain ~taxonomic_group
      ~genotype:clean_geno ~parent_strain_id:valid_pid ~notes:req.notes
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

(** Handles `PATCH /api/v1/strains/bulk-csv`.

    Partially updates existing strains identified by `id` or `uid` from a CSV
    payload. Omitted or blank columns preserve each strain's existing value. All
    rows are validated before committing atomically in a single transaction. *)
let bulk_csv_patch_strains_handler request =
  let result =
    let* patch_items = Api_utils.parse_body_csv Decoders.strain_patch request in
    let* allowed_groups = load_taxonomic_groups () in
    let rec prepare acc = function
      | [] -> Lwt_result.return (List.rev acc)
      | (item : Api.Strain.bulk_patch_item) :: rest ->
          let* existing = Api_utils.find_strain_by_identifier item.identifier in
          let raw_genus = Option.value ~default:existing.genus item.genus in
          let raw_species =
            Option.value ~default:existing.species item.species
          in
          let raw_strain_name =
            Option.value ~default:existing.strain_name item.strain_name
          in
          let raw_genotype =
            match item.genotype with
            | Some _ -> item.genotype
            | None -> existing.genotype
          in
          let raw_parent =
            match item.parent_strain_id with
            | Some _ -> item.parent_strain_id
            | None -> existing.parent_strain_id
          in
          let raw_notes =
            match item.notes with
            | Some _ -> item.notes
            | None -> existing.notes
          in
          let* clean_genus, clean_species, clean_nm, clean_geno, valid_pid =
            match
              Core.Strain.validate_creation ~genus:raw_genus
                ~species:raw_species ~strain_name:raw_strain_name
                ~genotype:raw_genotype ~parent_strain_id:raw_parent
            with
            | Ok tuple -> Lwt_result.return tuple
            | Error msg -> Lwt_result.fail (`Bad_Request msg)
          in
          let* domain =
            match item.domain with
            | None -> Lwt_result.return existing.domain
            | Some raw -> (
                match Core.Types.strain_domain_of_string raw with
                | Ok d -> Lwt_result.return d
                | Error msg -> Lwt_result.fail (`Bad_Request msg))
          in
          let* taxonomic_group =
            match item.taxonomic_group with
            | None -> Lwt_result.return existing.taxonomic_group
            | Some _ as raw -> (
                match
                  Core.Strain.validate_taxonomic_group ~allowed_groups raw
                with
                | Ok grp -> Lwt_result.return grp
                | Error msg -> Lwt_result.fail (`Bad_Request msg))
          in
          let update_item : Storage.Strain.update_strain =
            {
              id = existing.id;
              genus = clean_genus;
              species = clean_species;
              strain_name = clean_nm;
              domain;
              taxonomic_group;
              genotype = clean_geno;
              parent_strain_id = valid_pid;
              notes = raw_notes;
            }
          in
          prepare (update_item :: acc) rest
    in
    let* updates = prepare [] patch_items in
    Storage.Strain.update_many updates
  in
  Api_utils.handle_response
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
    Dream.patch "/api/v1/strains/bulk-csv" bulk_csv_patch_strains_handler;
  ]
