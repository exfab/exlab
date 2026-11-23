(* CSV Decoding functions for data types *)

module S = Exlab_core.String_utils

let sample_create (get : Csv_utils.row_accessor) :
    (Api_types.Sample.create, string) result =
  let open Api_types.Sample in
  match S.non_empty (get "sample_type") with
  | None -> Error "Sample type cannot be empty"
  | Some sample_type ->
      Ok
        {
          sample_type;
          category = S.non_empty (get "category");
          parent_sample_id = S.to_int_option (get "parent_sample_id");
          parent_sample_short_id = S.non_empty (get "parent_sample_short_id");
          strain_id = S.to_int_option (get "strain_id");
          community_id = S.to_int_option (get "community_id");
          result_definition_ids =
            S.to_int_list ~sep:',' (get "result_definition_ids");
          genus = S.non_empty (get "genus");
          species = S.non_empty (get "species");
          strain_name = S.non_empty (get "strain_name");
          genotype = S.non_empty (get "genotype");
        }

let product_create (get : Csv_utils.row_accessor) :
    (Api_types.Product.create, string) result =
  let open Api_types.Product in
  let raw_part_number = S.non_empty (get "manufacturer_part_number") in

  match
    Exlab_core.Product.validate_creation ~name:(get "name")
      ~manufacturer_part_number:raw_part_number
  with
  | Ok (clean_name, clean_part_number) ->
      Ok
        {
          name = clean_name;
          brand = S.non_empty (get "brand");
          manufacturer_part_number = clean_part_number;
          description = S.non_empty (get "description");
        }
  | Error msg -> Error msg

let strain_create (get : Csv_utils.row_accessor) :
    (Api_types.Strain.create, string) result =
  let open Api_types.Strain in
  match
    ( S.non_empty (get "genus"),
      S.non_empty (get "species"),
      S.non_empty (get "strain_name") )
  with
  | None, _, _ -> Error "Strain genus cannot be empty"
  | _, None, _ -> Error "Strain species cannot be empty"
  | _, _, None -> Error "Strain name cannot be empty"
  | Some genus, Some species, Some strain_name ->
      Ok
        {
          genus;
          species;
          strain_name;
          domain = S.non_empty (get "domain");
          taxonomic_group = S.non_empty (get "taxonomic_group");
          genotype = S.non_empty (get "genotype");
          parent_strain_id = S.to_int_option (get "parent_strain_id");
          notes = S.non_empty (get "notes");
          links = [];
        }

let strain_patch (get : Csv_utils.row_accessor) :
    (Api_types.Strain.bulk_patch_item, string) result =
  let identifier_opt =
    match S.non_empty (get "id") with
    | Some _ as id -> id
    | None -> S.non_empty (get "uid")
  in
  match identifier_opt with
  | None -> Error "Missing required column: 'id' or 'uid'"
  | Some identifier ->
      Ok
        {
          Api_types.Strain.identifier;
          genus = S.non_empty (get "genus");
          species = S.non_empty (get "species");
          strain_name = S.non_empty (get "strain_name");
          domain = S.non_empty (get "domain");
          taxonomic_group = S.non_empty (get "taxonomic_group");
          genotype = S.non_empty (get "genotype");
          parent_strain_id = S.to_int_option (get "parent_strain_id");
          notes = S.non_empty (get "notes");
        }

let well_layout_item (get : Csv_utils.row_accessor) :
    (Api_types.Plate.well_layout_item, string) result =
  let open Api_types.Plate in
  match (S.non_empty (get "well"), S.non_empty (get "sample_short_id")) with
  | None, _ -> Error "Well identifier cannot be empty"
  | _, None -> Error "Sample short_id cannot be empty"
  | Some well, Some sample_short_id -> Ok { well; sample_short_id }

let bulk_plate_action_item (get : Csv_utils.row_accessor) :
    (Api_types.Plate.bulk_action_item, string) result =
  match
    ( S.non_empty (get "plate_name"),
      S.non_empty (get "well"),
      S.non_empty (get "sample_short_id") )
  with
  | None, _, _ -> Error "plate_name cannot be empty"
  | Some plate_name, None, None ->
      Ok (Api_types.Plate.Create_blank { plate_name })
  | _, None, _ -> Error "Well identifier cannot be empty if sample is provided"
  | _, _, None -> Error "Sample short_id cannot be empty if well is provided"
  | Some plate_name, Some well, Some sample_short_id ->
      Ok
        (Api_types.Plate.Create_with_layout
           { plate_name; well; sample_short_id })

let bulk_plate_layout_item (get : Csv_utils.row_accessor) :
    (Api_types.Plate.bulk_layout_item, string) result =
  let open Api_types.Plate in
  let plate_id_opt =
    match S.non_empty (get "plate_name") with
    | Some _ as name -> name
    | None -> S.non_empty (get "plate_short_id")
  in
  match
    (plate_id_opt, S.non_empty (get "well"), S.non_empty (get "sample_short_id"))
  with
  | None, _, _ ->
      Error "Missing required column: 'plate_name' or 'plate_short_id'"
  | _, None, _ -> Error "Well identifier cannot be empty"
  | _, _, None -> Error "Sample short_id cannot be empty"
  | Some plate_name, Some well, Some sample_short_id ->
      Ok { plate_name; well; sample_short_id }
