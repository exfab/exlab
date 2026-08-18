(* CSV Decoding functions for data types *)

let sample_create (get : Csv_utils.row_accessor) :
    (Api_types.Sample.create, string) result =
  let open Api_types.Sample in
  let sample_type = Csv_utils.to_string (get "sample_type") in
  match sample_type with
  | "" -> Error "Sample type cannot be empty"
  | _ ->
      Ok
        {
          sample_type;
          category = Csv_utils.to_string_option (get "category");
          parent_sample_id = Csv_utils.to_int_option (get "parent_sample_id");
          parent_sample_short_id =
            Csv_utils.to_string_option (get "parent_sample_short_id");
          strain_id = Csv_utils.to_int_option (get "strain_id");
          community_id = Csv_utils.to_int_option (get "community_id");
          result_definition_ids =
            Csv_utils.to_int_list ~sep:',' (get "result_definition_ids");
          genus = Csv_utils.to_string_option (get "genus");
          species = Csv_utils.to_string_option (get "species");
          strain_name = Csv_utils.to_string_option (get "strain_name");
          genotype = Csv_utils.to_string_option (get "genotype");
        }

let product_create (get : Csv_utils.row_accessor) :
    (Api_types.Product.create, string) result =
  let open Api_types.Product in
  let raw_name = Csv_utils.to_string (get "name") in
  let raw_part_number =
    Csv_utils.to_string_option (get "manufacturer_part_number")
  in

  match
    Exlab_core.Product.validate_creation ~name:raw_name
      ~manufacturer_part_number:raw_part_number
  with
  | Ok (clean_name, clean_part_number) ->
      Ok
        {
          name = clean_name;
          brand = Csv_utils.to_string_option (get "brand");
          manufacturer_part_number = clean_part_number;
          description = Csv_utils.to_string_option (get "description");
        }
  | Error msg -> Error msg

let strain_create (get : Csv_utils.row_accessor) :
    (Api_types.Strain.create, string) result =
  let open Api_types.Strain in
  let genus = Csv_utils.to_string (get "genus") in
  let species = Csv_utils.to_string (get "species") in
  let strain_name = Csv_utils.to_string (get "strain_name") in

  match (genus, species, strain_name) with
  | "", _, _ -> Error "Strain genus cannot be empty"
  | _, "", _ -> Error "Strain species cannot be empty"
  | _, _, "" -> Error "Strain name cannot be empty"
  | _ ->
      Ok
        {
          genus;
          species;
          strain_name;
          genotype = Csv_utils.to_string_option (get "genotype");
          parent_strain_id = Csv_utils.to_int_option (get "parent_strain_id");
          notes = Csv_utils.to_string_option (get "notes");
          links = [];
        }

let well_layout_item (get : Csv_utils.row_accessor) :
    (Api_types.Plate.well_layout_item, string) result =
  let open Api_types.Plate in
  let well = Csv_utils.to_string (get "well") in
  let sample_short_id = Csv_utils.to_string (get "sample_short_id") in

  match (well, sample_short_id) with
  | "", _ -> Error "Well identifier cannot be empty"
  | _, "" -> Error "Sample short_id cannot be empty"
  | _ -> Ok { well; sample_short_id }

let bulk_plate_action_item (get : Csv_utils.row_accessor) :
    (Api_types.Plate.bulk_action_item, string) result =
  let plate_name = Csv_utils.to_string (get "plate_name") in
  let well = Csv_utils.to_string (get "well") in
  let sample_short_id = Csv_utils.to_string (get "sample_short_id") in

  match (plate_name, well, sample_short_id) with
  | "", _, _ -> Error "plate_name cannot be empty"
  | name, "", "" -> Ok (Api_types.Plate.Create_blank { plate_name = name })
  | _, "", _ -> Error "Well identifier cannot be empty if sample is provided"
  | _, _, "" -> Error "Sample short_id cannot be empty if well is provided"
  | name, w, s ->
      Ok
        (Api_types.Plate.Create_with_layout
           { plate_name = name; well = w; sample_short_id = s })

let bulk_plate_layout_item (get : Csv_utils.row_accessor) :
    (Api_types.Plate.bulk_layout_item, string) result =
  let open Api_types.Plate in
  let plate_name = Csv_utils.to_string (get "plate_name") in
  let well = Csv_utils.to_string (get "well") in
  let sample_short_id = Csv_utils.to_string (get "sample_short_id") in

  match (plate_name, well, sample_short_id) with
  | "", _, _ -> Error "plate_name cannot be empty"
  | _, "", _ -> Error "Well identifier cannot be empty"
  | _, _, "" -> Error "Sample short_id cannot be empty"
  | _ -> Ok { plate_name; well; sample_short_id }
