open Exlab_server
open Decoders
open Alcotest

let test_sample_create () =
  let get =
    Test_helpers.mock_row
      [
        ("name", "test_sample");
        ("sample_type", "test_type");
        ("category", "test_category");
        ("parent_sample_id", "123");
        ("strain_id", "456");
        ("result_definition_ids", "1,2,3");
      ]
  in

  let expected : (Api_types.Sample.create, string) result =
    Ok
      {
        sample_type = "test_type";
        category = Some "test_category";
        parent_sample_id = Some 123;
        parent_sample_short_id = None;
        strain_id = Some 456;
        community_id = None;
        result_definition_ids = [ 1; 2; 3 ];
        genus = None;
        species = None;
        strain_name = None;
        genotype = None;
      }
  in
  let result = sample_create get in
  let sample_create_testable =
    testable Api_types.Sample.pp_create Api_types.Sample.equal_create
  in
  Alcotest.(check (result sample_create_testable string))
    "should be equal" expected result

let test_sample_create_empty_type () =
  let get =
    Test_helpers.mock_row [ ("name", "test_sample"); ("sample_type", "") ]
  in
  let result = sample_create get in
  let sample_create_testable =
    testable Api_types.Sample.pp_create Api_types.Sample.equal_create
  in
  Alcotest.(check (result sample_create_testable string))
    "should return Error" (Error "Sample type cannot be empty") result

let test_product_create () =
  let get =
    Test_helpers.mock_row
      [
        ("name", "test_product");
        ("brand", "test_brand");
        ("manufacturer_part_number", "test_part_number");
        ("description", "test_description");
      ]
  in
  let expected : (Api_types.Product.create, string) result =
    Ok
      {
        Api_types.Product.name = "test_product";
        brand = Some "test_brand";
        manufacturer_part_number = Some "TEST_PART_NUMBER";
        description = Some "test_description";
      }
  in
  let result = product_create get in
  let product_create_testable =
    testable Api_types.Product.pp_create Api_types.Product.equal_create
  in
  Alcotest.(check (result product_create_testable string))
    "should be equal" expected result

let test_strain_create () =
  let get =
    Test_helpers.mock_row
      [
        ("genus", "test_genus");
        ("species", "test_species");
        ("strain_name", "test_strain");
        ("genotype", "test_genotype");
        ("parent_strain_id", "123");
        ("notes", "test_notes");
      ]
  in
  let expected : (Api_types.Strain.create, string) result =
    Ok
      {
        genus = "test_genus";
        species = "test_species";
        strain_name = "test_strain";
        genotype = Some "test_genotype";
        parent_strain_id = Some 123;
        notes = Some "test_notes";
        links = [];
      }
  in
  let result = strain_create get in
  let strain_create_testable =
    testable Api_types.Strain.pp_create Api_types.Strain.equal_create
  in
  Alcotest.(check (result strain_create_testable string))
    "should be equal" expected result

let test_strain_create_empty_species () =
  let get =
    Test_helpers.mock_row
      [
        ("genus", "test_genus"); ("species", ""); ("strain_name", "test_strain");
      ]
  in
  let result = strain_create get in
  let strain_create_testable =
    testable Api_types.Strain.pp_create Api_types.Strain.equal_create
  in
  Alcotest.(check (result strain_create_testable string))
    "should return Error" (Error "Strain species cannot be empty") result

let test_strain_create_empty_name () =
  let get =
    Test_helpers.mock_row
      [
        ("genus", "test_genus"); ("species", "test_species"); ("strain_name", "");
      ]
  in
  let result = strain_create get in
  let strain_create_testable =
    testable Api_types.Strain.pp_create Api_types.Strain.equal_create
  in
  Alcotest.(check (result strain_create_testable string))
    "should return Error" (Error "Strain name cannot be empty") result

let test_well_layout_item () =
  let get =
    Test_helpers.mock_row [ ("well", "A1"); ("sample_short_id", "SAMP-001") ]
  in
  let expected : (Api_types.Plate.well_layout_item, string) result =
    Ok { Api_types.Plate.well = "A1"; sample_short_id = "SAMP-001" }
  in
  let result = well_layout_item get in
  let well_layout_item_testable =
    testable Api_types.Plate.pp_well_layout_item
      Api_types.Plate.equal_well_layout_item
  in
  Alcotest.(check (result well_layout_item_testable string))
    "should be equal" expected result

let test_well_layout_item_empty_well () =
  let get =
    Test_helpers.mock_row [ ("well", ""); ("sample_short_id", "SAMP-001") ]
  in
  let result = well_layout_item get in
  let well_layout_item_testable =
    testable Api_types.Plate.pp_well_layout_item
      Api_types.Plate.equal_well_layout_item
  in
  Alcotest.(check (result well_layout_item_testable string))
    "should return Error" (Error "Well identifier cannot be empty") result

let test_well_layout_item_empty_sample_short_id () =
  let get = Test_helpers.mock_row [ ("well", "A1"); ("sample_short_id", "") ] in
  let result = well_layout_item get in
  let well_layout_item_testable =
    testable Api_types.Plate.pp_well_layout_item
      Api_types.Plate.equal_well_layout_item
  in
  Alcotest.(check (result well_layout_item_testable string))
    "should return Error" (Error "Sample short_id cannot be empty") result

let test_bulk_plate_action_blank () =
  let get =
    Test_helpers.mock_row
      [ ("plate_name", "Plate 1"); ("well", ""); ("sample_short_id", "") ]
  in
  let expected : (Api_types.Plate.bulk_action_item, string) result =
    Ok (Api_types.Plate.Create_blank { plate_name = "Plate 1" })
  in
  let result = bulk_plate_action_item get in
  let testable_item =
    testable Api_types.Plate.pp_bulk_action_item
      Api_types.Plate.equal_bulk_action_item
  in
  Alcotest.(check (result testable_item string))
    "should return Create_blank" expected result

let test_bulk_plate_action_layout () =
  let get =
    Test_helpers.mock_row
      [ ("plate_name", "Plate 1"); ("well", "A1"); ("sample_short_id", "S1") ]
  in
  let expected : (Api_types.Plate.bulk_action_item, string) result =
    Ok
      (Api_types.Plate.Create_with_layout
         { plate_name = "Plate 1"; well = "A1"; sample_short_id = "S1" })
  in
  let result = bulk_plate_action_item get in
  let testable_item =
    testable Api_types.Plate.pp_bulk_action_item
      Api_types.Plate.equal_bulk_action_item
  in
  Alcotest.(check (result testable_item string))
    "should return Create_with_layout" expected result

let test_bulk_plate_action_missing_name () =
  let get =
    Test_helpers.mock_row
      [ ("plate_name", ""); ("well", "A1"); ("sample_short_id", "S1") ]
  in
  let result = bulk_plate_action_item get in
  let testable_item =
    testable Api_types.Plate.pp_bulk_action_item
      Api_types.Plate.equal_bulk_action_item
  in
  Alcotest.(check (result testable_item string))
    "should return Error" (Error "plate_name cannot be empty") result

let test_bulk_plate_action_missing_well () =
  let get =
    Test_helpers.mock_row
      [ ("plate_name", "Plate 1"); ("well", ""); ("sample_short_id", "S1") ]
  in
  let result = bulk_plate_action_item get in
  let testable_item =
    testable Api_types.Plate.pp_bulk_action_item
      Api_types.Plate.equal_bulk_action_item
  in
  Alcotest.(check (result testable_item string))
    "should return Error"
    (Error "Well identifier cannot be empty if sample is provided") result

let test_bulk_plate_action_missing_sample () =
  let get =
    Test_helpers.mock_row
      [ ("plate_name", "Plate 1"); ("well", "A1"); ("sample_short_id", "") ]
  in
  let result = bulk_plate_action_item get in
  let testable_item =
    testable Api_types.Plate.pp_bulk_action_item
      Api_types.Plate.equal_bulk_action_item
  in
  Alcotest.(check (result testable_item string))
    "should return Error"
    (Error "Sample short_id cannot be empty if well is provided") result

let test_bulk_plate_layout_item_name () =
  let get =
    Test_helpers.mock_row
      [ ("plate_name", "Plate 1"); ("well", "A1"); ("sample_short_id", "S1") ]
  in
  let expected : (Api_types.Plate.bulk_layout_item, string) result =
    Ok { plate_name = "Plate 1"; well = "A1"; sample_short_id = "S1" }
  in
  let result = bulk_plate_layout_item get in
  let testable_item =
    testable Api_types.Plate.pp_bulk_layout_item
      Api_types.Plate.equal_bulk_layout_item
  in
  Alcotest.(check (result testable_item string))
    "should parse plate_name correctly" expected result

let test_bulk_plate_layout_item_short_id () =
  let get =
    Test_helpers.mock_row
      [
        ("plate_short_id", "P-16-0001");
        ("well", "A1");
        ("sample_short_id", "S1");
      ]
  in
  let expected : (Api_types.Plate.bulk_layout_item, string) result =
    Ok { plate_name = "P-16-0001"; well = "A1"; sample_short_id = "S1" }
  in
  let result = bulk_plate_layout_item get in
  let testable_item =
    testable Api_types.Plate.pp_bulk_layout_item
      Api_types.Plate.equal_bulk_layout_item
  in
  Alcotest.(check (result testable_item string))
    "should parse plate_short_id correctly" expected result

let test_bulk_plate_layout_item_missing_plate () =
  let get =
    Test_helpers.mock_row [ ("well", "A1"); ("sample_short_id", "S1") ]
  in
  let result = bulk_plate_layout_item get in
  let testable_item =
    testable Api_types.Plate.pp_bulk_layout_item
      Api_types.Plate.equal_bulk_layout_item
  in
  Alcotest.(check (result testable_item string))
    "should return Error when plate identifier is missing"
    (Error "Missing required column: 'plate_name' or 'plate_short_id'") result

let test_product_create_minimal () =
  let get =
    Test_helpers.mock_row
      [
        ("name", "test_product");
        ("manufacturer_part_number", "test_part_number");
      ]
  in
  let expected : (Api_types.Product.create, string) result =
    Ok
      {
        Api_types.Product.name = "test_product";
        brand = None;
        manufacturer_part_number = Some "TEST_PART_NUMBER";
        description = None;
      }
  in
  let result = product_create get in
  let product_create_testable =
    testable Api_types.Product.pp_create Api_types.Product.equal_create
  in
  Alcotest.(check (result product_create_testable string))
    "should be equal" expected result

let test_product_create_empty_name () =
  let get =
    Test_helpers.mock_row
      [ ("name", ""); ("manufacturer_part_number", "test_part_number") ]
  in
  let result = product_create get in
  let product_create_testable =
    testable Api_types.Product.pp_create Api_types.Product.equal_create
  in
  Alcotest.(check (result product_create_testable string))
    "should return Error" (Error "Product name cannot be empty") result

let test_product_create_normalization () =
  let get =
    Test_helpers.mock_row
      [
        ("name", "  Messy Product  ");
        ("manufacturer_part_number", "  pn-123  ");
        ("brand", "   ");
        ("description", "");
      ]
  in

  let expected : (Api_types.Product.create, string) result =
    Ok
      {
        Api_types.Product.name = "Messy Product";
        brand = None;
        manufacturer_part_number = Some "PN-123";
        description = None;
      }
  in

  let result = product_create get in

  let product_create_testable =
    testable Api_types.Product.pp_create Api_types.Product.equal_create
  in
  Alcotest.(check (result product_create_testable string))
    "Handles whitespace and normalization" expected result

let suite =
  [
    ( "Decoder: Sample",
      [
        test_case "Create Success" `Quick test_sample_create;
        test_case "Fail: Empty Type" `Quick test_sample_create_empty_type;
      ] );
    ( "Decoder: Product",
      [
        test_case "Create Success" `Quick test_product_create;
        test_case "Create Minimal" `Quick test_product_create_minimal;
        test_case "Normalization" `Quick test_product_create_normalization;
        test_case "Fail: Empty Name" `Quick test_product_create_empty_name;
      ] );
    ( "Decoder: Strain",
      [
        test_case "Create Success" `Quick test_strain_create;
        test_case "Fail: Empty Species" `Quick test_strain_create_empty_species;
        test_case "Fail: Empty Strain Name" `Quick test_strain_create_empty_name;
      ] );
    ( "Decoder: Plate Layout",
      [
        test_case "Item Success" `Quick test_well_layout_item;
        test_case "Fail: Empty Well" `Quick test_well_layout_item_empty_well;
        test_case "Fail: Empty Sample" `Quick
          test_well_layout_item_empty_sample_short_id;
        test_case "Bulk Action Blank" `Quick test_bulk_plate_action_blank;
        test_case "Bulk Action Layout" `Quick test_bulk_plate_action_layout;
        test_case "Bulk Action Fail: Empty Name" `Quick
          test_bulk_plate_action_missing_name;
        test_case "Bulk Action Fail: Missing Well" `Quick
          test_bulk_plate_action_missing_well;
        test_case "Bulk Action Fail: Missing Sample" `Quick
          test_bulk_plate_action_missing_sample;
        test_case "Bulk Layout: By Name" `Quick test_bulk_plate_layout_item_name;
        test_case "Bulk Layout: By Short ID" `Quick
          test_bulk_plate_layout_item_short_id;
        test_case "Bulk Layout: Missing Plate" `Quick
          test_bulk_plate_layout_item_missing_plate;
      ] );
  ]
