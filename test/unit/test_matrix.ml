open Alcotest
open Exlab_core.Types
open Exlab_core.Export_data

let test_generate_matrix_empty () =
  let definitions = [] in
  let samples = [] in
  let plates = [] in
  let results = [] in
  let expected_columns =
    `List
      [
        `Assoc
          [
            ("title", `String "Entity Type");
            ("data", `String "entity_type");
            ("visible", `Bool true);
          ];
        `Assoc
          [
            ("title", `String "Short ID");
            ("data", `String "short_id");
            ("visible", `Bool true);
          ];
      ]
  in
  let expected_json =
    `Assoc [ ("columns", expected_columns); ("data", `List []) ]
  in
  let actual_json = generate_matrix ~definitions ~samples ~plates results in
  Alcotest.(check string)
    "Empty matrix should match"
    (Yojson.Safe.to_string expected_json)
    (Yojson.Safe.to_string actual_json)

let test_generate_matrix_scalars_and_series () =
  let definitions =
    [
      {
        id = 1;
        uid = "u1";
        short_id = "RD1";
        name = "OD600";
        description = None;
        data_type = ResultType.Float;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "u2";
        short_id = "RD2";
        name = "Growth";
        description = None;
        data_type = ResultType.FloatSeries;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let samples =
    [
      {
        id = 1;
        uid = "s1";
        short_id = "SMP-001";
        project_id = 1;
        sample_type = "Culture";
        status = Active;
        category = Experimental;
        parent_sample_id = None;
        strain_id = None;
        community_id = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "s2";
        short_id = "SMP-002";
        project_id = 1;
        sample_type = "Culture";
        status = Active;
        category = Experimental;
        parent_sample_id = None;
        strain_id = None;
        community_id = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let plates = [] in
  let results =
    [
      {
        id = 1;
        uid = "r1";
        sample_id = Some 1;
        plate_id = None;
        result_definition_id = 1;
        value = Some (ResultPayload.Float 1.3);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "r2";
        sample_id = Some 1;
        plate_id = None;
        result_definition_id = 2;
        value = Some (ResultPayload.FloatSeries [ (1, 0.1); (2, 0.5) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 3;
        uid = "r3";
        sample_id = Some 2;
        plate_id = None;
        result_definition_id = 1;
        value = Some (ResultPayload.Float 1.5);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 4;
        uid = "r4";
        sample_id = Some 2;
        plate_id = None;
        result_definition_id = 2;
        value = Some (ResultPayload.FloatSeries [ (1, 0.15); (2, 0.52) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in

  let actual_json = generate_matrix ~definitions ~samples ~plates results in

  let expected_columns =
    `List
      [
        `Assoc
          [
            ("title", `String "Entity Type");
            ("data", `String "entity_type");
            ("visible", `Bool true);
          ];
        `Assoc
          [
            ("title", `String "Short ID");
            ("data", `String "short_id");
            ("visible", `Bool true);
          ];
        `Assoc
          [
            ("title", `String "OD600");
            ("data", `String "def_1_scalar");
            ("visible", `Bool true);
          ];
        `Assoc
          [
            ("title", `String "Growth (Pt 1)");
            ("data", `String "def_2_pt_1");
            ("visible", `Bool false);
          ];
        `Assoc
          [
            ("title", `String "Growth (Pt 2)");
            ("data", `String "def_2_pt_2");
            ("visible", `Bool false);
          ];
      ]
  in

  let expected_data =
    `List
      [
        `Assoc
          [
            ("id", `Int 1);
            ("entity_type", `String "Sample");
            ("short_id", `String "SMP-001");
            ("def_2_pt_1", `Float 0.1);
            ("def_2_pt_2", `Float 0.5);
            ("def_1_scalar", `Float 1.3);
          ];
        `Assoc
          [
            ("id", `Int 2);
            ("entity_type", `String "Sample");
            ("short_id", `String "SMP-002");
            ("def_2_pt_1", `Float 0.15);
            ("def_2_pt_2", `Float 0.52);
            ("def_1_scalar", `Float 1.5);
          ];
      ]
  in

  let expected_json =
    `Assoc [ ("columns", expected_columns); ("data", expected_data) ]
  in

  Alcotest.(check string)
    "Scalars and Series matrix should match"
    (Yojson.Safe.to_string expected_json)
    (Yojson.Safe.to_string actual_json)

let suite =
  [
    ( "Matrix Generation",
      [
        test_case "Empty Matrix" `Quick test_generate_matrix_empty;
        test_case "Scalars and Series Matrix" `Quick
          test_generate_matrix_scalars_and_series;
      ] );
  ]
