open Alcotest
open Exlab_core.Types
open Exlab_core.Export_data

let test_escape_csv_field () =
  let check_escape input expected =
    let actual = escape_csv_field input in
    Alcotest.(check string) ("Escaping '" ^ input ^ "'") expected actual
  in
  check_escape "simple" "simple";
  check_escape "with,comma" "\"with,comma\"";
  check_escape "with\"quote" "\"with\"\"quote\"";
  check_escape "with\nnewline" "\"with\nnewline\"";
  check_escape "a,b\"c\nd" "\"a,b\"\"c\nd\""

let test_plate_wells_to_csv_rows () =
  let rows = [ ("A1", Some "S1"); ("B2", None); ("C3", Some "S3") ] in
  let expected_csv =
    "Coordinate,Short ID\n" ^ "A1,S1\n" ^ "B2,\n" ^ "C3,S3\n"
  in
  let actual_csv = plate_wells_to_csv_rows rows in
  Alcotest.(check string) "CSV output for plate wells" expected_csv actual_csv

let test_samples_to_csv () =
  let samples =
    [
      {
        id = 1;
        uid = "uid1";
        short_id = "S1";
        project_id = 1;
        sample_type = "Type A";
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
        uid = "uid2";
        short_id = "S2";
        project_id = 1;
        sample_type = "Type B";
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
  let expected_csv =
    "ID,Short ID,Sample Type,Status\n" ^ "1,S1,Type A,Active\n"
    ^ "2,S2,Type B,Active\n"
  in
  let actual_csv = samples_to_csv samples in
  Alcotest.(check string) "CSV output for samples" expected_csv actual_csv

let test_samples_to_csv_empty () =
  let samples = [] in
  let expected_csv = "ID,Short ID,Sample Type,Status\n" in
  let actual_csv = samples_to_csv samples in
  Alcotest.(check string)
    "CSV output for empty samples list" expected_csv actual_csv

let test_projects_to_csv () =
  let projects =
    [
      {
        id = 1;
        uid = "uid1";
        short_id = "P1";
        name = "Project 1";
        description = None;
        status = Active;
        contact_name = None;
        owner = Some "Owner 1";
        metadata = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "uid2";
        short_id = "P2";
        name = "Project 2, with comma";
        description = None;
        status = Archived;
        contact_name = None;
        owner = Some "Owner 2";
        metadata = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let expected_csv =
    "ID,Short ID,Name,Status,Owner\n" ^ "1,P1,Project 1,Active,Owner 1\n"
    ^ "2,P2,\"Project 2, with comma\",Archived,Owner 2\n"
  in
  let actual_csv = projects_to_csv projects in
  Alcotest.(check string) "CSV output for projects list" expected_csv actual_csv

let test_projects_to_csv_with_metadata () =
  let projects =
    [
      {
        id = 1;
        uid = "uid1";
        short_id = "P1";
        name = "Project 1";
        description = None;
        status = Active;
        contact_name = None;
        owner = Some "Owner 1";
        metadata =
          Some (`Assoc [ ("Client", `String "Acme"); ("Cost", `Int 100) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "uid2";
        short_id = "P2";
        name = "Project 2";
        description = None;
        status = Archived;
        contact_name = None;
        owner = Some "Owner 2";
        metadata =
          Some (`Assoc [ ("Client", `String "Globex"); ("Urgent", `Bool true) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 3;
        uid = "uid3";
        short_id = "P3";
        name = "Project 3";
        description = None;
        status = Active;
        contact_name = None;
        owner = None;
        metadata = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let expected_csv =
    "ID,Short ID,Name,Status,Owner,Client,Cost,Urgent\n"
    ^ "1,P1,Project 1,Active,Owner 1,Acme,100,\n"
    ^ "2,P2,Project 2,Archived,Owner 2,Globex,,true\n"
    ^ "3,P3,Project 3,Active,,,,\n"
  in
  let actual_csv = projects_to_csv projects in
  Alcotest.(check string)
    "CSV output for projects with metadata" expected_csv actual_csv

let test_projects_to_csv_empty () =
  let projects = [] in
  let expected_csv = "ID,Short ID,Name,Status,Owner\n" in
  let actual_csv = projects_to_csv projects in
  Alcotest.(check string)
    "CSV output for empty projects list" expected_csv actual_csv

let test_products_to_csv () =
  let products =
    [
      {
        id = 1;
        uid = "uid1";
        short_id = "P1";
        name = "Product 1";
        brand = Some "Brand A";
        manufacturer_part_number = Some "PN1";
        description = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "uid2";
        short_id = "P2";
        name = "Product 2, with comma";
        brand = Some "Brand B";
        manufacturer_part_number = Some "PN2";
        description = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 3;
        uid = "uid3";
        short_id = "P3";
        name = "Sparse Product";
        brand = None;
        manufacturer_part_number = None;
        description = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let expected_csv =
    "ID,Short ID,Name,Brand,Part Number,Created At\n"
    ^ "1,P1,Product 1,Brand A,PN1,0.\n"
    ^ "2,P2,\"Product 2, with comma\",Brand B,PN2,0.\n"
    ^ "3,P3,Sparse Product,,,0.\n"
  in
  let actual_csv = products_to_csv products in
  Alcotest.(check string) "CSV output for products list" expected_csv actual_csv

let test_products_to_csv_empty () =
  let products = [] in
  let expected_csv = "ID,Short ID,Name,Brand,Part Number,Created At\n" in
  let actual_csv = products_to_csv products in
  Alcotest.(check string)
    "CSV output for empty products list" expected_csv actual_csv

let test_result_definitions_to_csv () =
  let result_definitions =
    [
      {
        id = 1;
        uid = "uid1";
        short_id = "RD1";
        name = "OD600";
        description = None;
        data_type = ResultType.Float;
        unit = Some "AU";
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "uid2";
        short_id = "RD2";
        name = "Boolean Flag";
        description = None;
        data_type = ResultType.Boolean;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let expected_csv =
    "ID,Short ID,Name,Category,Required,Data Type,Unit,Created At\n"
    ^ "1,RD1,OD600,No Category,False,Float,AU,0.\n"
    ^ "2,RD2,Boolean Flag,No Category,False,Boolean,,0.\n"
  in
  let actual_csv = result_definitions_to_csv result_definitions in
  Alcotest.(check string)
    "CSV output for result definitions list" expected_csv actual_csv

let test_result_definitions_to_csv_empty () =
  let result_definitions = [] in
  let expected_csv =
    "ID,Short ID,Name,Category,Required,Data Type,Unit,Created At\n"
  in
  let actual_csv = result_definitions_to_csv result_definitions in
  Alcotest.(check string)
    "CSV output for empty result definitions list" expected_csv actual_csv

let test_plate_wells_to_numeric_csv () =
  let rows = [ ("A1", Some "S1"); ("B2", None); ("C3", Some "S3") ] in
  let expected_csv =
    "well,sample_short_id\n" ^ "1,S1\n" ^ "10,\n" ^ "19,S3\n"
  in
  let actual_csv =
    plate_wells_to_numeric_csv
      (Exlab_core.Plate.validate_format "96-well" |> Result.get_ok)
      rows
  in
  Alcotest.(check string)
    "Numeric CSV output for plate wells" expected_csv actual_csv

let test_plate_wells_to_matrix_csv () =
  let rows = [ ("A1", Some "S1"); ("B2", Some "S2"); ("H12", Some "S3") ] in
  let expected_csv =
    ",1,2,3,4,5,6,7,8,9,10,11,12\n" ^ "A,S1,,,,,,,,,,,\n" ^ "B,,S2,,,,,,,,,,\n"
    ^ "C,,,,,,,,,,,,\n" ^ "D,,,,,,,,,,,,\n" ^ "E,,,,,,,,,,,,\n"
    ^ "F,,,,,,,,,,,,\n" ^ "G,,,,,,,,,,,,\n" ^ "H,,,,,,,,,,,,S3\n"
  in
  let actual_csv =
    plate_wells_to_matrix_csv
      (Exlab_core.Plate.validate_format "96-well" |> Result.get_ok)
      rows
  in
  Alcotest.(check string)
    "Matrix CSV output for plate wells" expected_csv actual_csv

let test_plate_results_to_csv () =
  let definitions =
    [
      {
        id = 1;
        uid = "uid1";
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
        uid = "uid2";
        short_id = "RD2";
        name = "Notes";
        description = None;
        data_type = ResultType.String;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 3;
        uid = "uid3";
        short_id = "RD3";
        name = "Growth Curve";
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
  let results =
    [
      {
        id = 1;
        uid = "r1";
        sample_id = None;
        plate_id = Some 1;
        result_definition_id = 1;
        value = Some (ResultPayload.Float 1.23);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "r2";
        sample_id = None;
        plate_id = Some 1;
        result_definition_id = 2;
        value = Some (ResultPayload.String "some notes");
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 3;
        uid = "r3";
        sample_id = None;
        plate_id = Some 1;
        result_definition_id = 3;
        value = Some (ResultPayload.FloatSeries [ (1, 1.0); (2, 2.0) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let expected_csv =
    "Result Type,Value,Updated At\n\
     OD600,1.23,0.000000\n\
     Notes,some notes,0.000000\n\
     Growth \
     Curve,\"{\"\"type\"\":\"\"FloatSeries\"\",\"\"value\"\":[{\"\"time\"\":1,\"\"value\"\":1.0},{\"\"time\"\":2,\"\"value\"\":2.0}]}\",0.000000\n"
  in
  let actual_csv = plate_results_to_csv ~definitions results in
  Alcotest.(check string) "CSV output for plate results" expected_csv actual_csv

let test_generate_longitudinal_matrix () =
  let definitions = [] in
  let samples = [] in
  let plates = [] in
  let wells = [] in
  let strains = [] in
  let results = [] in
  let matrix =
    generate_longitudinal_matrix ~definitions ~samples ~plates ~wells ~strains
      results
  in
  let expected_columns =
    [
      `Assoc
        [
          ("title", `String "Strain");
          ("data", `String "strain");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Source");
          ("data", `String "source");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Experimental");
          ("data", `String "experimental");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate");
          ("data", `String "plate");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate_Pos");
          ("data", `String "plate_pos");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Time_Point");
          ("data", `String "time_point");
          ("visible", `Bool true);
        ];
    ]
  in
  let expected =
    `Assoc [ ("columns", `List expected_columns); ("data", `List []) ]
  in
  Alcotest.(check string)
    "Empty Matrix columns and data"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string matrix)

let test_generate_longitudinal_matrix () =
  let definitions = [] in
  let samples = [] in
  let plates = [] in
  let wells = [] in
  let strains = [] in
  let results = [] in
  let matrix =
    generate_longitudinal_matrix ~definitions ~samples ~plates ~wells ~strains
      results
  in
  let expected_columns =
    [
      `Assoc
        [
          ("title", `String "Strain");
          ("data", `String "strain");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Source");
          ("data", `String "source");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Experimental");
          ("data", `String "experimental");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate");
          ("data", `String "plate");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate_Pos");
          ("data", `String "plate_pos");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Time_Point");
          ("data", `String "time_point");
          ("visible", `Bool true);
        ];
    ]
  in
  let expected =
    `Assoc [ ("columns", `List expected_columns); ("data", `List []) ]
  in
  Alcotest.(check string)
    "Empty Matrix columns and data"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string matrix)

let test_longitudinal_matrix_data () =
  let definitions =
    [
      {
        id = 1;
        uid = "uid1";
        short_id = "RD1";
        name = "OD600";
        description = None;
        data_type = ResultType.FloatSeries;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "uid2";
        short_id = "RD2";
        name = "Media";
        description = None;
        data_type = ResultType.String;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let strains =
    [
      {
        id = 10;
        uid = "u10";
        genus = "G";
        species = "S";
        strain_name = "NC1";
        genotype = None;
        parent_strain_id = None;
        notes = None;
        external_links = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let samples =
    [
      {
        id = 100;
        uid = "u100";
        short_id = "UCR-1";
        project_id = 1;
        sample_type = "Source";
        status = Active;
        category = Source;
        parent_sample_id = None;
        strain_id = Some 10;
        community_id = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 101;
        uid = "u101";
        short_id = "UCR-1-01";
        project_id = 1;
        sample_type = "Exp";
        status = Active;
        category = Experimental;
        parent_sample_id = Some 100;
        strain_id = Some 10;
        community_id = None;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let plates =
    [
      {
        id = 200;
        uid = "u200";
        short_id = "Plt-1";
        name = "Plate 1";
        project_id = 1;
        product_id = None;
        plate_format = Well_96;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let wells =
    [ { id = 300; plate_id = 200; sample_id = Some 101; coordinate = "A1" } ]
  in
  let results =
    [
      {
        id = 1;
        uid = "r1";
        sample_id = Some 101;
        plate_id = None;
        result_definition_id = 1;
        value = Some (ResultPayload.FloatSeries [ (1, 0.12); (2, 0.45) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "r2";
        sample_id = Some 101;
        plate_id = None;
        result_definition_id = 2;
        value = Some (ResultPayload.String "glc");
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let matrix =
    generate_longitudinal_matrix ~definitions ~samples ~plates ~wells ~strains
      results
  in

  let expected_columns =
    [
      `Assoc
        [
          ("title", `String "Strain");
          ("data", `String "strain");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Source");
          ("data", `String "source");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Experimental");
          ("data", `String "experimental");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate");
          ("data", `String "plate");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate_Pos");
          ("data", `String "plate_pos");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Time_Point");
          ("data", `String "time_point");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "OD600");
          ("data", `String "def_1");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Media");
          ("data", `String "def_2");
          ("visible", `Bool true);
        ];
    ]
  in

  let expected_data =
    [
      `Assoc
        [
          ("strain", `String "NC1");
          ("source", `String "UCR-1");
          ("experimental", `String "UCR-1-01");
          ("plate", `String "Plt-1");
          ("plate_pos", `String "A1");
          ("time_point", `Int 1);
          ("def_1", `Float 0.12);
          ("def_2", `String "glc");
        ];
      `Assoc
        [
          ("strain", `String "NC1");
          ("source", `String "UCR-1");
          ("experimental", `String "UCR-1-01");
          ("plate", `String "Plt-1");
          ("plate_pos", `String "A1");
          ("time_point", `Int 2);
          ("def_1", `Float 0.45);
          ("def_2", `String "glc");
        ];
    ]
  in

  let expected =
    `Assoc
      [ ("columns", `List expected_columns); ("data", `List expected_data) ]
  in
  Alcotest.(check string)
    "Longitudinal matrix data"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string matrix)

let test_longitudinal_matrix_filters_archived () =
  let definitions =
    [
      {
        id = 1;
        uid = "uid1";
        short_id = "RD1";
        name = "OD600";
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
  let strains = [] in
  let samples =
    [
      {
        id = 101;
        uid = "u101";
        short_id = "UCR-1-01";
        project_id = 1;
        sample_type = "Exp";
        status = Archived;
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
  let wells = [] in
  let results =
    [
      {
        id = 1;
        uid = "r1";
        sample_id = Some 101;
        plate_id = None;
        result_definition_id = 1;
        value = Some (ResultPayload.FloatSeries [ (1, 0.12) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let matrix =
    generate_longitudinal_matrix ~definitions ~samples ~plates ~wells ~strains
      results
  in

  let expected_columns =
    [
      `Assoc
        [
          ("title", `String "Strain");
          ("data", `String "strain");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Source");
          ("data", `String "source");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Experimental");
          ("data", `String "experimental");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate");
          ("data", `String "plate");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate_Pos");
          ("data", `String "plate_pos");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Time_Point");
          ("data", `String "time_point");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "OD600");
          ("data", `String "def_1");
          ("visible", `Bool true);
        ];
    ]
  in

  let expected =
    `Assoc [ ("columns", `List expected_columns); ("data", `List []) ]
  in
  Alcotest.(check string)
    "Longitudinal matrix data filters archived"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string matrix)

let test_longitudinal_matrix_plate_results () =
  let definitions =
    [
      {
        id = 1;
        uid = "uid1";
        short_id = "RD1";
        name = "OD600";
        description = None;
        data_type = ResultType.FloatSeries;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "uid2";
        short_id = "RD2";
        name = "Media";
        description = None;
        data_type = ResultType.String;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let strains = [] in
  let samples =
    [
      {
        id = 101;
        uid = "u101";
        short_id = "UCR-1-01";
        project_id = 1;
        sample_type = "Exp";
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
  let plates =
    [
      {
        id = 200;
        uid = "u200";
        short_id = "Plt-1";
        name = "Plate 1";
        project_id = 1;
        product_id = None;
        plate_format = Well_96;
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let wells =
    [ { id = 300; plate_id = 200; sample_id = Some 101; coordinate = "A1" } ]
  in
  let results =
    [
      {
        id = 1;
        uid = "r1";
        sample_id = Some 101;
        plate_id = None;
        result_definition_id = 1;
        value = Some (ResultPayload.FloatSeries [ (1, 0.12) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "r2";
        sample_id = None;
        plate_id = Some 200;
        result_definition_id = 2;
        value = Some (ResultPayload.String "glc");
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let matrix =
    generate_longitudinal_matrix ~definitions ~samples ~plates ~wells ~strains
      results
  in

  let expected_columns =
    [
      `Assoc
        [
          ("title", `String "Strain");
          ("data", `String "strain");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Source");
          ("data", `String "source");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Experimental");
          ("data", `String "experimental");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate");
          ("data", `String "plate");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Plate_Pos");
          ("data", `String "plate_pos");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Time_Point");
          ("data", `String "time_point");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "OD600");
          ("data", `String "def_1");
          ("visible", `Bool true);
        ];
      `Assoc
        [
          ("title", `String "Media");
          ("data", `String "def_2");
          ("visible", `Bool true);
        ];
    ]
  in

  let expected_data =
    [
      `Assoc
        [
          ("strain", `String "");
          ("source", `String "");
          ("experimental", `String "UCR-1-01");
          ("plate", `String "Plt-1");
          ("plate_pos", `String "A1");
          ("time_point", `Int 1);
          ("def_1", `Float 0.12);
          ("def_2", `String "glc");
        ];
    ]
  in

  let expected =
    `Assoc
      [ ("columns", `List expected_columns); ("data", `List expected_data) ]
  in
  Alcotest.(check string)
    "Longitudinal matrix data with plate results"
    (Yojson.Safe.to_string expected)
    (Yojson.Safe.to_string matrix)

let suite =
  [
    ("CSV", [ test_case "Escape Field" `Quick test_escape_csv_field ]);
    ( "Plate Well Export",
      [
        test_case "Well to CSV" `Quick test_plate_wells_to_csv_rows;
        test_case "Plate wells to numeric CSV" `Quick
          test_plate_wells_to_numeric_csv;
        test_case "Plate wells to matrix CSV" `Quick
          test_plate_wells_to_matrix_csv;
      ] );
    ( "Sample Export",
      [
        test_case "Sample List to CSV" `Quick test_samples_to_csv;
        test_case "Empty Sample List to CSV" `Quick test_samples_to_csv_empty;
      ] );
    ( "Project Export",
      [
        test_case "Project List to CSV" `Quick test_projects_to_csv;
        test_case "Empty Project List to CSV" `Quick test_projects_to_csv_empty;
        test_case "Project List with Metadata to CSV" `Quick
          test_projects_to_csv_with_metadata;
      ] );
    ( "Product Export",
      [
        test_case "Product List to CSV" `Quick test_products_to_csv;
        test_case "Empty Product List to CSV" `Quick test_products_to_csv_empty;
      ] );
    ( "Result Definition Export",
      [
        test_case "Result Definition List to CSV" `Quick
          test_result_definitions_to_csv;
        test_case "Empty Result Definition List to CSV" `Quick
          test_result_definitions_to_csv_empty;
      ] );
    ( "Plate Results Export",
      [ test_case "Plate Results to CSV" `Quick test_plate_results_to_csv ] );
    ( "Longitudinal Matrix Export",
      [
        test_case "Empty Matrix" `Quick test_generate_longitudinal_matrix;
        test_case "Data Matrix" `Quick test_longitudinal_matrix_data;
        test_case "Filters Archived" `Quick
          test_longitudinal_matrix_filters_archived;
        test_case "Plate Results" `Quick test_longitudinal_matrix_plate_results;
      ] );
  ]

let test_plate_results_to_csv () =
  let definitions =
    [
      {
        id = 1;
        uid = "uid1";
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
        uid = "uid2";
        short_id = "RD2";
        name = "Notes";
        description = None;
        data_type = ResultType.String;
        unit = None;
        category = None;
        is_required = false;
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 3;
        uid = "uid3";
        short_id = "RD3";
        name = "Growth Curve";
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
  let results =
    [
      {
        id = 1;
        uid = "r1";
        sample_id = None;
        plate_id = Some 1;
        result_definition_id = 1;
        value = Some (ResultPayload.Float 1.23);
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 2;
        uid = "r2";
        sample_id = None;
        plate_id = Some 1;
        result_definition_id = 2;
        value = Some (ResultPayload.String "some notes");
        created_at = 0.0;
        updated_at = 0.0;
      };
      {
        id = 3;
        uid = "r3";
        sample_id = None;
        plate_id = Some 1;
        result_definition_id = 3;
        value = Some (ResultPayload.FloatSeries [ (1, 1.0); (2, 2.0) ]);
        created_at = 0.0;
        updated_at = 0.0;
      };
    ]
  in
  let expected_csv =
    "Result Type,Value,Updated At\n\
     OD600,1.23,0.000000\n\
     Notes,some notes,0.000000\n\
     Growth \
     Curve,\"{\"\"type\"\":\"\"TimeSeries\"\",\"\"value\"\":[{\"\"time\"\":1,\"\"value\"\":{\"\"type\"\":\"\"Float\"\",\"\"value\"\":1}},{\"\"time\"\":2,\"\"value\"\":{\"\"type\"\":\"\"Float\"\",\"\"value\"\":2}}]}\",0.000000\n"
  in
  let actual_csv = plate_results_to_csv ~definitions results in
  Alcotest.(check string) "CSV output for plate results" expected_csv actual_csv
