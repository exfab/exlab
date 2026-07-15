(* Test Definitions for API endpoint /api/v1/strains *)

let handler = Test_utils.admin_app

let test_create_malformed_json _switch () =
  let body = {| {"genus": "Broken JSON", |} in
  let req = Test_utils.json_post ~path:"/api/v1/strains" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_fields _switch () =
  let body = {| {"genus": "Saccharomyces", "species": "cerevisiae"} |} in
  let req = Test_utils.json_post ~path:"/api/v1/strains" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing fields" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_strain_lifecycle _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let strain_name = Printf.sprintf "Test Strain UID %f" time in
  let body =
    Printf.sprintf
      {| {
    "genus": "Saccharomyces",
    "species": "cerevisiae",
    "strain_name": "%s",
    "genotype": "WT",
    "parent_strain_id": null,
    "notes": "A test strain for UID lookup.",
    "links": []
  } |}
      strain_name
  in

  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body
      ~expected_status:201 "Create successful"
  in
  let* create_body = Dream.body create_res in
  let json = Yojson.Safe.from_string create_body in
  let strain_id = Yojson.Safe.Util.(member "id" json |> to_int) in

  let path = Printf.sprintf "/api/v1/strains/%d" strain_id in
  let* _ =
    Test_utils.assert_json_get ~handler ~path ~expected_status:200
      "Get by ID should return 200"
  in

  let update_body =
    Printf.sprintf
      {| {
    "genus": "Saccharomyces",
    "species": "cerevisiae",
    "strain_name": "%s Updated",
    "genotype": "delta-foo",
    "parent_strain_id": null,
    "notes": "Updated notes"
  } |}
      strain_name
  in
  let* _ =
    Test_utils.assert_json_put ~handler ~path ~body:update_body
      ~expected_status:200 "Update successful"
  in

  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/strains"
      ~expected_status:200 "Get all strains"
  in
  Lwt.return ()

let test_bulk_create_json_success _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let name_a = Printf.sprintf "Strain A %f" time in
  let name_b = Printf.sprintf "Strain B %f" time in
  let body =
    Printf.sprintf
      {|
    { "strains": [
        { "genus": "Saccharomyces", "species": "cerevisiae", "strain_name": "%s", "genotype": "WT" },
        { "genus": "Escherichia", "species": "coli", "strain_name": "%s", "genotype": "foo" }
      ]
    }
  |}
      name_a name_b
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains/bulk" ~body
      ~expected_status:201 "Bulk create successful"
  in
  Lwt.return ()

let test_bulk_create_csv_success _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let name_c = Printf.sprintf "Strain C %f" time in
  let name_d = Printf.sprintf "Strain D %f" time in
  let csv_body =
    Printf.sprintf
      "genus,species,strain_name,genotype\n\
       Saccharomyces,cerevisiae,%s,WT\n\
       Escherichia,coli,%s,foo"
      name_c name_d
  in
  let* _ =
    Test_utils.assert_csv_post ~handler ~path:"/api/v1/strains/bulk-csv"
      ~body:csv_body ~expected_status:201 "Bulk CSV create successful"
  in
  Lwt.return ()

let test_search_strains _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let search_term = Printf.sprintf "UniqueSearchTerm%f" time in
  let name_a = Printf.sprintf "Strain A %s" search_term in
  let name_b = "Unrelated Strain" in

  let body_a =
    Printf.sprintf
      {| {"genus": "Saccharomyces", "species": "cerevisiae", "strain_name": "%s", "genotype": "WT", "links": []} |}
      name_a
  in
  let body_b =
    Printf.sprintf
      {| {"genus": "Escherichia", "species": "coli", "strain_name": "%s", "genotype": "foo", "links": []} |}
      name_b
  in

  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body:body_a
      ~expected_status:201 "Create Strain A"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body:body_b
      ~expected_status:201 "Create Strain B"
  in

  let search_path = Printf.sprintf "/api/v1/strains?search=%s" search_term in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search strains"
  in

  let* search_body = Dream.body search_res in
  let json = Yojson.Safe.from_string search_body in
  let data = Yojson.Safe.Util.(member "data" json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching strain" 1 (List.length data);

  let returned_name =
    Yojson.Safe.Util.(List.hd data |> member "strain_name" |> to_string)
  in
  Alcotest.(check string) "Returned strain name matches" name_a returned_name;

  Lwt.return ()

let test_advanced_search_strains _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let search_term = Printf.sprintf "AdvSearchTerm%f" time in
  let name_a = Printf.sprintf "Strain A %s" search_term in
  let name_b = Printf.sprintf "Strain B %s" search_term in

  let body_a =
    Printf.sprintf
      {| {"genus": "Saccharomyces", "species": "cerevisiae", "strain_name": "%s", "genotype": "WT", "links": []} |}
      name_a
  in
  let body_b =
    Printf.sprintf
      {| {"genus": "Escherichia", "species": "coli", "strain_name": "%s", "genotype": "foo", "links": []} |}
      name_b
  in

  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body:body_a
      ~expected_status:201 "Create Strain A"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body:body_b
      ~expected_status:201 "Create Strain B"
  in

  (* Search by genus *)
  let search_path_genus =
    Printf.sprintf "/api/v1/strains?search=%s&genus=Saccharomyces" search_term
  in
  let* search_res_genus =
    Test_utils.assert_json_get ~handler ~path:search_path_genus
      ~expected_status:200 "Search strains by genus"
  in
  let* search_body_genus = Dream.body search_res_genus in
  let json_genus = Yojson.Safe.from_string search_body_genus in
  let data_genus = Yojson.Safe.Util.(member "data" json_genus |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching strain by genus" 1
    (List.length data_genus);
  let returned_name_genus =
    Yojson.Safe.Util.(List.hd data_genus |> member "strain_name" |> to_string)
  in
  Alcotest.(check string)
    "Returned strain name matches" name_a returned_name_genus;

  (* Search by species *)
  let search_path_species =
    Printf.sprintf "/api/v1/strains?search=%s&species=coli" search_term
  in
  let* search_res_species =
    Test_utils.assert_json_get ~handler ~path:search_path_species
      ~expected_status:200 "Search strains by species"
  in
  let* search_body_species = Dream.body search_res_species in
  let json_species = Yojson.Safe.from_string search_body_species in
  let data_species = Yojson.Safe.Util.(member "data" json_species |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching strain by species" 1
    (List.length data_species);
  let returned_name_species =
    Yojson.Safe.Util.(List.hd data_species |> member "strain_name" |> to_string)
  in
  Alcotest.(check string)
    "Returned strain name matches" name_b returned_name_species;

  Lwt.return ()

let test_strain_get_nested_projects _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Nested Proj %f" time in
  let strain_name = Printf.sprintf "Nested Strain %f" time in

  (* 1. Create Project *)
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
      ~expected_status:201 "Create Project"
  in
  let* proj_body = Dream.body proj_res in
  let proj_id =
    Yojson.Safe.Util.(member "id" (Yojson.Safe.from_string proj_body) |> to_int)
  in

  (* 2. Create Strain *)
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Test", "species": "strain", "strain_name": "%s", "links": []}|}
           strain_name)
      ~expected_status:201 "Create Strain"
  in
  let* strain_body = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body) |> to_int)
  in

  (* 3. Create Source Sample *)
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d}|}
           strain_id)
      ~expected_status:201 "Create Source Sample"
  in

  (* 4. Get Strain and verify nested project data *)
  let path = Printf.sprintf "/api/v1/strains/%d" strain_id in
  let* get_res =
    Test_utils.assert_json_get ~handler ~path ~expected_status:200
      "Get Strain by ID"
  in
  let* get_body = Dream.body get_res in
  let json = Yojson.Safe.from_string get_body in

  let source_samples =
    Yojson.Safe.Util.(member "source_samples" json |> to_list)
  in
  Alcotest.(check int)
    "Should have exactly 1 source sample" 1
    (List.length source_samples);

  let sample_entry = List.hd source_samples in
  let returned_proj_id =
    Yojson.Safe.Util.(member "project" sample_entry |> member "id" |> to_int)
  in
  let returned_proj_name =
    Yojson.Safe.Util.(
      member "project" sample_entry |> member "name" |> to_string)
  in

  Alcotest.(check int) "Returned project ID matches" proj_id returned_proj_id;
  Alcotest.(check string)
    "Returned project name matches" proj_name returned_proj_name;

  Lwt.return ()

let suite =
  [
    ( "Strain API",
      [
        Alcotest_lwt.test_case "Strain Lifecycle (Create, Get, Update, List)"
          `Quick test_strain_lifecycle;
        Alcotest_lwt.test_case "Get Strain returns nested projects" `Quick
          test_strain_get_nested_projects;
        Alcotest_lwt.test_case "Search Strains" `Quick test_search_strains;
        Alcotest_lwt.test_case "Advanced Search Strains" `Quick
          test_advanced_search_strains;
        Alcotest_lwt.test_case "Bulk JSON: Success" `Quick
          test_bulk_create_json_success;
        Alcotest_lwt.test_case "Bulk CSV: Success" `Quick
          test_bulk_create_csv_success;
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "Reject Missing Fields" `Quick
          test_create_missing_fields;
      ] );
  ]
