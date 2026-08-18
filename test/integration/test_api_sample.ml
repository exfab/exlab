(* Test Definitions for API endpoint /api/projects/:id/samples *)

let handler = Test_utils.admin_app

let test_bad_json_requests _switch () =
  let open Lwt.Syntax in
  let cases =
    [
      ( "/api/v1/projects/1/samples",
        {| {"name": "Broken JSON", |},
        "Should return 400 for bad JSON" );
      ( "/api/v1/projects/1/samples",
        {| {"name": "Test Sample"} |},
        "Should return 400 for missing fields" );
      ( "/api/v1/projects/1/samples",
        {| {
              "name": "Invalid Source", 
              "sample_type": "DNA", 
              "category": "Source", 
              "parent_sample_id": null, 
              "strain_id": null,
              "result_definition_ids": []
           } |},
        "Reject Source without Strain (400)" );
      ( "/api/v1/projects/1/samples",
        {| {
              "name": "Invalid Exp", 
              "sample_type": "DNA", 
              "category": "Experimental", 
              "parent_sample_id": null,
              "strain_id": null,
              "result_definition_ids": []
           } |},
        "Reject Experimental without Parent (400)" );
      ( "/api/v1/projects/1/samples/bulk",
        {| { "samples": [ { "sample_type": "DNA", "category": "Source" } ] } |},
        "Should return 400 for missing 'name' in bulk" );
      ( "/api/v1/projects/1/samples/bulk",
        {| { "samples": [ { "name": "Invalid Bulk Exp", "sample_type": "DNA", "category": "Experimental", "parent_sample_id": null, "strain_id": null } ] } |},
        "Should return 400 for invalid experimental in bulk" );
    ]
  in
  let* _ =
    Lwt_list.iter_s
      (fun (path, body, msg) ->
        let* _ =
          Test_utils.assert_json_post ~handler ~path ~body ~expected_status:400
            msg
        in
        Lwt.return_unit)
      cases
  in
  Lwt.return ()

let test_sample_lifecycle _switch () =
  let open Lwt.Syntax in
  (* 1. Create a Source Sample (requires a strain) *)
  let strain_body =
    {| { "genus": "Escherichia", "species": "coli", "strain_name": "DH5a", "genotype": "foo", "notes": "bar", "links": [] } |}
  in
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:strain_body ~expected_status:201 "Create strain for sample"
  in
  let* strain_body_str = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body_str) |> to_int)
  in

  let source_body =
    Printf.sprintf
      {| { "sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": [] } |}
      strain_id
  in
  let* source_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:source_body ~expected_status:201 "Create source sample"
  in
  let* source_body_str = Dream.body source_res in
  let source_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string source_body_str) |> to_int)
  in

  (* 2. Create an Experimental Sample (requires a parent) *)
  let exp_body =
    Printf.sprintf
      {| { "sample_type": "Solid Cell Culture", "category": "Experimental", "parent_sample_id": %d, "result_definition_ids": [] } |}
      source_id
  in
  let* exp_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:exp_body ~expected_status:201 "Create experimental sample"
  in
  let* exp_body_str = Dream.body exp_res in
  let exp_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string exp_body_str) |> to_int)
  in

  (* 3. Fetch Sample Detailed *)
  let exp_path = Printf.sprintf "/api/v1/samples/%d" exp_id in
  let* _ =
    Test_utils.assert_json_get ~handler ~path:exp_path ~expected_status:200
      "Fetch sample detailed"
  in

  (* 4. Fetch Sample Children (from the source sample) *)
  let children_path = Printf.sprintf "/api/v1/samples/%d/children" source_id in
  let* children_res =
    Test_utils.assert_json_get ~handler ~path:children_path ~expected_status:200
      "Fetch sample children"
  in
  let* children_body = Dream.body children_res in
  let count =
    Yojson.Safe.Util.(
      member "count" (Yojson.Safe.from_string children_body) |> to_int)
  in
  Alcotest.(check int) "Source has 1 child" 1 count;

  (* 5. Update Sample *)
  let update_body =
    Printf.sprintf
      {| { "sample_type": "Unknown", "category": "Experimental", "parent_sample_id": %d, "strain_id": null, "community_id": null, "status": "Archived" } |}
      source_id
  in
  let* _ =
    Test_utils.assert_json_put ~handler ~path:exp_path ~body:update_body
      ~expected_status:200 "Update sample successful"
  in

  (* 6. Fetch All Samples *)
  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/samples"
      ~expected_status:200 "Fetch all samples"
  in
  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/projects/1/samples"
      ~expected_status:200 "Fetch project samples"
  in

  (* 7. Delete (Archive) Sample *)
  let* _ =
    Test_utils.assert_json_delete ~handler ~path:exp_path ~expected_status:204
      "Archive sample"
  in
  Lwt.return ()

let test_bad_csv_requests _switch () =
  let open Lwt.Syntax in
  let cases =
    [
      ( "/api/v1/projects/1/samples/bulk-csv",
        "name,sample_type,category\nSample A,DNA,Source\nSample B,RNA",
        "Should return 400 for malformed CSV data" );
      ( "/api/v1/projects/1/samples/bulk-csv",
        "sample_type,category\nDNA,Source",
        "Should return 400 for missing header" );
    ]
  in
  let* _ =
    Lwt_list.iter_s
      (fun (path, body, msg) ->
        let* _ =
          Test_utils.assert_csv_post ~handler ~path ~body ~expected_status:400
            msg
        in
        Lwt.return_unit)
      cases
  in
  Lwt.return ()

let test_search_samples _switch () =
  let open Lwt.Syntax in
  (* 1. Create a Strain *)
  let strain_body =
    {| { "genus": "SearchTest", "species": "coli", "strain_name": "S1", "links": [] } |}
  in
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:strain_body ~expected_status:201 "Create strain"
  in
  let* strain_body_str = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body_str) |> to_int)
  in

  (* 2. Create Sample A (to search for) *)
  let source_body_a =
    Printf.sprintf
      {| { "sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": [] } |}
      strain_id
  in
  let req_a =
    Test_utils.json_post ~path:"/api/v1/projects/1/samples" ~body:source_body_a
  in
  let response_a = Dream.test handler req_a in
  let* body_a_str = Dream.body response_a in
  let json_a = Yojson.Safe.from_string body_a_str in
  let short_id_a = Yojson.Safe.Util.(member "short_id" json_a |> to_string) in

  (* 3. Create Sample B (not matching) *)
  let source_body_b =
    Printf.sprintf
      {| { "sample_type": "Solid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": [] } |}
      strain_id
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:source_body_b ~expected_status:201 "Create sample B"
  in

  (* 4. Search project samples *)
  let search_path =
    Printf.sprintf "/api/v1/projects/1/samples?search=%s" short_id_a
  in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search project samples"
  in
  let* search_body = Dream.body search_res in
  let json = Yojson.Safe.from_string search_body in
  let data = Yojson.Safe.Util.(member "data" json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching sample in project" 1 (List.length data);

  let returned_short_id =
    Yojson.Safe.Util.(List.hd data |> member "short_id" |> to_string)
  in
  Alcotest.(check string)
    "Returned project sample short_id matches" short_id_a returned_short_id;

  (* 5. Search global samples *)
  let global_search_path =
    Printf.sprintf "/api/v1/samples?search=%s" short_id_a
  in
  let* global_search_res =
    Test_utils.assert_json_get ~handler ~path:global_search_path
      ~expected_status:200 "Search global samples"
  in
  let* global_search_body = Dream.body global_search_res in
  let global_json = Yojson.Safe.from_string global_search_body in
  let global_data = Yojson.Safe.Util.(member "data" global_json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching sample globally" 1
    (List.length global_data);
  let global_returned_short_id =
    Yojson.Safe.Util.(List.hd global_data |> member "short_id" |> to_string)
  in
  Alcotest.(check string)
    "Returned global sample short_id matches" short_id_a
    global_returned_short_id;

  Lwt.return ()

let test_advanced_search_samples _switch () =
  let open Lwt.Syntax in
  (* 1. Create a Strain *)
  let strain_body =
    {| { "genus": "SearchTest", "species": "coli", "strain_name": "S1", "links": [] } |}
  in
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:strain_body ~expected_status:201 "Create strain"
  in
  let* strain_body_str = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body_str) |> to_int)
  in

  (* 2. Create Sample A (matching category "Source") *)
  let source_body_a =
    Printf.sprintf
      {| { "sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": [] } |}
      strain_id
  in
  let* response_a =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:source_body_a ~expected_status:201 "Create sample A"
  in
  let* body_a_str = Dream.body response_a in
  let json_a = Yojson.Safe.from_string body_a_str in
  let short_id_a = Yojson.Safe.Util.(member "short_id" json_a |> to_string) in
  let id_a = Yojson.Safe.Util.(member "id" json_a |> to_int) in

  (* 3. Create Sample B (matching category "Experimental") *)
  let exp_body_b =
    Printf.sprintf
      {| { "sample_type": "Solid Cell Culture", "category": "Experimental", "parent_sample_id": %d, "result_definition_ids": [] } |}
      id_a
  in
  let* response_b =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:exp_body_b ~expected_status:201 "Create sample B"
  in
  let* body_b_str = Dream.body response_b in
  let json_b = Yojson.Safe.from_string body_b_str in
  let _short_id_b = Yojson.Safe.Util.(member "short_id" json_b |> to_string) in

  (* 4. Search global samples by short_id_a AND category Source *)
  let search_path =
    Printf.sprintf "/api/v1/samples?search=%s&category=Source" short_id_a
  in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search with category filter"
  in
  let* search_body = Dream.body search_res in
  let json = Yojson.Safe.from_string search_body in
  let data = Yojson.Safe.Util.(member "data" json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching sample" 1 (List.length data);
  let returned_short_id =
    Yojson.Safe.Util.(List.hd data |> member "short_id" |> to_string)
  in
  Alcotest.(check string)
    "Returned sample short_id matches" short_id_a returned_short_id;

  (* 5. Search global samples by short_id_a AND category Experimental (should return Sample B, because its short ID contains A's short ID) *)
  let search_path_exp =
    Printf.sprintf "/api/v1/samples?search=%s&category=Experimental" short_id_a
  in
  let* search_res_exp =
    Test_utils.assert_json_get ~handler ~path:search_path_exp
      ~expected_status:200 "Search with Experimental category filter"
  in
  let* search_body_exp = Dream.body search_res_exp in
  let json_exp = Yojson.Safe.from_string search_body_exp in
  let data_exp = Yojson.Safe.Util.(member "data" json_exp |> to_list) in
  Alcotest.(check int)
    "Should return exactly 1 matching sample (Sample B)" 1
    (List.length data_exp);

  (* 6. Search global samples by short_id_a AND specific project_id *)
  let search_path_proj =
    Printf.sprintf "/api/v1/samples?search=%s&project_id=1" short_id_a
  in
  let* search_res_proj =
    Test_utils.assert_json_get ~handler ~path:search_path_proj
      ~expected_status:200 "Search with Project filter"
  in
  let* search_body_proj = Dream.body search_res_proj in
  let json_proj = Yojson.Safe.from_string search_body_proj in
  let data_proj = Yojson.Safe.Util.(member "data" json_proj |> to_list) in
  Alcotest.(check int)
    "Should return exactly 2 matching samples in Project 1 (A and its child B)"
    2 (List.length data_proj);

  let search_path_proj_empty =
    Printf.sprintf "/api/v1/samples?search=%s&project_id=999" short_id_a
  in
  let* search_res_proj_empty =
    Test_utils.assert_json_get ~handler ~path:search_path_proj_empty
      ~expected_status:200 "Search with non-matching Project filter"
  in
  let* search_body_proj_empty = Dream.body search_res_proj_empty in
  let json_proj_empty = Yojson.Safe.from_string search_body_proj_empty in
  let data_proj_empty =
    Yojson.Safe.Util.(member "data" json_proj_empty |> to_list)
  in
  Alcotest.(check int)
    "Should return 0 matching samples in Project 999" 0
    (List.length data_proj_empty);

  (* 7. Search project with EMPTY search term *)
  let search_path_empty_query = "/api/v1/samples?search=&project_id=1" in
  let* search_res_empty_query =
    Test_utils.assert_json_get ~handler ~path:search_path_empty_query
      ~expected_status:200 "Search with empty search term and Project filter"
  in
  let* search_body_empty_query = Dream.body search_res_empty_query in
  let json_empty_query = Yojson.Safe.from_string search_body_empty_query in
  let data_empty_query =
    Yojson.Safe.Util.(member "data" json_empty_query |> to_list)
  in
  Alcotest.(check bool)
    "Should return > 0 matching samples in Project 1 with no search term" true
    (List.length data_empty_query > 0);

  (* 8. Search global samples by sample_type *)
  let search_path_type =
    Printf.sprintf
      "/api/v1/samples?sample_type=Liquid%%20Cell%%20Culture&project_id=1"
  in
  let* search_res_type =
    Test_utils.assert_json_get ~handler ~path:search_path_type
      ~expected_status:200 "Search with sample_type filter"
  in
  let* search_body_type = Dream.body search_res_type in
  let json_type = Yojson.Safe.from_string search_body_type in
  let data_type = Yojson.Safe.Util.(member "data" json_type |> to_list) in
  (* Sample A should be returned as it's Liquid Cell Culture *)
  let found_a_in_type =
    List.exists
      (fun item -> Yojson.Safe.Util.(member "id" item |> to_int) = id_a)
      data_type
  in
  Alcotest.(check bool)
    "Should find Sample A by sample_type" true found_a_in_type;

  (* 9. Search global samples by strain_id *)
  let search_path_strain_id =
    Printf.sprintf "/api/v1/samples?strain_id=%d" strain_id
  in
  let* search_res_strain_id =
    Test_utils.assert_json_get ~handler ~path:search_path_strain_id
      ~expected_status:200 "Search with strain_id filter"
  in
  let* search_body_strain_id = Dream.body search_res_strain_id in
  let json_strain_id = Yojson.Safe.from_string search_body_strain_id in
  let data_strain_id =
    Yojson.Safe.Util.(member "data" json_strain_id |> to_list)
  in
  (* Sample A has this strain_id *)
  Alcotest.(check bool)
    "Should return matching sample by strain_id" true
    (List.length data_strain_id > 0);
  let found_a_in_strain =
    List.exists
      (fun item -> Yojson.Safe.Util.(member "id" item |> to_int) = id_a)
      data_strain_id
  in
  Alcotest.(check bool)
    "Should find Sample A by strain_id" true found_a_in_strain;

  Lwt.return ()

let test_search_samples_by_strain _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let strain_name = Printf.sprintf "UniqueStrain%f" time in

  (* 1. Create a Strain with a unique name *)
  let strain_body =
    Printf.sprintf
      {| { "genus": "SearchTest", "species": "coli", "strain_name": "%s", "links": [] } |}
      strain_name
  in
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:strain_body ~expected_status:201 "Create strain for sample"
  in
  let* strain_body_str = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body_str) |> to_int)
  in

  (* 2. Create Sample with this strain *)
  let source_body =
    Printf.sprintf
      {| { "sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": [] } |}
      strain_id
  in
  let* response =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:source_body ~expected_status:201 "Create sample with strain"
  in
  let* body_str = Dream.body response in
  let json = Yojson.Safe.from_string body_str in
  let sample_id = Yojson.Safe.Util.(member "id" json |> to_int) in

  (* 3. Search global samples by strain_name *)
  let search_path = Printf.sprintf "/api/v1/samples?search=%s" strain_name in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search with strain name"
  in
  let* search_body = Dream.body search_res in
  Printf.printf "SEARCH BODY FOR STRAIN: %s\n" search_body;
  let search_json = Yojson.Safe.from_string search_body in
  let data = Yojson.Safe.Util.(member "data" search_json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching sample" 1 (List.length data);
  let returned_id = Yojson.Safe.Util.(List.hd data |> member "id" |> to_int) in
  Alcotest.(check int) "Returned sample ID matches" sample_id returned_id;

  (* 4. Search project samples by strain name *)
  let search_proj_path =
    Printf.sprintf "/api/v1/projects/1/samples?search=%s" strain_name
  in
  let* search_proj_res =
    Test_utils.assert_json_get ~handler ~path:search_proj_path
      ~expected_status:200 "Search project with strain name"
  in
  let* search_proj_body = Dream.body search_proj_res in
  let search_proj_json = Yojson.Safe.from_string search_proj_body in
  let proj_data =
    Yojson.Safe.Util.(member "data" search_proj_json |> to_list)
  in

  Alcotest.(check int)
    "Should return exactly 1 matching sample in project" 1
    (List.length proj_data);

  (* 5. Filter global samples by strain_id *)
  let filter_path = Printf.sprintf "/api/v1/samples?strain_id=%d" strain_id in
  let* filter_res =
    Test_utils.assert_json_get ~handler ~path:filter_path ~expected_status:200
      "Filter samples by strain_id"
  in
  let* filter_body = Dream.body filter_res in
  let filter_json = Yojson.Safe.from_string filter_body in
  let filter_data = Yojson.Safe.Util.(member "data" filter_json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching sample by strain_id" 1
    (List.length filter_data);

  Lwt.return ()

let test_bulk_csv_dry_run_samples _switch () =
  let open Lwt.Syntax in
  let unique_strain_name =
    Printf.sprintf "DryRunStrain%f" (Unix.gettimeofday ())
  in
  let csv_body =
    Printf.sprintf
      "sample_type,category,genus,species,strain_name\n\
       Liquid Cell Culture,Source,Escherichia,coli,%s\n\
       Liquid Cell Culture,Source,Escherichia,coli,%s"
      unique_strain_name unique_strain_name
  in
  let* res =
    Test_utils.assert_csv_post ~handler
      ~path:"/api/v1/projects/1/samples/bulk-csv?dry_run=true" ~body:csv_body
      ~expected_status:201 "Dry run bulk CSV upload"
  in
  let* res_body = Dream.body res in
  let json = Yojson.Safe.from_string res_body in
  let summary = Yojson.Safe.Util.(member "summary" json) in
  let created_samples =
    Yojson.Safe.Util.(member "created_samples" summary |> to_int)
  in
  let created_strains =
    Yojson.Safe.Util.(member "created_strains" summary |> to_int)
  in
  let linked_strains =
    Yojson.Safe.Util.(member "linked_strains" summary |> to_int)
  in

  Alcotest.(check int) "Should simulate 2 samples" 2 created_samples;
  Alcotest.(check int) "Should simulate 1 new strain" 1 created_strains;
  Alcotest.(check int) "Should simulate 1 linked strain" 1 linked_strains;

  (* Verify database state: the strain should not exist *)
  let search_path =
    Printf.sprintf "/api/v1/strains?search=%s" unique_strain_name
  in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search for dry run strain"
  in
  let* search_body = Dream.body search_res in
  let search_json = Yojson.Safe.from_string search_body in
  let count = Yojson.Safe.Util.(member "count" search_json |> to_int) in
  Alcotest.(check int) "Strain should not actually be created" 0 count;

  Lwt.return ()

let test_bulk_csv_parent_short_id _switch () =
  let open Lwt.Syntax in
  (* 1. Create a strain *)
  let time = Unix.gettimeofday () in
  let strain_name = Printf.sprintf "ShortIdStrain%f" time in
  let strain_body =
    Printf.sprintf
      {| { "genus": "Test", "species": "coli", "strain_name": "%s", "links": [] } |}
      strain_name
  in
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:strain_body ~expected_status:201 "Create strain for parent test"
  in
  let* strain_body_str = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body_str) |> to_int)
  in

  (* 2. Create a source sample *)
  let source_body =
    Printf.sprintf
      {| { "sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": [] } |}
      strain_id
  in
  let* source_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:source_body ~expected_status:201 "Create source sample"
  in
  let* source_body_str = Dream.body source_res in
  let source_json = Yojson.Safe.from_string source_body_str in
  let source_id = Yojson.Safe.Util.(member "id" source_json |> to_int) in
  let source_short_id =
    Yojson.Safe.Util.(member "short_id" source_json |> to_string)
  in

  (* 3. Create experimental sample via CSV using parent_sample_short_id *)
  let csv_body =
    Printf.sprintf
      "sample_type,category,parent_sample_short_id\n\
       Solid Cell Culture,Experimental,%s"
      source_short_id
  in
  let* res =
    Test_utils.assert_csv_post ~handler
      ~path:"/api/v1/projects/1/samples/bulk-csv" ~body:csv_body
      ~expected_status:201 "Create experimental with parent short id"
  in
  let* res_body = Dream.body res in
  let json = Yojson.Safe.from_string res_body in
  let samples = Yojson.Safe.Util.(member "samples" json |> to_list) in

  Alcotest.(check int) "Should create 1 sample" 1 (List.length samples);
  let created_sample = List.hd samples in
  let created_parent_id =
    Yojson.Safe.Util.(member "parent_sample_id" created_sample |> to_int_option)
  in

  Alcotest.(check (option int))
    "Parent ID matches" (Some source_id) created_parent_id;

  Lwt.return ()

let suite =
  [
    ( "Sample API",
      [
        Alcotest_lwt.test_case "Sample Lifecycle" `Quick test_sample_lifecycle;
        Alcotest_lwt.test_case "Search Samples" `Quick test_search_samples;
        Alcotest_lwt.test_case "Advanced Search Samples" `Quick
          test_advanced_search_samples;
        Alcotest_lwt.test_case "Search Samples By Strain" `Quick
          test_search_samples_by_strain;
        Alcotest_lwt.test_case "Reject Bad JSON Requests" `Quick
          test_bad_json_requests;
        Alcotest_lwt.test_case "Reject Bad CSV Requests" `Quick
          test_bad_csv_requests;
        Alcotest_lwt.test_case "Dry Run CSV Upload" `Quick
          test_bulk_csv_dry_run_samples;
        Alcotest_lwt.test_case "Bulk CSV Parent Short ID" `Quick
          test_bulk_csv_parent_short_id;
      ] );
  ]
