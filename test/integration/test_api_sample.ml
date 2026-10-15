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

let test_concurrent_sample_short_ids _switch () =
  let open Lwt.Syntax in
  let unwrap msg = function
    | Ok v -> v
    | Error _ -> Alcotest.fail ("Unexpected storage error in " ^ msg)
  in
  let proj, s1_short, created_sources, created_exps =
    Lwt_main.run
      (let* proj_res =
         Exlab_storage.Project.add ~name:"Concurrent Sample Project"
           ~description:None ~status:Exlab_core.Types.Active ~contact_name:None
           ~owner:None ()
       in
       let proj = unwrap "Project.add" proj_res in

       let* strain_res =
         Exlab_storage.Strain.add ~genus:"Escherichia" ~species:"coli"
           ~strain_name:"ConcStrain" ~genotype:None ~parent_strain_id:None
           ~notes:None
       in
       let strain = unwrap "Strain.add" strain_res in

       (* 1. Concurrently create 8 Source samples in the same project across separate pool connections *)
       let create_source () =
         let* res =
           Exlab_storage.Sample.add ~project_id:proj.id
             ~sample_type:"Liquid Cell Culture"
             ~category:Exlab_core.Types.Source ~parent_sample_id:None
             ~strain_id:(Some strain.id) ~community_id:None ()
         in
         match res with
         | Ok s -> Lwt.return (s.id, s.short_id)
         | Error _ ->
             Alcotest.fail
               "Concurrent Sample.add Source failed (race collision)"
       in
       let* created_sources =
         Lwt_list.map_p (fun () -> create_source ()) (List.init 8 (fun _ -> ()))
       in
       let s1_id, s1_short = List.hd created_sources in

       (* 2. Concurrently create 8 Experimental samples under the same parent *)
       let create_exp () =
         let* res =
           Exlab_storage.Sample.add ~project_id:proj.id
             ~sample_type:"Liquid Cell Culture"
             ~category:Exlab_core.Types.Experimental
             ~parent_sample_id:(Some s1_id) ~strain_id:None ~community_id:None
             ()
         in
         match res with
         | Ok s -> Lwt.return s.short_id
         | Error _ ->
             Alcotest.fail
               "Concurrent Sample.add Experimental failed (race collision)"
       in
       let* created_exps =
         Lwt_list.map_p (fun () -> create_exp ()) (List.init 8 (fun _ -> ()))
       in
       Lwt.return (proj, s1_short, created_sources, created_exps))
  in
  let src_ids = created_sources |> List.map snd |> List.sort String.compare in
  let expected_src_ids =
    List.init 8 (fun i -> Printf.sprintf "%s-%04d" proj.short_id (i + 1))
  in
  Alcotest.(check (list string))
    "Concurrent Source samples get distinct consecutive short IDs"
    expected_src_ids src_ids;

  let exp_ids = List.sort String.compare created_exps in
  let expected_exp_ids =
    List.init 8 (fun i -> Printf.sprintf "%s-%02d" s1_short (i + 1))
  in
  Alcotest.(check (list string))
    "Concurrent Experimental samples get distinct consecutive short IDs"
    expected_exp_ids exp_ids;
  Lwt.return ()

let test_mixed_bulk_create_many _switch () =
  let open Lwt.Syntax in
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{| {"name": "Mixed Bulk Sample Project", "status": "Active"} |}
      ~expected_status:201 "Create project"
  in
  let* proj_body = Dream.body proj_res in
  let proj_json = Yojson.Safe.from_string proj_body in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_short =
    Yojson.Safe.Util.(member "short_id" proj_json |> to_string)
  in

  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        {| { "genus": "Escherichia", "species": "coli", "strain_name": "MixedBulkStrain", "links": [] } |}
      ~expected_status:201 "Create strain"
  in
  let* strain_body = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body) |> to_int)
  in

  let samples_path = Printf.sprintf "/api/v1/projects/%d/samples" proj_id in
  let src_body =
    Printf.sprintf
      {| { "sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": [] } |}
      strain_id
  in
  let* p_a_res =
    Test_utils.assert_json_post ~handler ~path:samples_path ~body:src_body
      ~expected_status:201 "Create Parent A"
  in
  let* p_a_body = Dream.body p_a_res in
  let p_a_json = Yojson.Safe.from_string p_a_body in
  let p_a_short = Yojson.Safe.Util.(member "short_id" p_a_json |> to_string) in

  let* p_b_res =
    Test_utils.assert_json_post ~handler ~path:samples_path ~body:src_body
      ~expected_status:201 "Create Parent B"
  in
  let* p_b_body = Dream.body p_b_res in
  let p_b_json = Yojson.Safe.from_string p_b_body in
  let p_b_short = Yojson.Safe.Util.(member "short_id" p_b_json |> to_string) in

  (* Interleaved batch: Child of B, Source 3, Child of A, Child of B, Source 4, Child of A *)
  let bulk_csv =
    Printf.sprintf
      "sample_type,category,parent_sample_short_id,strain_id\n\
       Liquid Cell Culture,Experimental,%s,\n\
       Liquid Cell Culture,Source,,%d\n\
       Liquid Cell Culture,Experimental,%s,\n\
       Liquid Cell Culture,Experimental,%s,\n\
       Liquid Cell Culture,Source,,%d\n\
       Liquid Cell Culture,Experimental,%s,"
      p_b_short strain_id p_a_short p_b_short strain_id p_a_short
  in
  let bulk_path =
    Printf.sprintf "/api/v1/projects/%d/samples/bulk-csv" proj_id
  in
  let* bulk_res =
    Test_utils.assert_csv_post ~handler ~path:bulk_path ~body:bulk_csv
      ~expected_status:201 "Mixed bulk CSV upload"
  in
  let* bulk_body = Dream.body bulk_res in
  let created_short_ids =
    Yojson.Safe.from_string bulk_body
    |> Yojson.Safe.Util.member "samples"
    |> Yojson.Safe.Util.to_list
    |> List.map (fun s -> Yojson.Safe.Util.(member "short_id" s |> to_string))
  in
  let expected_short_ids =
    [
      Printf.sprintf "%s-01" p_b_short;
      Printf.sprintf "%s-0003" proj_short;
      Printf.sprintf "%s-01" p_a_short;
      Printf.sprintf "%s-02" p_b_short;
      Printf.sprintf "%s-0004" proj_short;
      Printf.sprintf "%s-02" p_a_short;
    ]
  in
  Alcotest.(check (list string))
    "Mixed batch preserves submission order and increments each scope"
    expected_short_ids created_short_ids;
  Lwt.return ()

let with_forced_rollback f =
  let open Lwt.Syntax in
  let captured = ref None in
  let tx_res =
    Lwt_main.run
      (Exlab_storage.Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
           let* res = f (module Conn : Caqti_lwt.CONNECTION) in
           captured := Some res;
           Lwt.return (Error `Force_rollback)))
  in
  match (tx_res, !captured) with
  | Error `Force_rollback, Some res -> res
  | Error (#Caqti_error.t as e), _ -> Error e
  | _ -> Alcotest.fail "Expected forced rollback"

let test_m008_migration_backfill_and_guards _switch () =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  (* 1. Reparented sample backfill test (always rolled back) *)
  let reparent_res =
    with_forced_rollback (fun (module Conn : Caqti_lwt.CONNECTION) ->
        let open Lwt_result.Syntax in
        let* proj =
          Exlab_storage.Project.insert
            (module Conn)
            ~name:"Backfill Test Proj" ~description:None
            ~status:Exlab_core.Types.Active ~contact_name:None ~owner:None ()
        in
        let* strain =
          Exlab_storage.Strain.insert
            (module Conn)
            ~genus:"Escherichia" ~species:"coli" ~strain_name:"BF"
            ~genotype:None ~parent_strain_id:None ~notes:None
        in
        let ins_sample =
          (t6 string string int string (option int) (option int) ->! int)
            "INSERT INTO samples (uid, short_id, project_id, sample_type, \
             category, parent_sample_id, strain_id, created_at) VALUES (?, ?, \
             ?, 'Unknown', ?, ?, ?, 0.0) RETURNING id"
        in
        let a_short = proj.short_id ^ "-0001" in
        let b_short = proj.short_id ^ "-0002" in
        let* a_id =
          Conn.find ins_sample
            ( Exlab_storage.Utils.make_uuid (),
              a_short,
              proj.id,
              "Source",
              None,
              Some strain.id )
        in
        let* b_id =
          Conn.find ins_sample
            ( Exlab_storage.Utils.make_uuid (),
              b_short,
              proj.id,
              "Source",
              None,
              Some strain.id )
        in
        (* Insert A-01, A-02 under A, and A-03 reparented to B *)
        let* _ =
          Conn.find ins_sample
            ( Exlab_storage.Utils.make_uuid (),
              a_short ^ "-01",
              proj.id,
              "Experimental",
              Some a_id,
              Some strain.id )
        in
        let* _ =
          Conn.find ins_sample
            ( Exlab_storage.Utils.make_uuid (),
              a_short ^ "-02",
              proj.id,
              "Experimental",
              Some a_id,
              Some strain.id )
        in
        let* _ =
          Conn.find ins_sample
            ( Exlab_storage.Utils.make_uuid (),
              a_short ^ "-03",
              proj.id,
              "Experimental",
              Some b_id,
              Some strain.id )
        in
        (* Also insert S-<proj.id>-0004 and P-<proj.id>-0007 plates *)
        let ins_plate =
          (t3 string string int ->. unit)
            "INSERT INTO plates (uid, short_id, name, project_id, \
             plate_format, created_at) VALUES (?, ?, 'BF Plate', ?, '96-well', \
             0.0)"
        in
        let* () =
          Conn.exec ins_plate
            ( Exlab_storage.Utils.make_uuid (),
              Printf.sprintf "S-%d-0004" proj.id,
              proj.id )
        in
        let* () =
          Conn.exec ins_plate
            ( Exlab_storage.Utils.make_uuid (),
              Printf.sprintf "P-%d-0007" proj.id,
              proj.id )
        in
        (* Clear only the specific (scope, scope_id) pairs for this fixture *)
        let del_counter =
          (t2 string int ->. unit)
            "DELETE FROM short_id_counters WHERE scope = ? AND scope_id = ?"
        in
        let* () = Conn.exec del_counter ("source_sample", proj.id) in
        let* () = Conn.exec del_counter ("source_plate", proj.id) in
        let* () = Conn.exec del_counter ("experimental_plate", proj.id) in
        let* () = Conn.exec del_counter ("experimental_sample", a_id) in
        let* () = Conn.exec del_counter ("experimental_sample", b_id) in
        let* () = Exlab_storage.Migrate.m008_short_id_counters (module Conn) in
        let get_counter =
          (t2 string int ->? int)
            "SELECT last_value FROM short_id_counters WHERE scope = ? AND \
             scope_id = ?"
        in
        let* a_counter =
          Conn.find_opt get_counter ("experimental_sample", a_id)
        in
        let* b_counter =
          Conn.find_opt get_counter ("experimental_sample", b_id)
        in
        let* proj_counter =
          Conn.find_opt get_counter ("source_sample", proj.id)
        in
        let* s_plate_counter =
          Conn.find_opt get_counter ("source_plate", proj.id)
        in
        let* p_plate_counter =
          Conn.find_opt get_counter ("experimental_plate", proj.id)
        in
        Lwt.return
          (Ok
             ( a_counter,
               b_counter,
               proj_counter,
               s_plate_counter,
               p_plate_counter )))
  in
  (match reparent_res with
  | Error (`Not_Found m) -> Alcotest.fail ("Backfill test failed: " ^ m)
  | Error (#Caqti_error.t as e) ->
      Alcotest.fail ("Backfill test failed: " ^ Caqti_error.show e)
  | Ok (a_counter, b_counter, proj_counter, s_plate_counter, p_plate_counter) ->
      Alcotest.(check (option int))
        "Parent A counter reflects reparented A-03" (Some 3) a_counter;
      Alcotest.(check (option int))
        "Parent B has no counter row since no B-NN exists" None b_counter;
      Alcotest.(check (option int))
        "Project source_sample counter is 2" (Some 2) proj_counter;
      Alcotest.(check (option int))
        "Project source_plate counter is 4" (Some 4) s_plate_counter;
      Alcotest.(check (option int))
        "Project experimental_plate counter is 7" (Some 7) p_plate_counter);

  (* 2. Duplicate plates.short_id guard test (always rolled back) *)
  let dup_plate_res =
    with_forced_rollback (fun (module Conn : Caqti_lwt.CONNECTION) ->
        let open Lwt_result.Syntax in
        let* proj =
          Exlab_storage.Project.insert
            (module Conn)
            ~name:"Dup Plate Proj" ~description:None
            ~status:Exlab_core.Types.Active ~contact_name:None ~owner:None ()
        in
        let drop_uq =
          (unit ->. unit)
            "ALTER TABLE plates DROP CONSTRAINT plates_short_id_key"
        in
        let* () = Conn.exec drop_uq () in
        let ins_plate =
          (t2 string int ->. unit)
            "INSERT INTO plates (uid, short_id, name, project_id, \
             plate_format, created_at) VALUES (?, 'DUP-PLT-1', 'Dup', ?, \
             '96-well', 0.0)"
        in
        let* () =
          Conn.exec ins_plate (Exlab_storage.Utils.make_uuid (), proj.id)
        in
        let* () =
          Conn.exec ins_plate (Exlab_storage.Utils.make_uuid (), proj.id)
        in
        Exlab_storage.Migrate.m008_short_id_counters (module Conn))
  in
  (match dup_plate_res with
  | Ok () -> Alcotest.fail "Expected duplicate plates guard to fail"
  | Error (`Not_Found m) -> Alcotest.fail ("Unexpected Not_Found: " ^ m)
  | Error (#Caqti_error.t as err) ->
      let msg = Caqti_error.show err in
      Alcotest.(check bool)
        "Error mentions Duplicate plates.short_id" true
        (Re.execp (Re.compile (Re.str "Duplicate plates.short_id")) msg));

  (* 3. Ambiguous sample prefix guard test (always rolled back) *)
  let ambig_res =
    with_forced_rollback (fun (module Conn : Caqti_lwt.CONNECTION) ->
        let open Lwt_result.Syntax in
        let shared_prefix = Printf.sprintf "AMBIG-%d" (Random.int 1000000) in
        let* proj =
          Exlab_storage.Project.insert
            (module Conn)
            ~short_id:shared_prefix ~name:"Ambig Proj" ~description:None
            ~status:Exlab_core.Types.Active ~contact_name:None ~owner:None ()
        in
        let ins_sample =
          (t3 string string int ->. unit)
            "INSERT INTO samples (uid, short_id, project_id, sample_type, \
             category, created_at) VALUES (?, ?, ?, 'Unknown', 'Source', 0.0)"
        in
        let* () =
          Conn.exec ins_sample
            (Exlab_storage.Utils.make_uuid (), shared_prefix, proj.id)
        in
        let* () =
          Conn.exec ins_sample
            (Exlab_storage.Utils.make_uuid (), shared_prefix ^ "-01", proj.id)
        in
        Exlab_storage.Migrate.m008_short_id_counters (module Conn))
  in
  (match ambig_res with
  | Ok () -> Alcotest.fail "Expected ambiguous prefix guard to fail"
  | Error (`Not_Found m) -> Alcotest.fail ("Unexpected Not_Found: " ^ m)
  | Error (#Caqti_error.t as err) ->
      let msg = Caqti_error.show err in
      Alcotest.(check bool)
        "Error mentions Ambiguous sample short_id prefix" true
        (Re.execp (Re.compile (Re.str "Ambiguous sample short_id prefix")) msg));
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
        Alcotest_lwt.test_case "Concurrent Sample Short IDs" `Quick
          test_concurrent_sample_short_ids;
        Alcotest_lwt.test_case "Mixed Bulk Create Many" `Quick
          test_mixed_bulk_create_many;
        Alcotest_lwt.test_case "Migration v8 Backfill and Guards" `Quick
          test_m008_migration_backfill_and_guards;
      ] );
  ]
