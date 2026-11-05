(* Test Definitions for API endpoint /api/results/bulk-csv *)

let handler = Test_utils.admin_app

(* Helper to create a test Result Definition and return its Short ID *)
let create_test_result_definition handler data_type =
  let short_id =
    "bulk_csv_test_def_" ^ data_type ^ string_of_float (Unix.gettimeofday ())
  in
  let body =
    Printf.sprintf
      {| {
         "short_id": "%s",
         "name": "Bulk Test Result Definition %s %f",
         "data_type": "%s",
         "is_required": false
       }
       |}
      short_id data_type (Unix.gettimeofday ()) data_type
  in
  let req = Test_utils.json_post ~path:"/api/v1/result-definitions" ~body in
  let response = Dream.test handler req in

  Alcotest.(check int)
    "Should create result definition" 201
    (Dream.status response |> Dream.status_to_int);

  Lwt.return short_id

let test_bulk_create_csv_missing_target_header _switch () =
  let csv_body = "def-0001,def-0002\n12.5,test" in
  let req =
    Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv_body
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing target header" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_create_csv_no_data _switch () =
  let csv_body = "sample_short_id,def-0001" in
  let req =
    Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv_body
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for CSV with no data rows" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_create_csv_invalid_target_id _switch () =
  let csv_body = "sample_short_id,def-0001\nSMP-INVALID-1234,12.5" in
  let req =
    Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv_body
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 404 for invalid target short_id" 404
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_create_csv_invalid_def_id _switch () =
  let csv_body = "sample_short_id,DEF-INVALID-1234\n1,12.5" in
  let req =
    Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv_body
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 404 for invalid result definition short_id" 404
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_create_csv_mismatched_data_type _switch () =
  (* Create a Float definition to test failure with "hello" *)
  let%lwt def_short_id = create_test_result_definition handler "Float" in

  let sample_body =
    {| {
    "sample_type": "Unknown",
    "category": "Experimental"
    ,"parent_sample_id": 1
  } |}
  in
  let s_req =
    Test_utils.json_post ~path:"/api/v1/projects/1/samples" ~body:sample_body
  in
  let s_res = Dream.test handler s_req in
  let s_body = Dream.body s_res |> Lwt_main.run in
  let s_json = Yojson.Safe.from_string s_body in
  let sample_short_id =
    Yojson.Safe.Util.(s_json |> member "short_id" |> to_string)
  in

  (* Use an existing seeded sample. The value must not exist if target
     resolution runs before validation, which would yield a 404; for a
     validation-failure test either ordering is acceptable. *)
  let csv_body =
    Printf.sprintf "sample_short_id,%s\n%s,hello" def_short_id sample_short_id
  in
  let req =
    Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv_body
  in
  let res = Dream.test handler req in

  (* Expect 400 for validation failure rather than 404 for missing target, 
     we want to ensure payload_of_string catches the 'hello' as invalid float. *)
  Alcotest.(check int)
    "Should return 400 for mismatched data type" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_create_csv_invalid_plate_id _switch () =
  let csv_body = "plate_short_id,def-0001\nPLT-INVALID-1234,12.5" in

  let req =
    Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv_body
  in

  let res = Dream.test handler req in

  Alcotest.(check int)
    "Should return 404 for invalid target plate short_id" 404
    (Dream.status res |> Dream.status_to_int);

  Lwt.return ()

let test_bulk_create_csv_valid_plate _switch () =
  let%lwt def_short_id = create_test_result_definition handler "String" in

  let plate_body =
    {| {
    "name": "Test Plate Results",
    "project_id": 1,
    "plate_format": "96-well",
    "category": "Experimental"
  } |}
  in

  let p_req = Test_utils.json_post ~path:"/api/v1/plates" ~body:plate_body in

  let p_res = Dream.test handler p_req in

  let p_body = Dream.body p_res |> Lwt_main.run in

  let p_json = Yojson.Safe.from_string p_body in

  let plate_short_id =
    Yojson.Safe.Util.(p_json |> member "short_id" |> to_string)
  in

  let csv_body =
    Printf.sprintf "plate_short_id,%s\n%s,Passed" def_short_id plate_short_id
  in

  let req =
    Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv_body
  in

  let res = Dream.test handler req in

  Alcotest.(check int)
    "Should return 201 for valid plate result creation" 201
    (Dream.status res |> Dream.status_to_int);

  Lwt.return ()

let test_bulk_csv_upsert_overwrites_existing _switch () =
  let open Lwt.Syntax in
  let* def1_short_id = create_test_result_definition handler "Float" in
  let* def2_short_id = create_test_result_definition handler "String" in

  let sample_body =
    {| {
    "sample_type": "Liquid Cell Culture",
    "category": "Experimental",
    "parent_sample_id": 1
  } |}
  in
  let* s_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:sample_body ~expected_status:201
      "Create sample for bulk upsert test"
  in
  let* s_body = Dream.body s_res in
  let s_json = Yojson.Safe.from_string s_body in
  let sample_id = Yojson.Safe.Util.(s_json |> member "id" |> to_int) in
  let sample_short_id =
    Yojson.Safe.Util.(s_json |> member "short_id" |> to_string)
  in

  let csv1 =
    Printf.sprintf "sample_short_id,%s,%s\n%s,1.5,Initial" def1_short_id
      def2_short_id sample_short_id
  in
  let req1 = Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv1 in
  let res1 = Dream.test handler req1 in
  Alcotest.(check int)
    "First bulk CSV upload should return 201" 201
    (Dream.status res1 |> Dream.status_to_int);

  let csv2 =
    Printf.sprintf "sample_short_id,%s,%s\n%s,3.75,Updated" def1_short_id
      def2_short_id sample_short_id
  in
  let req2 = Test_utils.csv_post ~path:"/api/v1/results/bulk-csv" ~body:csv2 in
  let res2 = Dream.test handler req2 in
  Alcotest.(check int)
    "Second bulk CSV upload should return 201" 201
    (Dream.status res2 |> Dream.status_to_int);

  let sample_res_path = Printf.sprintf "/api/v1/samples/%d/results" sample_id in
  let* get_res =
    Test_utils.assert_json_get ~handler ~path:sample_res_path
      ~expected_status:200 "Fetch sample results after second bulk upload"
  in
  let* get_str = Dream.body get_res in
  let get_json = Yojson.Safe.from_string get_str in
  let count = Yojson.Safe.Util.(member "count" get_json |> to_int) in
  Alcotest.(check int)
    "Sample should still have 2 result values (not 4)" 2 count;
  Lwt.return ()

let suite =
  [
    ( "Bulk Results CSV API",
      [
        Alcotest_lwt.test_case "Bulk CSV: Missing Target Header" `Quick
          test_bulk_create_csv_missing_target_header;
        Alcotest_lwt.test_case "Bulk CSV: No Data Rows" `Quick
          test_bulk_create_csv_no_data;
        Alcotest_lwt.test_case "Bulk CSV: Invalid Target ID" `Quick
          test_bulk_create_csv_invalid_target_id;
        Alcotest_lwt.test_case "Bulk CSV: Invalid Def ID" `Quick
          test_bulk_create_csv_invalid_def_id;
        Alcotest_lwt.test_case "Bulk CSV: Mismatched Data Type" `Quick
          test_bulk_create_csv_mismatched_data_type;
        Alcotest_lwt.test_case "Bulk CSV: Invalid Plate Target ID" `Quick
          test_bulk_create_csv_invalid_plate_id;
        Alcotest_lwt.test_case "Bulk CSV: Valid Plate Import" `Quick
          test_bulk_create_csv_valid_plate;
        Alcotest_lwt.test_case "Bulk CSV: Upsert Overwrites Existing" `Quick
          test_bulk_csv_upsert_overwrites_existing;
      ] );
  ]
