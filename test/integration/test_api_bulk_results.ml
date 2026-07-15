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

  (* We use an existing seeded sample: "1" assuming basic seed structure, 
     or anything that would trigger validation BEFORE target resolution if we sequence correctly. 
     Actually, if target resolution happens first, this would fail 404 if 1 doesn't exist.
     But we want to test validation. Let's create a dummy target just in case, but since we are 
     testing validation failure, we can just let it fail on 400 before or after.
  *)
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
      ] );
  ]
