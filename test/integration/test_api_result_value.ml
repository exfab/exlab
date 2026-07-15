(* Test Definitions for API endpoint /api/samples/:id/results *)

let handler = Test_utils.admin_app

let create_test_plate handler =
  let body =
    {| {

    "name": "Export Test Plate",

    "plate_format": "96-well",

    "project_id": 1

  } |}
  in

  let req = Test_utils.json_post ~path:"/api/v1/plates" ~body in

  let res = Dream.test handler req in

  let json = Dream.body res |> Lwt_main.run |> Yojson.Safe.from_string in

  Lwt.return Yojson.Safe.Util.(member "id" json |> to_int |> string_of_int)

let create_test_result_definition handler data_type =
  let name =
    "Export Def " ^ data_type ^ string_of_float (Unix.gettimeofday ())
  in

  let body =
    Printf.sprintf
      {| {

    "name": "%s",
    "short_id": "%s",

    "data_type": "%s",

    "is_required": false

  } |}
      name
      (string_of_float (Unix.gettimeofday ()))
      data_type
  in

  let req = Test_utils.json_post ~path:"/api/v1/result-definitions" ~body in

  let res = Dream.test handler req in

  let json = Dream.body res |> Lwt_main.run |> Yojson.Safe.from_string in

  Lwt.return Yojson.Safe.Util.(member "id" json |> to_int |> string_of_int)

let test_create_malformed_json _switch () =
  let body = {| {"result_definition_id": 1, |} in
  let req = Test_utils.json_post ~path:"/api/v1/samples/1/results" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_fields _switch () =
  let body = {| {"value": {"type": "String", "value": "A"}} |} in
  let req = Test_utils.json_post ~path:"/api/v1/samples/1/results" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing fields" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_export_plate_results_csv _switch () =
  let%lwt plate_id = create_test_plate handler in

  let%lwt def_id = create_test_result_definition handler "String" in

  let result_body =
    Printf.sprintf
      {| {
    "result_definition_id": %s,
    "value": {"type": "String", "value": "Passed"}
  } |}
      def_id
  in

  let post_req =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/plates/%s/results" plate_id)
      ~body:result_body
  in

  let _ = Dream.test handler post_req in

  let get_req =
    Dream.request ~method_:`GET
      ~target:(Printf.sprintf "/api/v1/plates/%s/results?format=csv" plate_id)
      ""
  in

  let get_res = Dream.test handler get_req in

  Alcotest.(check int)
    "Should return 200 for plate results CSV export" 200
    (Dream.status get_res |> Dream.status_to_int);

  let body = Dream.body get_res |> Lwt_main.run in

  Alcotest.(check bool)
    "CSV should contain Result Type header" true
    (String.starts_with ~prefix:"Result Type,Value,Updated At" body);

  Lwt.return ()

let test_result_lifecycle _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let def_name = Printf.sprintf "Test Def %f" time in
  let def_short = Printf.sprintf "DEF-%f" time in

  (* 1. Create a Result Definition *)
  let def_body =
    Printf.sprintf
      {| {
    "name": "%s",
    "short_id": "%s",
    "data_type": "Float",
    "is_required": false
  } |}
      def_name def_short
  in
  let* def_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/result-definitions"
      ~body:def_body ~expected_status:201 "Create definition successful"
  in
  let* def_body_str = Dream.body def_res in
  let def_json = Yojson.Safe.from_string def_body_str in
  let def_id = Yojson.Safe.Util.(member "id" def_json |> to_int) in

  (* Fetch definition *)
  let def_path = Printf.sprintf "/api/v1/result-definitions/%d" def_id in
  let* _ =
    Test_utils.assert_json_get ~handler ~path:def_path ~expected_status:200
      "Fetch definition successful"
  in

  (* Create sample to attach results to *)
  let strain_body =
    {| { "genus": "Escherichia", "species": "coli", "strain_name": "DH5a" } |}
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

  let sample_body =
    Printf.sprintf
      {| { "sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": [] } |}
      strain_id
  in
  let* sample_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects/1/samples"
      ~body:sample_body ~expected_status:201 "Create source sample"
  in
  let* sample_body_str = Dream.body sample_res in
  let sample_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string sample_body_str) |> to_int)
  in

  (* 2. Create Result Value for Sample *)
  let res_body =
    Printf.sprintf
      {| {
    "result_definition_id": %d,
    "value": {"type": "Float", "value": 3.14159}
  } |}
      def_id
  in
  let sample_res_path = Printf.sprintf "/api/v1/samples/%d/results" sample_id in
  let* val_res =
    Test_utils.assert_json_post ~handler ~path:sample_res_path ~body:res_body
      ~expected_status:201 "Create result value successful"
  in
  let* val_body_str = Dream.body val_res in
  let val_json = Yojson.Safe.from_string val_body_str in
  let val_id = Yojson.Safe.Util.(member "id" val_json |> to_int) in

  (* 3. Fetch Results for Sample *)
  let* _ =
    Test_utils.assert_json_get ~handler ~path:sample_res_path
      ~expected_status:200 "Fetch sample results successful"
  in

  (* 4. Update Result Value *)
  let val_path = Printf.sprintf "/api/v1/results/%d" val_id in
  let update_res_body =
    {| {
    "value": {"type": "Float", "value": 2.718}
  } |}
  in
  let* _ =
    Test_utils.assert_json_put ~handler ~path:val_path ~body:update_res_body
      ~expected_status:200 "Update result value successful"
  in

  Lwt.return ()

let suite =
  [
    ( "Result Value API",
      [
        Alcotest_lwt.test_case "Result Lifecycle" `Quick test_result_lifecycle;
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "Reject Missing Fields" `Quick
          test_create_missing_fields;
        Alcotest_lwt.test_case "Export Plate Results CSV" `Quick
          test_export_plate_results_csv;
      ] );
  ]
