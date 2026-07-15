(* Test Definitions for API endpoint /api/result-definitions *)

(* Mock Router Setup *)
let handler = Test_utils.admin_app

(* Test POST rejection for use of invalid data type *)
let test_create_invalid_data_type _switch () =
  let body =
    {|
    {
      "short_id": "bad_def",
      "name": "Bad Definition",
      "data_type": "NotARealType"
    }
  |}
  in

  let req = Test_utils.json_post ~path:"/api/v1/result-definitions" ~body in

  Dream.set_body req body;

  (* Synchronous request *)
  let response = Dream.test handler req in
  let status_code = Dream.status response |> Dream.status_to_int in

  (* Asynchronous body read *)
  let%lwt response_body = Dream.body response in

  (* Check Status *)
  Alcotest.(check int) "Should return 400 Bad Request" 400 status_code;

  (* Check JSON Content *)
  let json = Yojson.Safe.from_string response_body in

  let error_msg =
    json |> Yojson.Safe.Util.member "error" |> Yojson.Safe.Util.to_string
  in
  let is_correct_error =
    String.starts_with ~prefix:"Invalid Payload" error_msg
  in
  Alcotest.(check bool)
    "Error message should start with 'Invalid Payload'" true is_correct_error;

  Lwt.return ()

(* Test POST rejection for broken JSON *)
let test_create_malformed_json _switch () =
  let body = {| {"name": "Broken JSON", "data_type": |} in

  let req = Test_utils.json_post ~path:"/api/v1/result-definitions" ~body in

  Dream.set_body req body;

  let response = Dream.test handler req in

  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_get_all_result_definitions _switch () =
  let req = Test_utils.json_get ~path:"/api/v1/result-definitions" in
  let response = Dream.test handler req in

  Alcotest.(check int)
    "should return 200" 200
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_search_result_definitions _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let search_term = Printf.sprintf "DefSearch%f" time in
  let name_a = Printf.sprintf "Definition A %s" search_term in
  let name_b = Printf.sprintf "Definition B Unrelated%f" time in

  let body_a =
    Printf.sprintf
      {| {"short_id": "def_a", "name": "%s", "data_type": "Float"} |} name_a
  in
  let body_b =
    Printf.sprintf
      {| {"short_id": "def_b", "name": "%s", "data_type": "Float"} |} name_b
  in

  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/result-definitions"
      ~body:body_a ~expected_status:201 "Create Def A"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/result-definitions"
      ~body:body_b ~expected_status:201 "Create Def B"
  in

  let search_path =
    Printf.sprintf "/api/v1/result-definitions?search=%s" search_term
  in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search definitions"
  in

  let* search_body = Dream.body search_res in
  let json = Yojson.Safe.from_string search_body in
  let data = Yojson.Safe.Util.(member "data" json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching definition" 1 (List.length data);

  let returned_name =
    Yojson.Safe.Util.(List.hd data |> member "name" |> to_string)
  in
  Alcotest.(check string)
    "Returned definition name matches" name_a returned_name;

  Lwt.return ()

let suite =
  [
    ( "Result Definitions API",
      [
        Alcotest_lwt.test_case "Reject Invalid Data Type" `Quick
          test_create_invalid_data_type;
        Alcotest_lwt.test_case "Search Result Definitions" `Quick
          test_search_result_definitions;
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "GET all result definitions" `Quick
          test_get_all_result_definitions;
      ] );
  ]
