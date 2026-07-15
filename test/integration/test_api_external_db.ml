(* Test Definitions for API endpoint /api/v1/external-db-definitions *)

let handler = Test_utils.admin_app

let test_create_malformed_json _switch () =
  let body = {| {"name": "Broken JSON", |} in
  let req =
    Test_utils.json_post ~path:"/api/v1/external-db-definitions" ~body
  in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_fields _switch () =
  let body = {| {"name": "Test External DB"} |} in
  let req =
    Test_utils.json_post ~path:"/api/v1/external-db-definitions" ~body
  in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing fields" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_external_db_lifecycle _switch () =
  let open Lwt.Syntax in
  (* Create *)
  let time = Unix.gettimeofday () in
  let name = Printf.sprintf "NCBI-%f" time in
  let body =
    Printf.sprintf
      {| {"name": "%s", "url_template": "https://ncbi.nlm.nih.gov/gene/{id}"} |}
      name
  in
  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/external-db-definitions"
      ~body ~expected_status:201 "Create successful"
  in
  let* create_body = Dream.body create_res in
  let json = Yojson.Safe.from_string create_body in
  let db_id = Yojson.Safe.Util.(member "id" json |> to_int) in

  (* Fetch *)
  let path = Printf.sprintf "/api/v1/external-db-definitions/%d" db_id in
  let* _ =
    Test_utils.assert_json_get ~handler ~path ~expected_status:200
      "Fetch successful"
  in

  (* Fetch All *)
  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/external-db-definitions"
      ~expected_status:200 "Fetch all successful"
  in

  (* Update *)
  let update_body =
    Printf.sprintf
      {| {"name": "%s Updated", "url_template": "https://ncbi.com/{id}"} |} name
  in
  let* _ =
    Test_utils.assert_json_put ~handler ~path ~body:update_body
      ~expected_status:200 "Update successful"
  in
  Lwt.return ()

let test_get_not_found _switch () =
  let open Lwt.Syntax in
  let* _ =
    Test_utils.assert_json_get ~handler
      ~path:"/api/v1/external-db-definitions/999999" ~expected_status:404
      "Get non-existent fails"
  in
  Lwt.return ()

let test_update_not_found _switch () =
  let open Lwt.Syntax in
  let update_body =
    {| {"name": "Does not exist", "url_template": "http://example.com"} |}
  in
  let* _ =
    Test_utils.assert_json_put ~handler
      ~path:"/api/v1/external-db-definitions/999999" ~body:update_body
      ~expected_status:404 "Update non-existent fails"
  in
  Lwt.return ()

let suite =
  [
    ( "External DB API",
      [
        Alcotest_lwt.test_case "External DB Lifecycle" `Quick
          test_external_db_lifecycle;
        Alcotest_lwt.test_case "Reject Get Not Found" `Quick test_get_not_found;
        Alcotest_lwt.test_case "Reject Update Not Found" `Quick
          test_update_not_found;
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "Reject Missing Fields" `Quick
          test_create_missing_fields;
      ] );
  ]
