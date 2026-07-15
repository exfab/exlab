(* Test Definitions for API endpoint /api/projects *)

let handler = Test_utils.admin_app

open Lwt.Syntax

let test_create_malformed_json _switch () =
  let body = {| {"name": "Broken JSON", |} in
  let req = Test_utils.json_post ~path:"/api/v1/projects" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_name _switch () =
  let body = {| {"description": "This is a test project"} |} in
  let req = Test_utils.json_post ~path:"/api/v1/projects" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing name" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_with_metadata _switch () =
  let template_body =
    {| { "value": [{"key": "Client", "field_type": ["String"]}] } |}
  in
  let template_req =
    Test_utils.json_put ~path:"/api/v1/settings/project_metadata_template"
      ~body:template_body
  in
  let _ = Dream.test Test_utils.admin_app template_req in

  let body =
    {| {
    "name": "Project with metadata",
    "status": "Pending",
    "metadata": { "Client": "Acme Corp" }
  } |}
  in
  let req = Test_utils.json_post ~path:"/api/v1/projects" ~body in
  let response = Dream.test Test_utils.admin_app req in
  Alcotest.(check int)
    "Should return 201 for valid creation with metadata" 201
    (Dream.status response |> Dream.status_to_int);

  let* body_str = Dream.body response in
  let json = Yojson.Safe.from_string body_str in

  let metadata_json = Yojson.Safe.Util.member "metadata" json in
  let client =
    metadata_json
    |> Yojson.Safe.Util.member "Client"
    |> Yojson.Safe.Util.to_string
  in
  Alcotest.(check string) "Should retain client metadata" "Acme Corp" client;
  Lwt.return ()

let test_create_without_metadata _switch () =
  let body =
    {| { "name": "Project without metadata", "status": "Pending" } |}
  in
  let req = Test_utils.json_post ~path:"/api/v1/projects" ~body in
  let response = Dream.test Test_utils.admin_app req in
  let* body_str = Dream.body response in
  Alcotest.(check int)
    ("Should return 201 for valid creation without metadata, got: " ^ body_str)
    201
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let suite =
  [
    ( "Project API",
      [
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "Reject Missing Name" `Quick
          test_create_missing_name;
        Alcotest_lwt.test_case "Create with metadata" `Quick
          test_create_with_metadata;
        Alcotest_lwt.test_case "Create without metadata" `Quick
          test_create_without_metadata;
      ] );
  ]
