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
  let _ =
    Dream.test Test_utils.admin_app
      (Test_utils.json_put ~path:"/api/v1/settings/project_metadata_template"
         ~body:{|{"value": []}|})
  in
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

let test_update_project_handles_string_template_and_existing_metadata _switch ()
    =
  (* 1. Set template using bare "String" field_type (as written by settings API / legacy configs) *)
  let* _ =
    Test_utils.assert_json_put ~handler
      ~path:"/api/v1/settings/project_metadata_template"
      ~body:
        {|{"value": [{"key": "Client", "field_type": "String"}], "description": "Default keys"}|}
      ~expected_status:204 "Set template with bare String field_type"
  in
  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:
        {|{"name": "Template Compat Project", "status": "Active", "metadata": {"Client": "ExFAB"}}|}
      ~expected_status:201 "Create project with bare-String template"
  in
  let* create_str = Dream.body create_res in
  let short_id =
    Yojson.Safe.from_string create_str
    |> Yojson.Safe.Util.member "short_id"
    |> Yojson.Safe.Util.to_string
  in

  (* 2. Clear template to [] (simulating template change or imported project like UCR-029),
     then Archive and Restore via PUT while preserving the project's existing metadata keys *)
  let* _ =
    Test_utils.assert_json_put ~handler
      ~path:"/api/v1/settings/project_metadata_template"
      ~body:{|{"value": [], "description": "Default metadata keys"}|}
      ~expected_status:204 "Clear project_metadata_template"
  in
  let* upd_res =
    Test_utils.assert_json_put ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s" short_id)
      ~body:
        {|{"name": "Template Compat Project", "status": "Archived", "metadata": {"Client": "ExFAB"}}|}
      ~expected_status:200
      "Archive project via PUT preserves existing metadata keys"
  in
  let* upd_str = Dream.body upd_res in
  let status_after =
    Yojson.Safe.from_string upd_str
    |> Yojson.Safe.Util.member "status"
    |> Yojson.Safe.Util.to_string
  in
  Alcotest.(check string) "Status updated to Archived" "Archived" status_after;
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
        Alcotest_lwt.test_case
          "Update project with String template and existing metadata" `Quick
          test_update_project_handles_string_template_and_existing_metadata;
      ] );
  ]
