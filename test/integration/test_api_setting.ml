open Lwt.Syntax

let test_get_setting _switch () =
  let req =
    Test_utils.json_get ~path:"/api/v1/settings/project_metadata_template"
  in
  let response = Dream.test Test_utils.admin_app req in
  let status = Dream.status response |> Dream.status_to_int in
  let is_ok_or_not_found = status = 200 || status = 404 in
  Alcotest.(check bool)
    "Should return 200 or 404 for getting a setting" true is_ok_or_not_found;
  Lwt.return ()

let test_put_setting_admin _switch () =
  let body =
    {| { "value": [{"key": "Client", "field_type": "String"}, {"key": "Priority", "field_type": "String"}], "description": "Template keys" } |}
  in
  let req =
    Test_utils.json_put ~path:"/api/v1/settings/project_metadata_template" ~body
  in
  let response = Dream.test Test_utils.admin_app req in
  Alcotest.(check int)
    "Should return 204 for valid PUT as admin" 204
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_put_setting_unauthorized _switch () =
  let body = {| { "value": [{"key": "Client", "field_type": "String"}] } |} in
  let req =
    Test_utils.json_put ~path:"/api/v1/settings/project_metadata_template" ~body
  in
  let response = Dream.test Test_utils.user_app req in
  Alcotest.(check int)
    "Should return 403 for valid PUT as regular user" 403
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let suite =
  [
    ( "Setting API",
      [
        Alcotest_lwt.test_case "GET setting" `Quick test_get_setting;
        Alcotest_lwt.test_case "PUT setting admin" `Quick test_put_setting_admin;
        Alcotest_lwt.test_case "PUT setting unauthorized" `Quick
          test_put_setting_unauthorized;
      ] );
  ]
