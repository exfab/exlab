(* Test Definitions for API endpoint /api/v1/strain-external-links *)

let handler = Test_utils.admin_app

let test_create_malformed_json _switch () =
  let body = {| {"strain_id": 1, |} in
  let req = Test_utils.json_post ~path:"/api/v1/strain-external-links" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_fields _switch () =
  let body = {| {"strain_id": 1} |} in
  let req = Test_utils.json_post ~path:"/api/v1/strain-external-links" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing fields" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let suite =
  [
    ( "Strain Link API",
      [
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "Reject Missing Fields" `Quick
          test_create_missing_fields;
      ] );
  ]
