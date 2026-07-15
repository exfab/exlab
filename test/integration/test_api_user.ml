(* Test Definitions for API endpoint /api/users *)

let handler = Test_utils.admin_app

let test_create_short_password _switch () =
  let body =
    {| {"email": "test@test.com", "password": "short", "role": "admin"} |}
  in
  let req = Test_utils.json_post ~path:"/api/v1/users" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for short password" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_fields _switch () =
  let body = {| {"email": "test@test.com"} |} in
  let req = Test_utils.json_post ~path:"/api/v1/users" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing fields" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_update_malformed_json _switch () =
  let body = {| {"email": "test@test.com", |} in
  let req =
    Dream.request ~method_:`PUT
      ~headers:[ ("Content-Type", "application/json") ]
      ~target:"/api/v1/users/1" ""
  in
  Dream.set_body req body;
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_list_users_omits_password_hash _switch () =
  let req = Test_utils.json_get ~path:"/api/v1/users" in
  let response = Dream.test handler req in
  let%lwt body = Dream.body response in
  Alcotest.(check bool)
    "Response body should not contain 'password_hash'" false
    (Re.execp (Re.compile (Re.str "password_hash")) body);
  Lwt.return ()

let test_list_users_unauthorized _switch () =
  let req = Test_utils.json_get ~path:"/api/v1/users" in
  let response = Dream.test Test_utils.user_app req in
  Alcotest.(check int)
    "Should return 403 Forbidden for non-admin" 403
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_get_user_unauthorized _switch () =
  let req = Test_utils.json_get ~path:"/api/v1/users/1" in
  let response = Dream.test Test_utils.user_app req in
  Alcotest.(check int)
    "Should return 403 Forbidden for non-admin" 403
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_search_users _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let search_term = Printf.sprintf "searchable%f" time in
  let email_a = Printf.sprintf "user_a_%s@test.com" search_term in
  let email_b = Printf.sprintf "user_b_unrelated%f@test.com" time in

  let body_a =
    Printf.sprintf
      {| {"email": "%s", "password": "password123", "role": "project_user"} |}
      email_a
  in
  let body_b =
    Printf.sprintf
      {| {"email": "%s", "password": "password123", "role": "project_user"} |}
      email_b
  in

  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/users" ~body:body_a
      ~expected_status:201 "Create User A"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/users" ~body:body_b
      ~expected_status:201 "Create User B"
  in

  let search_path = Printf.sprintf "/api/v1/users?search=%s" search_term in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search users"
  in

  let* search_body = Dream.body search_res in
  let json = Yojson.Safe.from_string search_body in
  let data = Yojson.Safe.Util.(member "data" json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching user" 1 (List.length data);

  let returned_email =
    Yojson.Safe.Util.(List.hd data |> member "email" |> to_string)
  in
  Alcotest.(check string) "Returned user email matches" email_a returned_email;

  Lwt.return ()

let suite =
  [
    ( "User API",
      [
        Alcotest_lwt.test_case "Reject Short Password" `Quick
          test_create_short_password;
        Alcotest_lwt.test_case "Search Users" `Quick test_search_users;
        Alcotest_lwt.test_case "Reject Missing Fields" `Quick
          test_create_missing_fields;
        Alcotest_lwt.test_case "Reject Malformed JSON on Update" `Quick
          test_update_malformed_json;
        Alcotest_lwt.test_case "User list omits password_hash" `Quick
          test_list_users_omits_password_hash;
        Alcotest_lwt.test_case "Reject User List for Non-Admin" `Quick
          test_list_users_unauthorized;
        Alcotest_lwt.test_case "Reject Get User for Non-Admin" `Quick
          test_get_user_unauthorized;
      ] );
  ]
