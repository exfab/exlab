(* Security and Authorization Tests *)

let test_unauthenticated_access _switch () =
  (* Use the 'app' which has auth_required middleware but no mock session *)
  let handler = Test_utils.app in
  let req = Test_utils.json_get ~path:"/api/v1/projects" in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 401 Unauthorized for unauthenticated request" 401
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_rbac_insufficient_permissions _switch () =
  (* Use 'user_app' which has 'project_user' role *)
  let handler = Test_utils.user_app in
  (* POST /api/v1/users is Admin only *)
  let body =
    {| {"email": "attacker@exlab.com", "password": "password123", "role": "admin"} |}
  in
  let req = Test_utils.json_post ~path:"/api/v1/users" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 403 Forbidden for insufficient role" 403
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_project_scope_access_denied _switch () =
  (* Use 'user_app' (user_id=2, role=project_user) *)
  let handler = Test_utils.user_app in

  (* 
     NOTE: In a real integration test with a DB, we would ensure project 1 
     exists but user 2 is not in it. 
     Since we are testing the middleware/logic layer here, 
     it will call check_project_access, which calls Storage.Project_user.is_user_in_project.
     In the test environment, if the DB is empty, it will return false, triggering 403.
  *)
  let req = Test_utils.json_get ~path:"/api/v1/projects/1" in
  let response = Dream.test handler req in

  Alcotest.(check int)
    "Should return 403 Forbidden for project not assigned to user" 403
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_update_password_success _switch () =
  let open Lwt.Syntax in
  (* Create a brand-new user and log in as them via a custom mock middleware
     that forces the session user_id. *)
  let timestamp = Unix.gettimeofday () |> int_of_float |> string_of_int in
  let email = Printf.sprintf "test_pw_success_%s@exlab.com" timestamp in
  let create_body =
    Printf.sprintf
      {| {"email": "%s", "password": "oldpassword", "role": "admin"} |} email
  in
  let create_req =
    Test_utils.json_post ~path:"/api/v1/users" ~body:create_body
  in
  let _ = Dream.test Test_utils.admin_app create_req in

  (* Fetch the user ID created above. *)
  let req_list = Test_utils.json_get ~path:"/api/v1/users" in
  let res_list = Dream.test Test_utils.admin_app req_list in
  let* body_str = Dream.body res_list in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let users = json |> member "data" |> to_list in
  let user =
    List.find (fun u -> u |> member "email" |> to_string = email) users
  in
  let user_id_int = user |> member "id" |> to_int in
  let user_id_str = string_of_int user_id_int in

  (* Create custom middleware for this specific user *)
  let custom_middleware handler request =
    let* () = Dream.set_session_field request "user_role" "admin" in
    let* () = Dream.set_session_field request "user_id" user_id_str in
    handler request
  in
  let custom_app =
    Dream.memory_sessions @@ custom_middleware
    @@ Dream.router
         [
           Dream.scope "/" [ Exlab_server.Auth.auth_required ] Test_utils.routes;
         ]
  in

  (* 2. Attempt to update password *)
  let body =
    {| {"current_password": "oldpassword", "new_password": "newpassword123"} |}
  in
  let req = Test_utils.json_put ~path:"/api/v1/me/password" ~body in
  let response = Dream.test custom_app req in
  Alcotest.(check int)
    "Should return 204 No Content on successful password update" 204
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_update_password_wrong_current _switch () =
  let open Lwt.Syntax in
  (* Pause briefly so consecutive-test timestamps do not collide. *)
  let _ = Unix.sleepf 0.01 in
  let timestamp = Unix.gettimeofday () |> int_of_float |> string_of_int in
  let email = Printf.sprintf "test_pw_fail_%s@exlab.com" timestamp in
  let create_body =
    Printf.sprintf
      {| {"email": "%s", "password": "oldpassword", "role": "admin"} |} email
  in
  let create_req =
    Test_utils.json_post ~path:"/api/v1/users" ~body:create_body
  in
  let _ = Dream.test Test_utils.admin_app create_req in

  let req_list = Test_utils.json_get ~path:"/api/v1/users" in
  let res_list = Dream.test Test_utils.admin_app req_list in
  let* body_str = Dream.body res_list in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let users = json |> member "data" |> to_list in
  let user =
    List.find (fun u -> u |> member "email" |> to_string = email) users
  in
  let user_id_int = user |> member "id" |> to_int in
  let user_id_str = string_of_int user_id_int in

  let custom_middleware handler request =
    let* () = Dream.set_session_field request "user_role" "admin" in
    let* () = Dream.set_session_field request "user_id" user_id_str in
    handler request
  in
  let custom_app =
    Dream.memory_sessions @@ custom_middleware
    @@ Dream.router
         [
           Dream.scope "/" [ Exlab_server.Auth.auth_required ] Test_utils.routes;
         ]
  in

  (* 2. Attempt to update password with wrong current *)
  let body =
    {| {"current_password": "wrongpassword", "new_password": "newpassword123"} |}
  in
  let req = Test_utils.json_put ~path:"/api/v1/me/password" ~body in
  let response = Dream.test custom_app req in
  Alcotest.(check int)
    "Should return 400 Bad Request on wrong current password" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let suite =
  [
    ( "Security API",
      [
        Alcotest_lwt.test_case "Reject Unauthenticated" `Quick
          test_unauthenticated_access;
        Alcotest_lwt.test_case "Reject Insufficient Role (RBAC)" `Quick
          test_rbac_insufficient_permissions;
        Alcotest_lwt.test_case "Reject Project Out of Scope" `Quick
          test_project_scope_access_denied;
        Alcotest_lwt.test_case "Update Password Success" `Quick
          test_update_password_success;
        Alcotest_lwt.test_case "Reject Update Password (Wrong Current)" `Quick
          test_update_password_wrong_current;
      ] );
  ]
