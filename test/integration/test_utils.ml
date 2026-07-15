open Exlab_server
(* Mock Database *)

(* * The Global Test Handler
 * * This combines all routes from your application into a single router.
 * Use this in all integration tests to ensure you are testing against 
 * the real application structure.
 *)
let routes =
  Project_routes.routes @ Result_routes.routes @ Plate_routes.routes
  @ Product_routes.routes @ Sample_routes.routes @ Well_routes.routes
  @ Strain_routes.routes @ External_db_routes.routes @ Strain_link_routes.routes
  @ User_routes.routes @ Setting_routes.routes @ Auth_routes.protected_routes
  @ Community_routes.routes

let app =
  Dream.memory_sessions
  @@ Dream.router [ Dream.scope "/" [ Auth.auth_required ] routes ]

let mock_admin_middleware handler request =
  let open Lwt.Syntax in
  let* () = Dream.set_session_field request "user_role" "admin" in
  let* () = Dream.set_session_field request "user_id" "1" in
  handler request

let admin_app =
  Dream.memory_sessions @@ mock_admin_middleware
  @@ Dream.router [ Dream.scope "/" [ Auth.auth_required ] routes ]

let mock_user_middleware handler request =
  let open Lwt.Syntax in
  let* () = Dream.set_session_field request "user_role" "project_user" in
  let* () = Dream.set_session_field request "user_id" "2" in
  handler request

let user_app =
  Dream.memory_sessions @@ mock_user_middleware
  @@ Dream.router [ Dream.scope "/" [ Auth.auth_required ] routes ]

(* * Helper to create a properly formatted JSON POST request.
 * Reduces boilerplate in individual tests.
 *)
let json_post ~path ~body =
  let req =
    Dream.request ~method_:`POST
      ~headers:[ ("Content-Type", "application/json") ]
      ~target:path ""
  in
  Dream.set_body req body;
  req

let csv_post ~path ~body =
  let req =
    Dream.request ~method_:`POST
      ~headers:[ ("Content-Type", "text/csv") ]
      ~target:path ""
  in
  Dream.set_body req body;
  req

let csv_patch ~path ~body =
  let req =
    Dream.request ~method_:`PATCH
      ~headers:[ ("Content-Type", "text/csv") ]
      ~target:path ""
  in
  Dream.set_body req body;
  req

(* * Helper to create a properly formatted JSON GET request.
 * Reduces boilerplate in individual tests.
 *)
let json_get ~path = Dream.request ~method_:`GET ~target:path ""

let assert_json_post ~handler ~path ~body ~expected_status msg =
  let req = json_post ~path ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    msg expected_status
    (Dream.status response |> Dream.status_to_int);
  Lwt.return response

let assert_csv_post ~handler ~path ~body ~expected_status msg =
  let req = csv_post ~path ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    msg expected_status
    (Dream.status response |> Dream.status_to_int);
  Lwt.return response

let assert_json_get ~handler ~path ~expected_status msg =
  let req = json_get ~path in
  let response = Dream.test handler req in
  Alcotest.(check int)
    msg expected_status
    (Dream.status response |> Dream.status_to_int);
  Lwt.return response

let json_put ~path ~body =
  let req =
    Dream.request ~method_:`PUT
      ~headers:[ ("Content-Type", "application/json") ]
      ~target:path ""
  in
  Dream.set_body req body;
  req

let assert_json_put ~handler ~path ~body ~expected_status msg =
  let req = json_put ~path ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    msg expected_status
    (Dream.status response |> Dream.status_to_int);
  Lwt.return response

let json_patch ~path ~body =
  let req =
    Dream.request ~method_:`PATCH
      ~headers:[ ("Content-Type", "application/json") ]
      ~target:path ""
  in
  Dream.set_body req body;
  req

let assert_json_patch ~handler ~path ~body ~expected_status msg =
  let req = json_patch ~path ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    msg expected_status
    (Dream.status response |> Dream.status_to_int);
  Lwt.return response

let json_delete ~path =
  Dream.request ~method_:`DELETE
    ~headers:[ ("Content-Type", "application/json") ]
    ~target:path ""

let assert_json_delete ~handler ~path ~expected_status msg =
  let req = json_delete ~path in
  let response = Dream.test handler req in
  Alcotest.(check int)
    msg expected_status
    (Dream.status response |> Dream.status_to_int);
  Lwt.return response
