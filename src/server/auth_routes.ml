(** * This module defines the API routes for Authentication. * It handles user
    login, logout, and session information. * * This module handles the HTTP
    layer, translating requests and responses * between the client and the
    underlying storage and core logic layers. It * uses helper functions from
    the [Utils] module for common tasks like * parameter parsing and response
    serialization. *)

module Storage = Exlab_storage
module Core = Exlab_core

(** Handles `POST /api/v1/login`.

    Authenticates a user with their email and password. On success, it
    invalidates the current session and creates a new one with the user's
    identity and role.

    - **Body**: (json) The login credentials, defined by
      [Api_types.Auth.login_request].

    @return A JSON response with the authenticated user's details. *)
let login_handler request =
  let open Lwt.Syntax in
  let* parsed_result =
    Api_utils.parse_body_json Api_types.Auth.login_request_of_yojson request
  in
  match parsed_result with
  | Error (`Bad_Request msg) -> Api_utils.respond_with_error (`Bad_Request msg)
  | Error _ -> Api_utils.respond_with_error (`Bad_Request "Unknown parse error")
  | Ok req -> (
      let* db_result = Storage.User.get_by_email req.email in
      match db_result with
      | Ok (Some user) ->
          let is_valid =
            Core.User.verify_password ~plaintext:req.password
              ~hashed:user.password_hash
          in
          if is_valid then
            let* () = Dream.invalidate_session request in
            let* () =
              Dream.set_session_field request "user_id" (string_of_int user.id)
            in
            let* () =
              Dream.set_session_field request "user_role"
                (Core.User.role_to_string user.role)
            in

            Api_utils.handle_response
              ~serializer:Api_types.User.yojson_of_safe_user
              (Lwt.return (Ok (Api_types.User.to_safe_user user)))
          else
            Api_utils.respond_with_error
              (`Unauthorized "Invalid email or password")
      | Ok None ->
          Api_utils.respond_with_error
            (`Unauthorized "Invalid email or password")
      | Error _ ->
          Api_utils.respond_with_error (`Internal_Server_Error "Database error")
      )

(** Handles `POST /api/v1/logout`.

    Invalidates the current user session.

    @return An empty response with a [204 No Content] status on success. *)
let logout_handler request =
  let open Lwt.Syntax in
  let* () = Dream.invalidate_session request in
  Dream.respond ~status:`No_Content ""

(** Handles `GET /api/v1/me`.

    Retrieves the currently authenticated user's information from their session.

    @return A JSON response containing the current user's detailed information.
*)
let me_handler request =
  let open Lwt.Syntax in
  match Dream.session_field request "user_id" with
  | None -> Api_utils.respond_with_error (`Unauthorized "Not logged in")
  | Some id -> (
      let* user_result = Storage.User.get_by_id (int_of_string id) in
      match user_result with
      | Ok (Some user) ->
          Api_utils.handle_response
            ~serializer:Api_types.User.yojson_of_safe_user
            (Lwt.return (Ok (Api_types.User.to_safe_user user)))
      | _ -> Api_utils.respond_with_error (`Unauthorized "Session invalid"))

let update_password_handler request =
  let open Lwt_result.Syntax in
  let result =
    let* user_id, _role = Auth.get_session_user request in
    let* req =
      Api_utils.parse_body_json Api_types.Auth.password_update_of_yojson request
    in

    (* Validate new password length *)
    let* () =
      match Core.User.validate_password req.new_password with
      | Ok () -> Lwt.return (Ok ())
      | Error msg -> Lwt.return (Error (`Bad_Request msg))
    in

    let* user_opt = Storage.User.get_by_id user_id in
    match user_opt with
    | None -> Lwt.return (Error (`Unauthorized "User not found"))
    | Some user ->
        if
          Core.User.verify_password ~plaintext:req.current_password
            ~hashed:user.password_hash
        then Storage.User.update_password user_id req.new_password
        else Lwt.return (Error (`Bad_Request "Incorrect current password"))
  in
  Api_utils.handle_unit_result result

let login_route = Dream.post "/api/v1/login" login_handler

let protected_routes =
  [
    Dream.post "/api/v1/logout" logout_handler;
    Dream.get "/api/v1/me" me_handler;
    Dream.put "/api/v1/me/password" update_password_handler;
  ]

let routes = login_route :: protected_routes
