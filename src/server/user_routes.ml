(** * This module defines the API routes for managing Users. * It provides
    endpoints for creating, retrieving, updating, and deleting * Users, forming
    a complete CRUD interface. * * This module handles the HTTP layer,
    translating requests and responses * between the client and the underlying
    storage and core logic layers. It * uses helper functions from the [Utils]
    module for common tasks like * parameter parsing and response serialization.
    * * Key functionalities include: * - Listing all users. * - Creating a new
    user (Admin only). * - Retrieving a user by their identifier. * - Updating
    an existing user (Admin only). * - Deleting a user (Admin only). *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

(** Handles `GET /api/v1/users`.

    Retrieves a list of all users in the system.

    @return
      A JSON response containing a list of all safe users and a total count. *)
let get_all_users_handler request =
  let search_term_opt = Dream.query request "search" in
  let result =
    let* users =
      match search_term_opt with
      | Some term when String.length term > 0 -> Storage.User.search term
      | _ -> Storage.User.get_all ()
    in
    let safe_users = List.map Api_types.User.to_safe_user users in
    let response =
      { Api_types.User.data = safe_users; count = List.length safe_users }
    in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response ~serializer:Api_types.User.yojson_of_list_response
    result

(** Handles `GET /api/v1/users/:identifier`.

    Retrieves a user by their integer ID or email address.

    @param identifier The ID or email of the user.
    @return A JSON response containing the safe user's details. *)
let get_user_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* user = Api_utils.find_user_by_identifier identifier in
    Lwt.return (Ok (Api_types.User.to_safe_user user))
  in
  Api_utils.handle_response ~serializer:Api_types.User.yojson_of_safe_user
    result

(** Handles `POST /api/v1/users`.

    Creates a new user. Requires Admin role.

    - **Body**: (json) The user creation payload, defined by
      [Api_types.User.create].

    @return
      A JSON response with the newly created safe user and a [201 Created]
      status. *)
let create_user_handler request =
  let open Lwt_result.Syntax in
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.User.create_of_yojson request
    in
    let* () =
      match Core.User.validate_password req.password with
      | Ok () -> Lwt.return (Ok ())
      | Error msg -> Lwt.return (Error (`Bad_Request msg))
    in
    let* new_user =
      Storage.User.add ~email:req.email ~password:req.password ~role:req.role
    in
    Lwt.return (Ok (Api_types.User.to_safe_user new_user))
  in

  Api_utils.handle_response ~status:`Created
    ~serializer:Api_types.User.yojson_of_safe_user result

(** Handles `PUT /api/v1/users/:identifier`.

    Updates an existing user. Requires Admin role.

    @param identifier
      The ID or email of the user to update.
      - **Body**: (json) The user update payload, defined by
        [Api_types.User.update].
    @return A JSON response with the updated safe user details. *)
let update_user_handler request =
  let open Lwt.Syntax in
  let identifier = Dream.param request "identifier" in
  let* parsed_result =
    Api_utils.parse_body_json Api_types.User.update_of_yojson request
  in
  match parsed_result with
  | Error (`Bad_Request msg) -> Api_utils.respond_with_error (`Bad_Request msg)
  | Error _ -> Api_utils.respond_with_error (`Bad_Request "Unknown parse error")
  | Ok req ->
      let result =
        let open Lwt_result.Syntax in
        let* existing_user = Api_utils.find_user_by_identifier identifier in
        let* () =
          match req.password with
          | Some p when p <> "" -> (
              match Core.User.validate_password p with
              | Ok () -> Lwt.return (Ok ())
              | Error msg -> Lwt.return (Error (`Bad_Request msg)))
          | _ -> Lwt.return (Ok ())
        in
        let password_hash =
          match req.password with
          | Some p when p <> "" -> Core.User.hashed_password p
          | _ -> existing_user.password_hash
        in
        let* updated_user =
          Storage.User.update ~id:existing_user.id ~email:req.email
            ~password_hash ~role:req.role
        in
        Lwt.return (Ok (Api_types.User.to_safe_user updated_user))
      in
      Api_utils.handle_response ~status:`Created
        ~serializer:Api_types.User.yojson_of_safe_user result

(** Handles `DELETE /api/v1/users/:identifier`.

    Permanently deletes a user from the system. Requires Admin role.

    @param identifier The ID or email of the user to delete.
    @return An empty response with a [204 No Content] status on success. *)
let delete_user_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* existing_user = Api_utils.find_user_by_identifier identifier in
    Storage.User.delete existing_user.id
  in
  Api_utils.handle_unit_result result

let routes =
  [
    Dream.get "/api/v1/users"
      (Auth.role_required [ Core.Types.Admin ] get_all_users_handler);
    Dream.get "/api/v1/users/:identifier"
      (Auth.role_required [ Core.Types.Admin ] get_user_handler);
    Dream.post "/api/v1/users"
      (Auth.role_required [ Core.Types.Admin ] create_user_handler);
    Dream.put "/api/v1/users/:identifier"
      (Auth.role_required [ Core.Types.Admin ] update_user_handler);
    Dream.delete "/api/v1/users/:identifier"
      (Auth.role_required [ Core.Types.Admin ] delete_user_handler);
  ]
