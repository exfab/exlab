(** * This module defines the API routes for managing System Settings. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

(** Handles `GET /api/v1/settings/:key`. Retrieves a specific setting by its
    key. *)
let get_setting_handler request =
  let key = Dream.param request "key" in
  let result =
    let* setting_opt = Storage.Setting.get_by_key key in
    match setting_opt with
    | Some setting -> Lwt_result.return setting.value
    | None -> Lwt_result.fail (`Not_Found ("Setting not found: " ^ key))
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_json result

(** Handles `PUT /api/v1/settings/:key`. Creates or updates a setting. Requires
    Admin or Lab Manager role. *)
let update_setting_handler request =
  let key = Dream.param request "key" in
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.Setting.update_of_yojson request
    in
    let* _ =
      Storage.Setting.upsert ~key ~value:req.value ?description:req.description
        ()
    in
    Lwt_result.return ()
  in
  Api_utils.handle_unit_result result

let routes =
  [
    Dream.get "/api/v1/settings/:key" get_setting_handler;
    Dream.put "/api/v1/settings/:key"
      (Auth.role_required
         [ Core.Types.Admin; Core.Types.Lab_manager ]
         update_setting_handler);
  ]
