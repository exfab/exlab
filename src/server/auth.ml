module Core = Exlab_core
module Storage = Exlab_storage
open Lwt_result.Syntax

(* Auth utils *)
let auth_required handler request =
  match Dream.session_field request "user_id" with
  | None ->
      Api_utils.respond_with_error
        (`Unauthorized "You must be logged in to access this resource")
  | Some _user_id -> handler request

let role_required allowed_roles handler request =
  match Dream.session_field request "user_role" with
  | None ->
      Api_utils.respond_with_error
        (`Unauthorized "You must be logged in to access this resource")
  | Some role_str -> (
      match Core.User.role_of_string role_str with
      | Ok user_role ->
          if List.mem user_role allowed_roles then handler request
          else
            Api_utils.respond_with_error
              (`Forbidden "You do not have permission to access this resource")
      | Error _ ->
          Api_utils.respond_with_error
            (`Unauthorized "Invalid user role in session"))

let check_project_access ~user_id ~user_role ~project_id =
  match user_role with
  | Core.Types.Admin | Core.Types.Lab_manager -> Lwt.return (Ok ())
  | Core.Types.Project_manager | Core.Types.Project_user ->
      let* has_access =
        Storage.Project_user.is_user_in_project ~user_id ~project_id
      in
      if has_access then Lwt.return (Ok ())
      else
        Lwt.return (Error (`Forbidden "You do not have access to this project"))

let fetch_scoped_list ~user_id ~user_role ~get_all_function
    ~get_all_user_function =
  match user_role with
  | Core.Types.Admin | Core.Types.Lab_manager -> get_all_function ()
  | Core.Types.Project_manager | Core.Types.Project_user ->
      get_all_user_function user_id

let get_session_user request =
  let user_role_str = Dream.session_field request "user_role" in
  let user_id_str = Dream.session_field request "user_id" in
  match (user_role_str, user_id_str) with
  | Some role_str, Some id_str -> (
      match Core.User.role_of_string role_str with
      | Ok role -> Lwt.return (Ok (int_of_string id_str, role))
      | Error _ -> Lwt.return (Error (`Unauthorized "Invalid role in session")))
  | _ -> Lwt.return (Error (`Unauthorized "Not logged in"))

let with_project_access request identifier_key f =
  let* user_id, user_role = get_session_user request in
  let identifier = Dream.param request identifier_key in
  let* project = Api_utils.find_project_by_identifier identifier in
  let* () = check_project_access ~user_id ~user_role ~project_id:project.id in
  f ~user_id ~user_role ~project
