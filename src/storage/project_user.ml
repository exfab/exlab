(** Project-User assignment logic and queries. *)

open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* SQL Queries *)

let add_query =
  (t2 int int ->. unit)
    "INSERT INTO project_users (project_id, user_id) VALUES (?, ?) ON CONFLICT \
     (project_id, user_id) DO NOTHING"

let remove_query =
  (t2 int int ->. unit)
    "DELETE FROM project_users WHERE project_id = ? AND user_id = ?"

let is_in_project_query =
  (t2 int int ->? int)
    "SELECT id FROM project_users WHERE project_id = ? AND user_id = ?"

(* Let's define the project users type fetching using existing types if needed, 
   but for now we just return booleans or unit, and fetch users/projects in 
   their respective modules, or we can fetch a list of user_ids *)

let get_user_ids_for_project_query =
  (int ->* int) "SELECT user_id FROM project_users WHERE project_id = ?"

let get_project_ids_for_user_query =
  (int ->* int) "SELECT project_id FROM project_users WHERE user_id = ?"

(* OCaml Functions *)

(** [assign ~project_id ~user_id] assigns a user to a project. *)
let assign ~project_id ~user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.exec add_query (project_id, user_id))

(** [remove ~project_id ~user_id] removes a user from a project. *)
let remove ~project_id ~user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.exec remove_query (project_id, user_id))

(** [is_user_in_project ~project_id ~user_id] checks if a user is assigned to a
    project. *)
let is_user_in_project ~project_id ~user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* id_opt = Conn.find_opt is_in_project_query (project_id, user_id) in
      Lwt_result.return (Option.is_some id_opt))

(** [get_users_for_project ~project_id] gets all user IDs assigned to a project.
*)
let get_users_for_project ~project_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.collect_list get_user_ids_for_project_query project_id)

(** [get_projects_for_user ~user_id] gets all project IDs a user is assigned to.
*)
let get_projects_for_user ~user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.collect_list get_project_ids_for_user_query user_id)
