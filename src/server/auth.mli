(** Session authentication middleware, RBAC checks, and project-scoped resource
    authorization combinators. *)

val auth_required : Dream.middleware
val role_required : Exlab_core.Types.user_role list -> Dream.middleware

val check_project_access :
  user_id:int ->
  user_role:Exlab_core.Types.user_role ->
  project_id:int ->
  (unit, [> Caqti_error.t | `Forbidden of string ]) result Lwt.t

val fetch_scoped_list :
  user_id:int ->
  user_role:Exlab_core.Types.user_role ->
  get_all_function:(unit -> 'a) ->
  get_all_user_function:(int -> 'a) ->
  'a

val get_session_user :
  Dream.request ->
  (int * Exlab_core.Types.user_role, [> `Unauthorized of string ]) result Lwt.t

val with_project_access :
  Dream.request ->
  string ->
  (user_id:int ->
  user_role:Exlab_core.Types.user_role ->
  project:Exlab_core.Types.project ->
  ( 'a,
    ([> Caqti_error.t
     | `Bad_Request of string
     | `Forbidden of string
     | `Not_Found of string
     | `Unauthorized of string ]
     as
     'e) )
  result
  Lwt.t) ->
  ('a, 'e) result Lwt.t

val with_plate_access :
  Dream.request ->
  string ->
  (user_id:int ->
  user_role:Exlab_core.Types.user_role ->
  plate:Exlab_core.Types.plate ->
  ( 'a,
    ([> Caqti_error.t
     | `Bad_Request of string
     | `Forbidden of string
     | `Not_Found of string
     | `Unauthorized of string ]
     as
     'e) )
  result
  Lwt.t) ->
  ('a, 'e) result Lwt.t

val with_sample_access :
  Dream.request ->
  string ->
  (user_id:int ->
  user_role:Exlab_core.Types.user_role ->
  sample:Exlab_core.Types.sample ->
  ( 'a,
    ([> Caqti_error.t
     | `Bad_Request of string
     | `Forbidden of string
     | `Not_Found of string
     | `Unauthorized of string ]
     as
     'e) )
  result
  Lwt.t) ->
  ('a, 'e) result Lwt.t
