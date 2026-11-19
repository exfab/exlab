(** Shared HTTP request parsing, uniform JSON/CSV response rendering, and
    multi-identifier resource lookups. *)

module StringSet : Set.S with type elt = string

type api_error =
  [ Caqti_error.t
  | `Bad_Request of string
  | `Forbidden of string
  | `Internal_Server_Error of string
  | `Msg of string
  | `Not_Found of string
  | `Unauthorized of string ]

val get_int_param :
  Dream.request -> string -> (int, [> `Bad_Request of string ]) result Lwt.t

val parse_body_json :
  (Yojson.Safe.t -> 'a) ->
  Dream.request ->
  ('a, [> `Bad_Request of string ]) result Lwt.t

val parse_body_csv :
  (Csv_utils.row_accessor -> ('a, string) result) ->
  Dream.request ->
  ('a list, [> `Bad_Request of string ]) result Lwt.t

val respond_with_error : [< api_error ] -> Dream.response Lwt.t

val handle_unit_result :
  (unit, [< api_error ]) result Lwt.t -> Dream.response Lwt.t

val handle_response :
  ?status:Dream.status ->
  serializer:('a -> Yojson.Safe.t) ->
  ('a, [< api_error ]) result Lwt.t ->
  Dream.response Lwt.t

val handle_response_with_export :
  ?status:Dream.status ->
  request:Dream.request ->
  filename:string ->
  csv_serializer:('a -> string) ->
  json_serializer:('a -> Yojson.Safe.t) ->
  ('a, [< api_error ]) result Lwt.t ->
  Dream.response Lwt.t

val find_project_by_identifier :
  string ->
  ( Exlab_core.Types.project,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t

val find_sample_by_identifier :
  string ->
  ( Exlab_core.Types.sample,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t

val find_product_by_identifier :
  string ->
  ( Exlab_core.Types.product,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t

val find_plate_by_identifier :
  string ->
  ( Exlab_core.Types.plate,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t

val find_result_definition_by_identifier :
  string ->
  ( Exlab_core.Types.result_definition,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t

val find_result_category_by_identifier :
  string ->
  ( Exlab_core.Types.result_category,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t

val find_strain_by_identifier :
  string ->
  ( Exlab_core.Types.strain,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t

val find_community_by_identifier :
  string ->
  ( Exlab_core.Types.community,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t

val find_user_by_identifier :
  string ->
  ( Exlab_core.Types.user,
    [> Caqti_error.t | `Bad_Request of string | `Not_Found of string ] )
  result
  Lwt.t
