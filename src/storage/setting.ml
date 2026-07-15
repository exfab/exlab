(** Handling for the System Settings table. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields = "id, key, value, description, updated_at"

let setting_t =
  let encode (s : system_setting) =
    let value_str = Yojson.Safe.to_string s.value in
    Ok (s.id, s.key, value_str, s.description, s.updated_at)
  in
  let decode (id, key, value_str, description, updated_at) =
    let value = Yojson.Safe.from_string value_str in
    Ok ({ id; key; value; description; updated_at } : system_setting)
  in
  let rep = t5 int string string (option string) float in
  Caqti_type.custom ~encode ~decode rep

(* SQL Queries *)

let get_by_key_query =
  (string ->? setting_t)
    (Printf.sprintf "SELECT %s FROM system_settings WHERE key = ?" select_fields)

let upsert_query =
  (t3 string string (option string) ->! setting_t)
    (Printf.sprintf
       "INSERT INTO system_settings (key, value, description) VALUES (?, \
        ?::jsonb, ?) ON CONFLICT (key) DO UPDATE SET value = EXCLUDED.value, \
        description = COALESCE(EXCLUDED.description, \
        system_settings.description) RETURNING %s"
       select_fields)

(* OCaml Functions *)

(** [get_by_key key] retrieves a system setting by its key. *)
let get_by_key key =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_key_query key in
      Lwt_result.return row)

(** [upsert ~key ~value ?description ()] creates or updates a system setting. *)
let upsert ~key ~value ?description () =
  let value_str = Yojson.Safe.to_string value in
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find upsert_query (key, value_str, description) in
      Lwt_result.return row)
