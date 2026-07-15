(** Handling for the ResultDefinition data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let category_fields = "rc.id, rc.uid, rc.name, rc.description"

let definition_fields =
  "rd.id, rd.uid, rd.short_id, rd.name, rd.description, rd.data_type, rd.unit, \
   rd.is_required, rd.created_at, rd.updated_at"

let definition_t =
  let encode (d : Exlab_core.Types.result_definition) =
    let cat_id, cat_uid, cat_name, cat_desc =
      match d.category with
      | Some c -> (Some c.id, Some c.uid, Some c.name, c.description)
      | None -> (None, None, None, None)
    in
    let data_type_str = Exlab_core.Types.ResultType.to_string d.data_type in
    Ok
      ( ( d.id,
          d.uid,
          d.short_id,
          d.name,
          d.description,
          data_type_str,
          d.unit,
          d.is_required,
          d.created_at,
          d.updated_at ),
        (cat_id, cat_uid, cat_name, cat_desc) )
  in
  let decode
      ( ( id,
          uid,
          short_id,
          name,
          description,
          data_type_str,
          unit,
          is_required,
          created_at,
          updated_at ),
        (cat_id, cat_uid, cat_name, cat_desc) ) =
    match Exlab_core.Types.ResultType.of_string data_type_str with
    | Ok data_type ->
        let category =
          match (cat_id, cat_uid, cat_name) with
          | Some cat_id, Some cat_uid, Some cat_name ->
              Some
                ({
                   id = cat_id;
                   uid = cat_uid;
                   name = cat_name;
                   description = cat_desc;
                 }
                  : Exlab_core.Types.result_category)
          | _ -> None
        in
        Ok
          ({
             id;
             uid;
             short_id;
             name;
             description;
             data_type;
             unit;
             category;
             is_required;
             created_at;
             updated_at;
           }
            : Exlab_core.Types.result_definition)
    | Error _ ->
        Error
          (Printf.sprintf "Invalid data_type string in DB: %s" data_type_str)
  in
  let rep_def =
    t10 int string string string (option string) string (option string) bool
      float float
  in
  let rep_cat =
    t4 (option int) (option string) (option string) (option string)
  in
  Caqti_type.custom ~encode ~decode (t2 rep_def rep_cat)

let joined_t = definition_t

(* SQL Queries *)

let get_all_query =
  (unit ->* joined_t)
    (Printf.sprintf
       "SELECT %s, %s FROM result_definitions rd LEFT JOIN result_categories \
        rc ON rd.category_id = rc.id"
       definition_fields category_fields)

let search_query =
  (string ->* joined_t)
    (Printf.sprintf
       "SELECT %s, %s FROM result_definitions rd LEFT JOIN result_categories \
        rc ON rd.category_id = rc.id WHERE rd.name ILIKE '%%' || ? || '%%'"
       definition_fields category_fields)

let add_query =
  (t9 string string string (option string) string (option string) (option int)
     bool float
  ->! joined_t)
    (Printf.sprintf
       "WITH new_def AS (\n\
       \       INSERT INTO result_definitions (uid, short_id, name, \
        description, data_type, unit, category_id, is_required, created_at)\n\
       \       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)\n\
       \       RETURNING id, uid, short_id, name, description, data_type, \
        unit, is_required, created_at, updated_at, category_id\n\
       \     )\n\
       \     SELECT d.id, d.uid, d.short_id, d.name, d.description, \
        d.data_type, d.unit, d.is_required, d.created_at, d.updated_at, %s \
        FROM new_def d LEFT JOIN result_categories rc ON d.category_id = rc.id"
       category_fields)

let get_by_id_query =
  (int ->? joined_t)
    (Printf.sprintf
       "SELECT %s, %s FROM result_definitions rd LEFT JOIN result_categories \
        rc ON rd.category_id = rc.id WHERE rd.id = ?"
       definition_fields category_fields)

let get_by_uid_query =
  (string ->? joined_t)
    (Printf.sprintf
       "SELECT %s, %s FROM result_definitions rd LEFT JOIN result_categories \
        rc ON rd.category_id = rc.id WHERE rd.uid = ?"
       definition_fields category_fields)

let get_by_short_id_query =
  (string ->? joined_t)
    (Printf.sprintf
       "SELECT %s, %s FROM result_definitions rd LEFT JOIN result_categories \
        rc ON rd.category_id = rc.id WHERE rd.short_id = ?"
       definition_fields category_fields)

let update_query =
  (t8 string string (option string) string (option string) (option int) bool int
  ->! joined_t)
    (Printf.sprintf
       "WITH updated_def AS (\n\
       \       UPDATE result_definitions SET short_id = ?, name = ?, \
        description = ?, data_type = ?, unit = ?, category_id = ?, is_required \
        = ? WHERE id = ?\n\
       \       RETURNING id, uid, short_id, name, description, data_type, \
        unit, is_required, created_at, updated_at, category_id\n\
       \     )\n\
       \     SELECT d.id, d.uid, d.short_id, d.name, d.description, \
        d.data_type, d.unit, d.is_required, d.created_at, d.updated_at, %s \
        FROM updated_def d LEFT JOIN result_categories rc ON d.category_id = \
        rc.id"
       category_fields)

(* OCaml functions *)

(** [get_all ()] retrieves all result definitions. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [search term] retrieves result definitions matching the term in name. *)
let search term =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list search_query term in
      Lwt_result.return rows)

(* Decoupled search by id without touching db.request *)

(** [lookup conn id] retrieves a result definition by ID within a transaction.
*)
let lookup (module Conn : Caqti_lwt.CONNECTION) id =
  let open Lwt_result.Syntax in
  let* result = Conn.find_opt get_by_id_query id in
  Lwt.return (Ok result)

(* Open the db.request using lookup *)

(** [get_by_id id] retrieves a result definition by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_uid uid] retrieves a result definition by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [get_by_short_id short_id] retrieves a result definition by its short ID. *)
let get_by_short_id short_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_short_id_query short_id in
      Lwt_result.return row)

(** [insert conn ~short_id ~name ~description ~data_type ~unit ?category_id
     ?is_required ()] inserts a new result definition within a transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~short_id ~name ~description
    ~(data_type : ResultType.t) ~unit ?category_id ?(is_required = false) () =
  let uid = Utils.make_uuid () in
  let data_type_str = ResultType.to_string data_type in
  let created_at = Unix.time () in

  let* definition =
    Conn.find add_query
      ( uid,
        short_id,
        name,
        description,
        data_type_str,
        unit,
        category_id,
        is_required,
        created_at )
  in

  Lwt.return (Ok definition)

(** [add ~short_id ~name ~description ~data_type ~unit ?category_id ?is_required
     ()] creates a new result definition. *)
let add ~short_id ~name ~description ~(data_type : ResultType.t) ~unit
    ?category_id ?is_required () =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert
        (module Conn)
        ~short_id ~name ~description ~data_type ~unit ?category_id ?is_required
        ())

(** [update ~id ~short_id ~name ~description ~data_type ~unit ?category_id
     ~is_required ()] updates an existing result definition. *)
let update ~id ~short_id ~name ~description ~(data_type : ResultType.t) ~unit
    ?category_id ~is_required () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let data_type_str = ResultType.to_string data_type in
      let* row =
        Conn.find update_query
          ( short_id,
            name,
            description,
            data_type_str,
            unit,
            category_id,
            is_required,
            id )
      in
      Lwt_result.return row)
