(** Plate type logic and database operations. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields =
  "id, uid, short_id, name, project_id, product_id, plate_format, created_at, \
   updated_at"

let plate_t =
  let encode (p : Exlab_core.Types.plate) =
    Ok
      ( p.id,
        p.uid,
        p.short_id,
        p.name,
        p.project_id,
        p.product_id,
        string_of_plate_format p.plate_format,
        p.created_at,
        p.updated_at )
  in
  let decode
      ( id,
        uid,
        short_id,
        name,
        project_id,
        product_id,
        plate_format_str,
        created_at,
        updated_at ) =
    match plate_format_of_string plate_format_str with
    | Ok plate_format ->
        Ok
          ({
             plate_format;
             id;
             uid;
             short_id;
             name;
             project_id;
             product_id;
             created_at;
             updated_at;
           }
            : Exlab_core.Types.plate)
    | Error _ ->
        Error (Printf.sprintf "Invalid plate format in DB: %s" plate_format_str)
  in
  let rep = t9 int string string string int (option int) string float float in
  Caqti_type.custom ~encode ~decode rep

(* SQL Queries *)

(* GET all plates *)
let get_all_query =
  (unit ->* plate_t) (Printf.sprintf "SELECT %s FROM plates" select_fields)

let get_all_for_user_query =
  (int ->* plate_t)
    (Printf.sprintf
       "SELECT p.id, p.uid, p.short_id, p.name, p.project_id, p.product_id, \
        p.plate_format, p.created_at, p.updated_at FROM plates p INNER JOIN \
        project_users pu ON p.project_id = pu.project_id WHERE pu.user_id = ?")

(* POST a new plate *)
let add_query =
  (t7 string string int (option int) string string float ->! plate_t)
    (Printf.sprintf
       "INSERT INTO plates (uid, short_id, project_id, product_id, name, \
        plate_format, created_at)\n\
       \        VALUES (?, ?, ?, ?, ?, ?, ?)\n\
       \        RETURNING %s"
       select_fields)

(* PUT an update *)
let set_short_id_query =
  (t2 string int ->. unit) "UPDATE plates SET short_id = ? WHERE id = ?"

(* GET a plate by it's ID *)
let get_by_id_query =
  (int ->? plate_t)
    (Printf.sprintf "SELECT %s FROM plates WHERE id = ?" select_fields)

let get_by_uid_query =
  (string ->? plate_t)
    (Printf.sprintf "SELECT %s FROM plates WHERE uid = ?" select_fields)

let get_by_short_id_query =
  (string ->? plate_t)
    (Printf.sprintf "SELECT %s FROM plates WHERE short_id = ?" select_fields)

(* GET data for CSV output *)
let get_csv_data_query =
  (int ->* t3 string (option string) (option string))
    "SELECT w.coordinate, s.short_id, s.name\n\
    \     FROM wells w\n\
    \     LEFT JOIN samples s ON w.sample_id = s.id\n\
    \     WHERE w.plate_id = ?\n\
    \     ORDER BY w.id ASC"

let get_by_name_and_project_query =
  (t2 string int ->? plate_t)
    (Printf.sprintf "SELECT %s FROM plates WHERE name = ? AND project_id = ?"
       select_fields)

let get_by_project_id_query =
  (int ->* plate_t)
    (Printf.sprintf "SELECT %s FROM plates WHERE project_id = ?" select_fields)

let count_project_plates_query =
  (t2 int string ->! int)
    "SELECT COUNT(*) FROM plates WHERE project_id = ? AND short_id LIKE ?"

(* OCaml functions *)

(** [create conn ?category ~name ~project_id ~product_id ~plate_format] creates
    a plate within a transaction. *)
let create (module Conn : Caqti_lwt.CONNECTION) ?category ~name ~project_id
    ~product_id ~(plate_format : plate_format) () =
  let open Exlab_core.Plate in
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let plate_format_str = string_of_plate_format plate_format in

  let temp_short_id = uid in

  (* Generate the plate row in the plate table *)
  let* plate =
    Conn.find add_query
      ( uid,
        temp_short_id,
        project_id,
        product_id,
        name,
        plate_format_str,
        created_at )
  in
  let id = plate.id in

  let prefix = Exlab_core.Plate.get_category_prefix category in
  let match_pattern = Printf.sprintf "%s-%%" prefix in
  let* count =
    Conn.find count_project_plates_query (project_id, match_pattern)
  in
  (* The row was inserted with temp_short_id = uid, which does not match the
     category prefix, so it is excluded from the count above. count is therefore
     the number of previously existing plates of this type, and the new plate
     gets count + 1. *)
  let short_id =
    Exlab_core.Plate.generate_short_id ~category ~project_id ~count
  in

  let* () = Conn.exec set_short_id_query (short_id, id) in

  let coordinates = generate_well_coordinates plate_format in

  if coordinates = [] then
    Lwt.return (Error (`Msg ("Invalid plate_format: " ^ plate_format_str)))
  else
    let* () =
      Lwt_list.fold_left_s
        (fun acc coord ->
          match acc with
          | Error e -> Lwt.return (Error e)
          | Ok () ->
              let* _ =
                Well.create (module Conn) ~plate_id:id ~coordinate:coord
              in
              Lwt.return (Ok ()))
        (Ok ()) coordinates
    in

    let* updated_plate =
      let* p_opt = Conn.find_opt get_by_id_query id in
      match p_opt with
      | Some p -> Lwt.return (Ok p)
      | None ->
          Lwt.return (Error (`Not_Found "Failed to fetch plate after creation"))
    in
    Lwt.return (Ok updated_plate)

type create_plate_with_layout = {
  name : string;
  layouts : (string * string) list; (* well, sample_short_id *)
}

module CoordSet = Set.Make (String)

(** [create_many_with_layouts ?category ~project_id ~plate_format plates]
    creates multiple plates and layouts transactionally. *)
let create_many_with_layouts ?category ~project_id ~plate_format
    (plates : create_plate_with_layout list) =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let open Lwt_result.Syntax in
      (* 1. Fetch all required samples upfront *)
      let all_sample_short_ids =
        List.concat_map (fun p -> List.map snd p.layouts) plates
        |> List.sort_uniq Exlab_core.String_utils.natural_compare
      in

      let* samples =
        Sample.get_many_by_short_ids_tx (module Conn) all_sample_short_ids
      in

      let* () =
        if List.length samples <> List.length all_sample_short_ids then
          Lwt.return
            (Error (`Bad_Request "One or more sample short IDs are invalid"))
        else Lwt.return (Ok ())
      in

      let sample_map =
        List.map
          (fun (s : Exlab_core.Types.sample) -> (s.short_id, s.id))
          samples
        |> List.to_seq |> Hashtbl.of_seq
      in

      (* 2. Create plates and their layouts *)
      let* created_plates =
        Lwt_list.fold_left_s
          (fun acc p_data ->
            match acc with
            | Error e -> Lwt.return (Error e)
            | Ok acc_plates ->
                let* new_plate =
                  create
                    (module Conn)
                    ?category ~name:p_data.name ~project_id ~product_id:None
                    ~plate_format ()
                in

                let* resolved_layout =
                  Lwt_list.fold_left_s
                    (fun acc_res (well, sample_short_id) ->
                      match acc_res with
                      | Error e -> Lwt.return (Error e)
                      | Ok (seen, acc) -> (
                          match
                            Exlab_core.Plate.normalize_coordinate plate_format
                              well
                          with
                          | Ok coord ->
                              if CoordSet.mem coord seen then
                                Lwt.return
                                  (Error
                                     (`Bad_Request
                                        (Printf.sprintf
                                           "Duplicate well coordinate '%s' \
                                            specified for plate '%s'"
                                           coord p_data.name)))
                              else
                                Lwt.return
                                  (Ok
                                     ( CoordSet.add coord seen,
                                       ( coord,
                                         Hashtbl.find sample_map sample_short_id
                                       )
                                       :: acc ))
                          | Error msg -> Lwt.return (Error (`Bad_Request msg))))
                    (Ok (CoordSet.empty, []))
                    p_data.layouts
                  |> Lwt.map (function
                    | Ok (_, items) -> Ok (List.rev items)
                    | Error e -> Error e)
                in

                let* () =
                  Well.update_layout_tx
                    (module Conn)
                    ~plate_id:new_plate.id resolved_layout
                in

                Lwt.return (Ok (new_plate :: acc_plates)))
          (Ok []) plates
      in
      Lwt.return (Ok (List.rev created_plates)))

(** [find conn plate_id] retrieves a plate by its ID within a transaction. *)
let find (module Conn : Caqti_lwt.CONNECTION) plate_id =
  let* result = Conn.find_opt get_by_id_query plate_id in
  Lwt.return (Ok result)

(** [add ?category ~name ~project_id ~product_id ~plate_format] creates a new
    plate. *)
let add ?category ~name ~project_id ~product_id ~(plate_format : plate_format)
    () =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      create
        (module Conn)
        ?category ~name ~project_id ~product_id ~plate_format ())

(** [get_by_id id] retrieves a plate by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_uid uid] retrieves a plate by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [get_by_short_id short_id] retrieves a plate by its short ID. *)
let get_by_short_id short_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_short_id_query short_id in
      Lwt_result.return row)

(** [get_all ()] retrieves all plates. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [get_all_for_user user_id] retrieves all plates accessible to a user. *)
let get_all_for_user user_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_for_user_query user_id in
      Lwt_result.return rows)

(** [get_by_project_id project_id] retrieves all plates for a given project. *)
let get_by_project_id project_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_project_id_query project_id in
      Lwt_result.return rows)

(** [get_by_name_and_project ~name ~project_id] retrieves a plate by name within
    a project. *)
let get_by_name_and_project ~name ~project_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row =
        Conn.find_opt get_by_name_and_project_query (name, project_id)
      in
      Lwt_result.return row)
