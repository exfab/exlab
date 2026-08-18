(** Handling for well types and plate layouts. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

(* SQL Queries *)

(* Get plate by id *)
let get_by_plate_id_query =
  (int ->* t4 int int (option int) string)
    "SELECT id, plate_id, sample_id, coordinate\n\
    \     FROM wells\n\
    \     WHERE plate_id = ?"

(* Add a single well to a plate *)
let add_query =
  (t2 int string ->! int)
    "INSERT INTO wells (plate_id, coordinate) VALUES (?, ?) RETURNING id"

(* Assign a sample to a well *)
let update_sample_query =
  (t2 (option int) int ->. unit) "UPDATE wells SET sample_id = ? WHERE id = ?"

(* Assign sample to well based on coordinate on plate *)
let update_sample_by_coord_query =
  (t3 (option int) int string ->. unit)
    "UPDATE wells SET sample_id = ? WHERE plate_id = ? AND coordinate = ?"

(* Query plates to find every well a specific sample is present *)
let get_locations_by_sample_query =
  (int ->* t4 int string string string)
    "\n\
    \     SELECT p.id, p.short_id, p.name, w.coordinate\n\
    \     FROM wells w\n\
    \     JOIN plates p ON w.plate_id = p.id\n\
    \     WHERE w.sample_id = ?\n\
    \     ORDER BY p.id DESC, w.coordinate ASC\n\
    \    "

(* Return distinct categories of samples currently on a plate *)
let get_plate_categories_query =
  (int ->* string)
    "SELECT DISTINCT s.category\n\
    \     FROM wells w\n\
    \     JOIN samples s ON w.sample_id = s.id\n\
    \     WHERE w.plate_id = ? AND w.sample_id IS NOT NULL"

(* Ocaml functions *)

(** [get_by_plate_id plate_id] retrieves all wells for a given plate. *)
let get_by_plate_id plate_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_plate_id_query plate_id in
      let wells =
        List.map
          (fun (id, plate_id, sample_id, coordinate) ->
            { id; plate_id; sample_id; coordinate })
          rows
      in
      Lwt_result.return wells)

(* Decoupled transaction logic, atomic actions *)

(** [create conn ~plate_id ~coordinate] creates a new well within a transaction.
*)
let create (module Conn : Caqti_lwt.CONNECTION) ~plate_id ~coordinate =
  Conn.find add_query (plate_id, coordinate)

(** [set_sample_by_coord conn ~plate_id ~coordinate ~sample_id] sets a sample in
    a well by coordinate within a transaction. *)
let set_sample_by_coord (module Conn : Caqti_lwt.CONNECTION) ~plate_id
    ~coordinate ~sample_id =
  Conn.exec update_sample_by_coord_query (sample_id, plate_id, coordinate)

(** [set_sample_by_id conn ~well_id ~sample_id] sets a sample in a well by ID
    within a transaction. *)
let set_sample_by_id (module Conn : Caqti_lwt.CONNECTION) ~well_id ~sample_id =
  Conn.exec update_sample_query (sample_id, well_id)

(** [find_locations conn sample_id] finds all well locations for a sample within
    a transaction. *)
let find_locations (module Conn : Caqti_lwt.CONNECTION) sample_id =
  Conn.collect_list get_locations_by_sample_query sample_id

(* Decoupled transaction logic, transactions *)

(** [assign_by_coordinate ~plate_id ~coordinate ~sample_id] assigns a sample to
    a well by its coordinate. *)
let assign_by_coordinate ~plate_id ~coordinate ~sample_id =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      set_sample_by_coord (module Conn) ~plate_id ~coordinate ~sample_id)

(** [assign_by_id ~well_id ~sample_id] assigns a sample to a well by its
    internal ID. *)
let assign_by_id ~well_id ~sample_id =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      set_sample_by_id (module Conn) ~well_id ~sample_id)

(** [get_locations_by_sample_id sample_id] retrieves all well locations where a
    specific sample is present. *)
let get_locations_by_sample_id sample_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      find_locations (module Conn) sample_id)

(** [get_plate_categories plate_id] retrieves distinct categories of samples
    currently on a plate. *)
let get_plate_categories plate_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* categories_str =
        Conn.collect_list get_plate_categories_query plate_id
      in
      Lwt_result.return
        (List.map
           (fun s -> Utils.unwrap_or_fail (sample_category_of_string s))
           categories_str))

(** [update_layout_tx conn ~plate_id layout] updates the sample assignments
    within a transaction. *)
let update_layout_tx (module Conn : Caqti_lwt.CONNECTION) ~plate_id
    (layout : (string * int) list) =
  let open Lwt_result.Syntax in
  let* () =
    Lwt_list.iter_s
      (fun (coordinate, sample_id) ->
        match%lwt
          set_sample_by_coord
            (module Conn)
            ~plate_id ~coordinate ~sample_id:(Some sample_id)
        with
        | Ok () -> Lwt.return_unit
        | Error err -> Lwt.fail_with (Caqti_error.show err))
      layout
    |> Lwt_result.ok
  in
  Lwt.return (Ok ())

(** [update_layout ~plate_id layout] updates the sample assignments for multiple
    wells on a plate at once. *)
let update_layout ~plate_id (layout : (string * int) list) =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      update_layout_tx (module Conn) ~plate_id layout)

(** [update_many_layouts updates] updates the sample assignments for multiple
    plates within a single transaction. *)
let update_many_layouts (updates : (int * (string * int) list) list) =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let open Lwt_result.Syntax in
      let* () =
        Lwt_list.iter_s
          (fun (plate_id, layout) ->
            match%lwt update_layout_tx (module Conn) ~plate_id layout with
            | Ok () -> Lwt.return_unit
            | Error err -> Lwt.fail_with (Caqti_error.show err))
          updates
        |> Lwt_result.ok
      in
      Lwt.return (Ok ()))

(** [unassign_bulk ~plate_id wells] removes the sample assignment for multiple
    wells on a plate at once.
    @return [Ok ()] if all updates succeed, or a Caqti error on failure. *)
let unassign_bulk ~plate_id (wells : string list) =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let open Lwt_result.Syntax in
      let* () =
        Lwt_list.iter_s
          (fun coordinate ->
            match%lwt
              set_sample_by_coord
                (module Conn)
                ~plate_id ~coordinate ~sample_id:None
            with
            | Ok () -> Lwt.return_unit
            | Error err -> Lwt.fail_with (Caqti_error.show err))
          wells
        |> Lwt_result.ok
      in
      Lwt.return (Ok ()))
