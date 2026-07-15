(** Handling for the ResultValue data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

(** Represents the parent entity for a result value. *)
type parent = Sample of int | Plate of int

let result_payload_t =
  let encode payload =
    Ok (payload |> ResultPayload.yojson_of_t |> Yojson.Safe.to_string)
  in
  let decode json_str =
    try Ok (json_str |> Yojson.Safe.from_string |> ResultPayload.t_of_yojson)
    with _ -> Error "Failed to parse result payload JSON"
  in
  custom ~encode ~decode string

let select_fields =
  "id, uid, sample_id, plate_id, result_definition_id, value, created_at, \
   updated_at"

let result_value_t =
  let encode (r : Exlab_core.Types.result_value) =
    Ok
      ( r.id,
        r.uid,
        r.sample_id,
        r.plate_id,
        r.result_definition_id,
        r.value,
        r.created_at,
        r.updated_at )
  in
  let decode
      ( id,
        uid,
        sample_id,
        plate_id,
        result_definition_id,
        value,
        created_at,
        updated_at ) =
    Ok
      ({
         id;
         uid;
         sample_id;
         plate_id;
         result_definition_id;
         value;
         created_at;
         updated_at;
       }
        : Exlab_core.Types.result_value)
  in
  Caqti_type.custom ~encode ~decode
    (t8 int string (option int) (option int) int (option result_payload_t) float
       float)

(* SQL Queries *)

let get_by_sample_id_query =
  (int ->* result_value_t)
    (Printf.sprintf "SELECT %s FROM result_values WHERE sample_id = ?"
       select_fields)

let get_by_plate_id_query =
  (int ->* result_value_t)
    (Printf.sprintf "SELECT %s FROM result_values WHERE plate_id = ?"
       select_fields)

let get_by_id_query =
  (int ->? result_value_t)
    (Printf.sprintf "SELECT %s FROM result_values WHERE id = ?" select_fields)

let get_by_uid_query =
  (string ->? result_value_t)
    (Printf.sprintf "SELECT %s FROM result_values WHERE uid = ?" select_fields)

let add_query =
  (t6 string (option int) (option int) int (option result_payload_t) float
  ->! result_value_t)
    (Printf.sprintf
       "INSERT INTO result_values\n\
       \        (uid, sample_id, plate_id, result_definition_id, value, \
        created_at)\n\
       \        VALUES (?, ?, ?, ?, ?::jsonb, ?)\n\
       \        RETURNING %s\n\
       \        "
       select_fields)

let update_query =
  (t3 (option result_payload_t) float int ->! result_value_t)
    (Printf.sprintf
       "UPDATE result_values\n\
       \        SET value = ?::jsonb, updated_at = ?\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

let get_by_plate_and_its_samples_query =
  (t2 int int ->* result_value_t)
    "SELECT r.id, r.uid, r.sample_id, r.plate_id, r.result_definition_id, \
     r.value, r.created_at, r.updated_at FROM result_values r WHERE r.plate_id \
     = ? OR r.sample_id IN (SELECT sample_id FROM wells WHERE plate_id = ? AND \
     sample_id IS NOT NULL)"

let get_by_project_id_query =
  (t2 int int ->* result_value_t)
    "SELECT r.id, r.uid, r.sample_id, r.plate_id, r.result_definition_id, \
     r.value, r.created_at, r.updated_at FROM result_values r LEFT JOIN \
     samples s ON r.sample_id = s.id LEFT JOIN plates p ON r.plate_id = p.id \
     WHERE s.project_id = ? OR p.project_id = ?"

(* OCaml Functions *)

(** [get_by_sample_id sample_id] retrieves all result values associated with a
    sample. *)
let get_by_sample_id sample_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_sample_id_query sample_id in
      Lwt_result.return rows)

(** [get_by_plate_id plate_id] retrieves all result values associated with a
    plate. *)
let get_by_plate_id plate_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_by_plate_id_query plate_id in
      Lwt_result.return rows)

(** [get_by_plate_and_its_samples plate_id] retrieves all result values for the
    plate and any samples in its wells. *)
let get_by_plate_and_its_samples plate_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        Conn.collect_list get_by_plate_and_its_samples_query (plate_id, plate_id)
      in
      Lwt_result.return rows)

(** [get_by_project_id project_id] retrieves all result values for samples or
    plates in a project. *)
let get_by_project_id project_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows =
        Conn.collect_list get_by_project_id_query (project_id, project_id)
      in
      Lwt_result.return rows)

(** [get_by_id id] retrieves a result value by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_uid uid] retrieves a result value by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [insert conn ~parent ~definition ~value] inserts a new result value within a
    transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~parent ~definition ~value =
  let open Lwt_result.Syntax in
  (* Validate *)
  let* () =
    match value with
    | Some payload -> (
        match
          Exlab_core.Result.validate_payload definition.data_type payload
        with
        | Ok () -> Lwt.return (Ok ())
        | Error msg -> Lwt.return (Error (`Msg msg)))
    | None -> Lwt.return (Ok ())
  in

  (* Core Logic*)
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in
  let sample_id, plate_id =
    match parent with
    | Sample id -> (Some id, None)
    | Plate id -> (None, Some id)
  in

  let* row =
    Conn.find add_query
      (uid, sample_id, plate_id, definition.id, value, created_at)
  in

  Lwt.return (Ok row)

(** [add ~parent ~result_definition_id ~value] creates a new result value. *)
let add ~parent ~result_definition_id ~value =
  let open Lwt_result.Syntax in
  let* definition =
    let* def_opt = Result_definition.get_by_id result_definition_id in
    match def_opt with
    | Some def -> Lwt.return (Ok def)
    | None -> Lwt.return (Error (`Not_Found "Result definition not found"))
  in

  (* Validation *)
  let* () =
    match value with
    | Some payload -> (
        match
          Exlab_core.Result.validate_payload definition.data_type payload
        with
        | Ok () -> Lwt.return (Ok ())
        | Error e -> Lwt.return (Error (`Msg e)))
    | None -> Lwt.return (Ok ())
  in

  (* Transaction *)
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert (module Conn) ~parent ~definition ~value)

(** [update ~id ~value] updates an existing result value. *)
let update ~id ~value =
  let open Lwt_result.Syntax in
  let* existing_opt = get_by_id id in
  match existing_opt with
  | None -> Lwt.return (Error (`Not_Found "Result value not found"))
  | Some existing -> (
      let* def_opt =
        Result_definition.get_by_id existing.result_definition_id
      in
      match def_opt with
      | None -> Lwt.return (Error (`Not_Found "Result definition not found"))
      | Some definition ->
          let* () =
            match value with
            | Some payload -> (
                match
                  Exlab_core.Result.validate_payload definition.data_type
                    payload
                with
                | Ok () -> Lwt.return (Ok ())
                | Error e -> Lwt.return (Error (`Msg e)))
            | None -> Lwt.return (Ok ())
          in
          let updated_at = Unix.time () in
          Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
              let* row = Conn.find update_query (value, updated_at, id) in
              Lwt_result.return row))

(** [append_timeseries_point ~id ~time ~scalar_value] appends a single data
    point to an existing TimeSeries result. *)
let append_timeseries_point ~id ~time
    ~(scalar_value : Exlab_core.Types.ResultPayload.t) =
  let open Lwt_result.Syntax in
  let* existing_opt = get_by_id id in
  match existing_opt with
  | None -> Lwt.return (Error (`Not_Found "Result value not found"))
  | Some existing -> (
      let* def_opt =
        Result_definition.get_by_id existing.result_definition_id
      in
      match def_opt with
      | None -> Lwt.return (Error (`Not_Found "Result definition not found"))
      | Some definition -> (
          let get_time current_points =
            match time with
            | Some t -> t
            | None -> (
                match current_points with
                | [] -> 1
                | _ ->
                    let max_t =
                      List.fold_left
                        (fun acc (t, _) -> if t > acc then t else acc)
                        0 current_points
                    in
                    max_t + 1)
          in
          let update_db new_value =
            let updated_at = Unix.time () in
            Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
                let* row =
                  Conn.find update_query (Some new_value, updated_at, id)
                in
                Lwt_result.return row)
          in
          match (definition.data_type, scalar_value) with
          | ( Exlab_core.Types.ResultType.StringSeries,
              Exlab_core.Types.ResultPayload.String v ) ->
              let current_points =
                match existing.value with
                | Some (Exlab_core.Types.ResultPayload.StringSeries pts) -> pts
                | _ -> []
              in
              let final_time = get_time current_points in
              let new_points = (final_time, v) :: current_points in
              let sorted =
                List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2) new_points
              in
              update_db (Exlab_core.Types.ResultPayload.StringSeries sorted)
          | ( Exlab_core.Types.ResultType.IntegerSeries,
              Exlab_core.Types.ResultPayload.Integer v ) ->
              let current_points =
                match existing.value with
                | Some (Exlab_core.Types.ResultPayload.IntegerSeries pts) -> pts
                | _ -> []
              in
              let final_time = get_time current_points in
              let new_points = (final_time, v) :: current_points in
              let sorted =
                List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2) new_points
              in
              update_db (Exlab_core.Types.ResultPayload.IntegerSeries sorted)
          | ( Exlab_core.Types.ResultType.FloatSeries,
              Exlab_core.Types.ResultPayload.Float v ) ->
              let current_points =
                match existing.value with
                | Some (Exlab_core.Types.ResultPayload.FloatSeries pts) -> pts
                | _ -> []
              in
              let final_time = get_time current_points in
              let new_points = (final_time, v) :: current_points in
              let sorted =
                List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2) new_points
              in
              update_db (Exlab_core.Types.ResultPayload.FloatSeries sorted)
          | ( Exlab_core.Types.ResultType.BooleanSeries,
              Exlab_core.Types.ResultPayload.Boolean v ) ->
              let current_points =
                match existing.value with
                | Some (Exlab_core.Types.ResultPayload.BooleanSeries pts) -> pts
                | _ -> []
              in
              let final_time = get_time current_points in
              let new_points = (final_time, v) :: current_points in
              let sorted =
                List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2) new_points
              in
              update_db (Exlab_core.Types.ResultPayload.BooleanSeries sorted)
          | ( Exlab_core.Types.ResultType.DatetimeSeries,
              Exlab_core.Types.ResultPayload.Datetime v ) ->
              let current_points =
                match existing.value with
                | Some (Exlab_core.Types.ResultPayload.DatetimeSeries pts) ->
                    pts
                | _ -> []
              in
              let final_time = get_time current_points in
              let new_points = (final_time, v) :: current_points in
              let sorted =
                List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2) new_points
              in
              update_db (Exlab_core.Types.ResultPayload.DatetimeSeries sorted)
          | ( Exlab_core.Types.ResultType.DateSeries,
              Exlab_core.Types.ResultPayload.Date v ) ->
              let current_points =
                match existing.value with
                | Some (Exlab_core.Types.ResultPayload.DateSeries pts) -> pts
                | _ -> []
              in
              let final_time = get_time current_points in
              let new_points = (final_time, v) :: current_points in
              let sorted =
                List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2) new_points
              in
              update_db (Exlab_core.Types.ResultPayload.DateSeries sorted)
          | ( Exlab_core.Types.ResultType.FileLinkSeries,
              Exlab_core.Types.ResultPayload.FileLink v ) ->
              let current_points =
                match existing.value with
                | Some (Exlab_core.Types.ResultPayload.FileLinkSeries pts) ->
                    pts
                | _ -> []
              in
              let final_time = get_time current_points in
              let new_points = (final_time, v) :: current_points in
              let sorted =
                List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2) new_points
              in
              update_db (Exlab_core.Types.ResultPayload.FileLinkSeries sorted)
          | _ ->
              Lwt.return
                (Error
                   (`Bad_Request
                      "Cannot append mismatched or non-scalar value to series"))
          ))
