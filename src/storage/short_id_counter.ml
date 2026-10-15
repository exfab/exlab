(** Atomic sequence counters for generating unique short IDs. *)

open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

type scope =
  | Source_sample
  | Experimental_sample
  | Source_plate
  | Experimental_plate

let string_of_scope = function
  | Source_sample -> "source_sample"
  | Experimental_sample -> "experimental_sample"
  | Source_plate -> "source_plate"
  | Experimental_plate -> "experimental_plate"

let plate_scope_of_category category =
  match Exlab_core.Plate.get_category_prefix category with
  | "S" -> Source_plate
  | _ -> Experimental_plate

let scope_t =
  let encode s = Ok (string_of_scope s) in
  let decode _ = Error "Decode not implemented for counter_scope" in
  Caqti_type.custom ~encode ~decode string

let reserve_query =
  (t3 scope_t int int ->! int)
    {|
      INSERT INTO short_id_counters (scope, scope_id, last_value)
      VALUES (?, ?, ?)
      ON CONFLICT (scope, scope_id)
      DO UPDATE SET last_value = short_id_counters.last_value + EXCLUDED.last_value
      RETURNING last_value
    |}

(** [reserve conn ~scope ~scope_id ~count] atomically reserves [count]
    consecutive 1-based sequence numbers for [(scope, scope_id)] and returns the
    first reserved number. Raises [Invalid_argument] if [count < 1]. *)
let reserve (module Conn : Caqti_lwt.CONNECTION) ~scope ~scope_id ~count =
  if count < 1 then invalid_arg "Short_id_counter.reserve: count must be >= 1"
  else
    let* last_value = Conn.find reserve_query (scope, scope_id, count) in
    Lwt.return (Ok (last_value - count + 1))
