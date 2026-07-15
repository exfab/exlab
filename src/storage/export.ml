(** Database export operations. *)

open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

type plate_csv_row = string * string option * string option
(** Type representing a row in a plate CSV export. *)

(* SQL Queries *)

let get_plate_wells_for_csv_query =
  (int ->* t2 string (option string))
    "SELECT w.coordinate, s.short_id\n\
    \     FROM wells w\n\
    \     LEFT JOIN samples s ON w.sample_id = s.id\n\
    \     WHERE w.plate_id = ?\n\
    \     ORDER BY CAST(SUBSTRING(w.coordinate FROM '[0-9]+') AS INTEGER), \
     SUBSTRING(w.coordinate FROM '[A-Z]+')"

(* OCaml functions *)

(** [get_plate_wells_for_csv plate_id] retrieves well coordinates and sample
    short IDs for a plate. *)
let get_plate_wells_for_csv plate_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      Conn.collect_list get_plate_wells_for_csv_query plate_id)
