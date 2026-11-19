(** Database connection pool and transaction execution. *)

val request :
  ((module Caqti_lwt.CONNECTION) ->
  ('a, ([> Caqti_error.t ] as 'e)) result Lwt.t) ->
  ('a, 'e) result Lwt.t
(** [request f] executes [f] using a connection acquired from the pool. *)

val transaction :
  ((module Caqti_lwt.CONNECTION) ->
  ('a, ([> Caqti_error.t ] as 'e)) result Lwt.t) ->
  ('a, 'e) result Lwt.t
(** [transaction f] executes [f] inside a database transaction, committing on
    [Ok] and rolling back on [Error] or exception. *)

val check_connection_silent : unit -> (unit, [> Caqti_error.t ]) result Lwt.t
(** Verify that the database is reachable via [SELECT 1] without exiting on
    failure. *)

val check_connection : unit -> unit Lwt.t
(** Verify that the database is reachable, logging and exiting the process with
    code [1] on failure. *)
