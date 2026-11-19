(** Versioned PostgreSQL schema migration runner. *)

val run : unit -> unit Lwt.t
(** Execute all pending schema migrations in order inside a transaction, exiting
    the process if a migration fails. *)

val m008_short_id_counters :
  (module Caqti_lwt.CONNECTION) ->
  (unit, [> Caqti_error.call_or_retrieve ]) result Lwt.t
(** Idempotent migration v8 ([short_id_counters] creation and backfill), exposed
    for integration testing of counter backfill invariants. *)
