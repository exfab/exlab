(** Main entry point for the ExLab HTTP server and route router. *)

val start : unit -> unit Lwt.t
(** Start the Dream web server on [0.0.0.0:8080] with session middleware, API
    routes, OpenAPI reference docs, and SPA static fallback. *)
