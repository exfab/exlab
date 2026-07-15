(** Database connection and pool management. *)

open Lwt.Infix

(** The database connection URL. Can be provided as a full [DATABASE_URL] or
    constructed from individual variables: [DB_USER], [DB_PASS], [DB_HOST],
    [DB_PORT], [DB_NAME]. *)
let connection_url =
  match Sys.getenv_opt "DATABASE_URL" with
  | Some url -> url
  | None ->
      let user =
        Sys.getenv_opt "DB_USER"
        |> Option.value ~default:"user"
        |> Uri.pct_encode
      in
      let pass =
        Sys.getenv_opt "DB_PASS"
        |> Option.value ~default:"password"
        |> Uri.pct_encode
      in
      let host =
        Sys.getenv_opt "DB_HOST" |> Option.value ~default:"localhost"
      in
      let port = Sys.getenv_opt "DB_PORT" |> Option.value ~default:"5433" in
      let name =
        Sys.getenv_opt "DB_NAME" |> Option.value ~default:"exlab_dev"
      in
      (* If running locally with docker-compose, set DB_PORT=5433 *)
      Printf.sprintf "postgresql://%s:%s@%s:%s/%s" user pass host port name

(** The size of the database connection pool, loaded from [DB_POOL_SIZE]. Cloud
    SQL has connection limits; ensure this is balanced across app instances. *)
let pool_size = try int_of_string (Sys.getenv "DB_POOL_SIZE") with _ -> 10

(** Configuration for the Caqti connection pool. *)
let pool_config = Caqti_pool_config.create ~max_size:pool_size ()

(** The Caqti connection pool. *)
let pool =
  match
    Caqti_lwt_unix.connect_pool ~pool_config (Uri.of_string connection_url)
  with
  | Ok pool -> pool
  | Error err ->
      let msg = "Pool creation failed: " ^ Caqti_error.show err in
      prerr_endline msg;
      failwith msg

(** [request f] executes the function [f] using a connection from the pool. *)
let request f = Caqti_lwt_unix.Pool.use f pool

(** [transaction f] executes the function [f] within a database transaction. If
    [f] returns an [Error] or raises an exception, the transaction is rolled
    back. *)
let transaction f =
  request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let open Lwt_result.Syntax in
      let* () = Conn.start () in

      Lwt.catch
        (fun () ->
          match%lwt f (module Conn : Caqti_lwt.CONNECTION) with
          | Ok res ->
              let* () = Conn.commit () in
              Lwt.return (Ok res)
          | Error err ->
              let* () = Conn.rollback () in
              Lwt.return (Error err))
        (fun exn ->
          let%lwt _ = Conn.rollback () in
          Lwt.fail exn))

(** [check_connection ()] verifies that the database is reachable. Exits the
    process if the connection fails. *)
let check_connection () =
  let check_query =
    let open Caqti_request.Infix in
    (Caqti_type.unit ->! Caqti_type.unit) "SELECT 1"
  in

  let work (module Conn : Caqti_lwt.CONNECTION) =
    let%lwt _result = Conn.find check_query () in
    Lwt.return (Ok ())
  in

  match%lwt request work with
  | Ok () -> Lwt_io.printl "SUCCESS: Database connected successfully."
  | Error err ->
      Lwt_io.eprintf "ERROR: Failed to connect to DB: %s\n"
        (Caqti_error.show err)
      >>= fun () -> exit 1

(** [check_connection_silent ()] verifies that the database is reachable without
    exiting the process on failure. *)
let check_connection_silent () =
  let check_query =
    let open Caqti_request.Infix in
    (Caqti_type.unit ->! Caqti_type.unit) "SELECT 1"
  in

  let work (module Conn : Caqti_lwt.CONNECTION) =
    let%lwt _result = Conn.find check_query () in
    Lwt.return (Ok ())
  in

  request work
