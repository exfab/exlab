let rec wait_for_db retry_count =
  let open Lwt.Syntax in
  if retry_count <= 0 then
    let* () =
      Lwt_io.eprintf
        "ERROR: Database not reachable after multiple attempts. Exiting.\n"
    in
    exit 1
  else
    let* () = Lwt_io.printl "Connecting to database..." in
    match%lwt Exlab_storage.Db.check_connection_silent () with
    | Ok () ->
        let* () = Lwt_io.printl "SUCCESS: Connected to database." in
        Lwt.return_unit
    | Error _ ->
        let* () =
          Lwt_io.printl "Database not ready yet, retrying in 2 seconds..."
        in
        let* () = Lwt_unix.sleep 2.0 in
        wait_for_db (retry_count - 1)

let () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = wait_for_db 15 in
     let* () = Lwt_io.printl "Running Migrations..." in
     let* () = Exlab_storage.Migrate.run () in

     let* () = Lwt_io.printl "Checking for default admin user..." in
     let* () =
       let* users_result = Exlab_storage.User.get_all () in
       match users_result with
       | Ok [] -> (
           let* () =
             Lwt_io.printl "No users found. Creating default admin account..."
           in
           let email =
             Sys.getenv_opt "DEFAULT_ADMIN_EMAIL"
             |> Option.value ~default:"admin@exlab.com"
           in
           let password =
             Sys.getenv_opt "DEFAULT_ADMIN_PASSWORD"
             |> Option.value ~default:"admin123"
           in
           let* result =
             Exlab_storage.User.add ~email ~password
               ~role:Exlab_core.Types.Admin
           in
           match result with
           | Ok _ -> Lwt_io.printl "Default admin account created successfully."
           | Error err ->
               Lwt_io.eprintf "Failed to create default admin: %s\n"
                 (Caqti_error.show err))
       | Ok _ -> Lwt_io.printl "Users exist. Skipping default admin creation."
       | Error err ->
           Lwt_io.eprintf "Error checking users: %s\n" (Caqti_error.show err)
     in

     let* () = Exlab_storage.Seed.run () in
     Lwt.return ());

  Lwt_main.run (Exlab_server.Lib.start ())
