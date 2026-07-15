(** This module is the main entry point for the ExLab web server. It is
    responsible for configuring and starting the Dream web server, as well as
    aggregating all the API routes from the different resource modules.

    The server is configured to:
    - Listen on all network interfaces (`0.0.0.0`).
    - Use a logger for recording request and response information.
    - Combine all API and static file routes into a single router.
    - Serve the frontend application's static files (HTML, CSS, JS).
    - A catch-all route that serves `index.html` for any requests that do not
      match an API endpoint or a static file, enabling client-side routing. *)

let scalar_html =
  {|<!DOCTYPE html>
<html>
  <head>
    <title>Exlab API Reference</title>
    <meta charset="utf-8" />
    <meta
      name="viewport"
      content="width=device-width, initial-scale=1" />
    <style>
      body {
        margin: 0;
        padding: 0;
      }
    </style>
  </head>
  <body>
    <!-- This script tag serves as the mount point for Scalar -->
    <script
      id="api-reference"
      data-url="/openapi.yaml"></script>
    
    <!-- Load the Scalar JS bundle -->
    <script src="https://cdn.jsdelivr.net/npm/@scalar/api-reference"></script>
  </body>
</html>|}

(** Configures and starts the Dream web server.

    This function aggregates all the route handlers from the various
    `*_routes.ml` modules, adds middleware for logging, and sets up routes for
    serving static assets. The server is started on port 8080 by default and
    listens on `0.0.0.0`, making it accessible from other machines on the
    network. *)
let start () =
  Dream.serve ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ (match Sys.getenv_opt "DREAM_SECRET" with
    | Some secret when secret <> "null" && secret <> "" ->
        Dream.set_secret secret
    | _ -> fun handler -> handler)
  @@ Dream.cookie_sessions
  @@ Dream.router
       [
         Auth_routes.login_route;
         Dream.scope "/" [ Auth.auth_required ]
           (Project_routes.routes @ Sample_routes.routes @ Result_routes.routes
          @ Product_routes.routes @ Plate_routes.routes @ Well_routes.routes
          @ Strain_routes.routes @ External_db_routes.routes
          @ Strain_link_routes.routes @ Community_routes.routes
          @ User_routes.routes @ Auth_routes.protected_routes
          @ Setting_routes.routes
           @ [
               Dream.get "/openapi.yaml" (fun request ->
                   Dream.from_filesystem "docs" "openapi.yaml" request);
               Dream.get "/docs" (fun _ -> Dream.html scalar_html);
             ]);
         Dream.get "/static/**" (Dream.static "./src/server/static");
         Dream.get "/**" (fun request ->
             Dream.from_filesystem "src/server/static" "index.html" request);
       ]
