(** Database schema migrations. *)

open Caqti_request.Infix
open Caqti_type.Std
open Lwt_result.Syntax

(* ========================================================================== *)
(* CORE MIGRATION TYPES AND STATE TABLE                                       *)
(* ========================================================================== *)

type migration = {
  version : int;
  name : string;
  up : (module Caqti_lwt.CONNECTION) -> (unit, Caqti_error.t) result Lwt.t;
}

let create_schema_migrations_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS schema_migrations (
         version INT PRIMARY KEY,
         applied_at REAL NOT NULL DEFAULT (extract(epoch from now()))
     );
    |}

let get_applied_versions_query =
  (unit ->* int) "SELECT version FROM schema_migrations ORDER BY version ASC"

let insert_migration_version_query =
  (int ->. unit) "INSERT INTO schema_migrations (version) VALUES (?)"

let get_applied_versions (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list get_applied_versions_query ()

(* ========================================================================== *)
(* MIGRATION V1: INITIAL SCHEMA                                               *)
(* ========================================================================== *)

let create_projects_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS projects (
     id SERIAL PRIMARY KEY,
     uid TEXT NOT NULL UNIQUE,
     short_id TEXT UNIQUE NOT NULL,
     name TEXT NOT NULL,
     description TEXT,
     status TEXT NOT NULL DEFAULT 'Active',
     contact_name TEXT,
     owner TEXT,
     created_at REAL NOT NULL,
     updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
     );
   |}

let create_trigger_function =
  (unit ->. unit)
    {|
     CREATE OR REPLACE FUNCTION update_updated_at_column()
     RETURNS TRIGGER AS $$
     BEGIN
         NEW.updated_at = extract(epoch from now());
         RETURN NEW;
     END;
     $$ language 'plpgsql';
     |}

let drop_old_projects_trigger =
  (unit ->. unit)
    "DROP TRIGGER IF EXISTS update_projects_updated_at ON projects;"

let attach_projects_trigger =
  (unit ->. unit)
    {|
     CREATE TRIGGER update_projects_updated_at
     BEFORE UPDATE ON projects
     FOR EACH ROW
     EXECUTE PROCEDURE update_updated_at_column();
     |}

let create_users_table =
  (unit ->. unit)
    {|
      CREATE TABLE IF NOT EXISTS users (
          id SERIAL PRIMARY KEY,
          uid TEXT NOT NULL UNIQUE,
          email TEXT NOT NULL UNIQUE,
          password_hash TEXT NOT NULL,
          role TEXT NOT NULL CHECK (role IN ('admin', 'lab_manager', 'project_manager', 'project_user')),
          created_at REAL NOT NULL,
          updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
      );
     |}

let drop_old_users_trigger =
  (unit ->. unit) "DROP TRIGGER IF EXISTS update_users_updated_at ON users;"

let attach_users_trigger =
  (unit ->. unit)
    {|
      CREATE TRIGGER update_users_updated_at
      BEFORE UPDATE ON users
      FOR EACH ROW
      EXECUTE PROCEDURE update_updated_at_column();
     |}

let create_project_users_table =
  (unit ->. unit)
    {|
      CREATE TABLE IF NOT EXISTS project_users (
          id SERIAL PRIMARY KEY,
          project_id INT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
          user_id INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
          UNIQUE(project_id, user_id)
      );
     |}

let create_samples_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS samples (
         id SERIAL PRIMARY KEY,
         uid TEXT NOT NULL UNIQUE,
         short_id TEXT UNIQUE NOT NULL,
         project_id INT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
         sample_type TEXT NOT NULL,
         status TEXT NOT NULL DEFAULT 'Active',
         category TEXT NOT NULL DEFAULT 'Experimental' CHECK (category IN ('Source', 'Experimental')),
         parent_sample_id INT REFERENCES samples(id) ON DELETE SET NULL,
         strain_id INT REFERENCES strains(id) ON DELETE SET NULL,
         created_at REAL NOT NULL,
         updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
     );
     |}

let drop_old_samples_trigger =
  (unit ->. unit) "DROP TRIGGER IF EXISTS update_samples_updated_at ON samples;"

let attach_samples_trigger =
  (unit ->. unit)
    {|
     CREATE TRIGGER update_samples_updated_at
     BEFORE UPDATE ON samples
     FOR EACH ROW
     EXECUTE PROCEDURE update_updated_at_column();
     |}

let create_result_categories_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS result_categories (
         id SERIAL PRIMARY KEY,
         uid TEXT NOT NULL UNIQUE,
         name TEXT NOT NULL UNIQUE,
         description TEXT
       );
     |}

let create_result_definitions_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS result_definitions (
         id SERIAL PRIMARY KEY,
         uid TEXT NOT NULL UNIQUE,
         short_id TEXT,
         name TEXT NOT NULL UNIQUE,
         description TEXT,
         data_type TEXT NOT NULL,
         unit TEXT,
         category_id INT REFERENCES result_categories(id) ON DELETE SET NULL,
         is_required BOOLEAN NOT NULL DEFAULT FALSE,
         created_at REAL NOT NULL,
         updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
       );
     |}

let create_result_values_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS result_values (
        id SERIAL PRIMARY KEY,
        uid TEXT NOT NULL UNIQUE,
        sample_id INT REFERENCES samples(id) ON DELETE CASCADE,
        plate_id INT REFERENCES plates(id) ON DELETE CASCADE,
        result_definition_id INT NOT NULL REFERENCES result_definitions(id) ON DELETE RESTRICT,
        value JSONB,
        created_at REAL NOT NULL,
        updated_at REAL NOT NULL DEFAULT (extract(epoch from now())),
        CONSTRAINT check_result_owner CHECK (
          (sample_id IS NOT NULL AND plate_id IS NULL) OR
          (sample_id IS NULL AND plate_id IS NOT NULL)
        )
      );
     |}

let drop_old_result_values_trigger =
  (unit ->. unit)
    "DROP TRIGGER IF EXISTS update_result_values_updated_at ON result_values;"

let attach_result_values_trigger =
  (unit ->. unit)
    {|
     CREATE TRIGGER update_result_values_updated_at
     BEFORE UPDATE ON result_values
     FOR EACH ROW
     EXECUTE PROCEDURE update_updated_at_column();
     |}

let create_products_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS products (
         id SERIAL PRIMARY KEY,
         uid TEXT NOT NULL UNIQUE,
         short_id TEXT NOT NULL,
         name TEXT NOT NULL UNIQUE,
         brand TEXT,
         manufacturer_part_number TEXT,
         description TEXT,
         created_at REAL NOT NULL,
         updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
       );
     |}

let create_plates_table =
  (unit ->. unit)
    {|
   CREATE TABLE IF NOT EXISTS plates (
       id SERIAL PRIMARY KEY,
       uid TEXT NOT NULL UNIQUE,
       short_id TEXT NOT NULL,
       name TEXT NOT NULL,
       project_id INT NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
       product_id INT REFERENCES products(id) ON DELETE SET NULL,
       plate_format TEXT NOT NULL,
       created_at REAL NOT NULL,
       updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
     );
   |}

let drop_old_plates_trigger =
  (unit ->. unit) "DROP TRIGGER IF EXISTS update_plates_updated_at ON plates;"

let attach_plates_trigger =
  (unit ->. unit)
    {|
      CREATE TRIGGER update_plates_updated_at
      BEFORE UPDATE ON plates
      FOR EACH ROW
      EXECUTE PROCEDURE update_updated_at_column();
    |}

let create_wells_table =
  (unit ->. unit)
    {|
      CREATE TABLE IF NOT EXISTS wells (
          id SERIAL PRIMARY KEY,
          plate_id INT NOT NULL REFERENCES plates(id) ON DELETE CASCADE,
          sample_id INT REFERENCES samples(id) ON DELETE SET NULL,
          coordinate TEXT NOT NULL,
          UNIQUE(plate_id, coordinate)
      );
     |}

let create_strains_table =
  (unit ->. unit)
    {|
      CREATE TABLE IF NOT EXISTS strains (
          id SERIAL PRIMARY KEY,
          uid TEXT NOT NULL UNIQUE,
          species_name TEXT NOT NULL,
          strain_name TEXT NOT NULL,
          genotype TEXT,
          parent_strain_id INT,
          notes TEXT,
          created_at REAL NOT NULL,
          updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
      );
     |}

let drop_old_strains_trigger =
  (unit ->. unit) "DROP TRIGGER IF EXISTS update_strains_updated_at ON strains;"

let attach_strains_trigger =
  (unit ->. unit)
    {|
      CREATE TRIGGER update_strains_updated_at
      BEFORE UPDATE ON strains
      FOR EACH ROW
      EXECUTE PROCEDURE update_updated_at_column();
     |}

let create_external_db_definitions_table =
  (unit ->. unit)
    {|
      CREATE TABLE IF NOT EXISTS external_db_definitions (
          id SERIAL PRIMARY KEY,
          uid TEXT NOT NULL UNIQUE,
          name TEXT NOT NULL,
          url_template TEXT,
          created_at REAL NOT NULL,
          updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
      );
     |}

let drop_old_external_db_definitions_trigger =
  (unit ->. unit)
    "DROP TRIGGER IF EXISTS update_external_db_definitions_updated_at ON \
     external_db_definitions;"

let attach_external_db_definitions_trigger =
  (unit ->. unit)
    {|
      CREATE TRIGGER update_external_db_definitions_updated_at
      BEFORE UPDATE ON external_db_definitions
      FOR EACH ROW
      EXECUTE PROCEDURE update_updated_at_column();
     |}

let create_strain_external_links_table =
  (unit ->. unit)
    {|
      CREATE TABLE IF NOT EXISTS strain_external_links (
          id SERIAL PRIMARY KEY,
          uid TEXT NOT NULL UNIQUE,
          strain_id INT NOT NULL REFERENCES strains(id) ON DELETE CASCADE,
          external_db_definition_id INT NOT NULL REFERENCES external_db_definitions(id) ON DELETE CASCADE,
          value TEXT NOT NULL,
          created_at REAL NOT NULL
      );
     |}

let create_indices =
  List.map
    (fun (name, table, column) ->
      (unit ->. unit)
        (Printf.sprintf "CREATE INDEX IF NOT EXISTS %s ON %s (%s)" name table
           column))
    [
      ("idx_samples_project_id", "samples", "project_id");
      ("idx_samples_parent_sample_id", "samples", "parent_sample_id");
      ("idx_samples_strain_id", "samples", "strain_id");
      ("idx_plates_project_id", "plates", "project_id");
      ("idx_wells_sample_id", "wells", "sample_id");
      ("idx_result_values_sample_id", "result_values", "sample_id");
      ("idx_result_values_plate_id", "result_values", "plate_id");
      ( "idx_result_values_result_definition_id",
        "result_values",
        "result_definition_id" );
      ( "idx_strain_external_links_strain_id",
        "strain_external_links",
        "strain_id" );
      ( "idx_strain_external_links_external_db_definition_id",
        "strain_external_links",
        "external_db_definition_id" );
    ]

let m001_initial_schema (module Conn : Caqti_lwt.CONNECTION) =
  let* () = Conn.exec create_projects_table () in
  let* () = Conn.exec create_trigger_function () in
  let* () = Conn.exec drop_old_projects_trigger () in
  let* () = Conn.exec attach_projects_trigger () in
  let* () = Conn.exec create_users_table () in
  let* () = Conn.exec drop_old_users_trigger () in
  let* () = Conn.exec attach_users_trigger () in
  let* () = Conn.exec create_project_users_table () in
  let* () = Conn.exec create_strains_table () in
  let* () = Conn.exec drop_old_strains_trigger () in
  let* () = Conn.exec attach_strains_trigger () in
  let* () = Conn.exec create_samples_table () in
  let* () = Conn.exec drop_old_samples_trigger () in
  let* () = Conn.exec attach_samples_trigger () in
  let* () = Conn.exec create_products_table () in
  let* () = Conn.exec create_plates_table () in
  let* () = Conn.exec drop_old_plates_trigger () in
  let* () = Conn.exec attach_plates_trigger () in
  let* () = Conn.exec create_wells_table () in
  let* () = Conn.exec create_result_categories_table () in
  let* () = Conn.exec create_result_definitions_table () in
  let* () = Conn.exec create_result_values_table () in
  let* () = Conn.exec drop_old_result_values_trigger () in
  let* () = Conn.exec attach_result_values_trigger () in
  let* () = Conn.exec create_external_db_definitions_table () in
  let* () = Conn.exec drop_old_external_db_definitions_trigger () in
  let* () = Conn.exec attach_external_db_definitions_trigger () in
  let* () = Conn.exec create_strain_external_links_table () in
  Lwt_list.fold_left_s
    (fun acc query ->
      match acc with
      | Error e -> Lwt.return (Error e)
      | Ok () -> Conn.exec query ())
    (Ok ()) create_indices

(* ========================================================================== *)
(* MIGRATION V2: STATUS CONSTRAINTS                                           *)
(* ========================================================================== *)

let alter_projects_status =
  (unit ->. unit)
    "ALTER TABLE projects ADD CONSTRAINT projects_status_check CHECK (status \
     IN ('Pending', 'Active', 'Completed', 'Archived'))"

let alter_samples_status =
  (unit ->. unit)
    "ALTER TABLE samples ADD CONSTRAINT samples_status_check CHECK (status IN \
     ('Pending', 'Active', 'Completed', 'Archived'))"

let m002_add_status_constraints (module Conn : Caqti_lwt.CONNECTION) =
  let* () = Conn.exec alter_projects_status () in
  Conn.exec alter_samples_status ()

(* ========================================================================== *)
(* MIGRATION V3: PROJECT METADATA                                             *)
(* ========================================================================== *)

let alter_projects_add_metadata =
  (unit ->. unit) "ALTER TABLE projects ADD COLUMN metadata JSONB DEFAULT NULL"

let m003_add_project_metadata (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec alter_projects_add_metadata ()

(* ========================================================================== *)
(* MIGRATION V4: SYSTEM SETTINGS                                              *)
(* ========================================================================== *)

let create_system_settings_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS system_settings (
         id SERIAL PRIMARY KEY,
         key TEXT NOT NULL UNIQUE,
         value JSONB NOT NULL,
         description TEXT,
         updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
     );
    |}

let attach_system_settings_trigger =
  (unit ->. unit)
    {|
      CREATE TRIGGER update_system_settings_updated_at
      BEFORE UPDATE ON system_settings
      FOR EACH ROW
      EXECUTE PROCEDURE update_updated_at_column();
     |}

let seed_project_metadata_template =
  (unit ->. unit)
    "INSERT INTO system_settings (key, value, description) VALUES \
     ('project_metadata_template', '[]'::jsonb, 'Default metadata keys for new \
     projects') ON CONFLICT (key) DO NOTHING;"

let m004_system_settings (module Conn : Caqti_lwt.CONNECTION) =
  let* () = Conn.exec create_system_settings_table () in
  let* () = Conn.exec attach_system_settings_trigger () in
  Conn.exec seed_project_metadata_template ()

(* ========================================================================== *)
(* MIGRATION V5: COMMUNITIES                                                  *)
(* ========================================================================== *)

let create_communities_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS communities (
         id SERIAL PRIMARY KEY,
         uid TEXT NOT NULL UNIQUE,
         name TEXT NOT NULL,
         notes TEXT,
         metadata JSONB DEFAULT NULL,
         created_at REAL NOT NULL,
         updated_at REAL NOT NULL DEFAULT (extract(epoch from now()))
     );
    |}

let drop_old_communities_trigger =
  (unit ->. unit)
    "DROP TRIGGER IF EXISTS update_communities_updated_at ON communities;"

let attach_communities_trigger =
  (unit ->. unit)
    {|
     CREATE TRIGGER update_communities_updated_at
     BEFORE UPDATE ON communities
     FOR EACH ROW
     EXECUTE PROCEDURE update_updated_at_column();
    |}

let create_community_members_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS community_members (
         id SERIAL PRIMARY KEY,
         uid TEXT NOT NULL UNIQUE,
         community_id INT NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
         strain_id INT REFERENCES strains(id) ON DELETE SET NULL,
         label TEXT NOT NULL,
         taxon_path TEXT,
         created_at REAL NOT NULL
     );
    |}

let create_community_external_links_table =
  (unit ->. unit)
    {|
     CREATE TABLE IF NOT EXISTS community_external_links (
         id SERIAL PRIMARY KEY,
         uid TEXT NOT NULL UNIQUE,
         community_id INT NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
         external_db_definition_id INT NOT NULL REFERENCES external_db_definitions(id) ON DELETE CASCADE,
         value TEXT NOT NULL,
         created_at REAL NOT NULL
     );
    |}

let alter_samples_add_community_id =
  (unit ->. unit)
    "ALTER TABLE samples ADD COLUMN community_id INT REFERENCES \
     communities(id) ON DELETE SET NULL"

let create_community_indices =
  List.map
    (fun (name, table, column) ->
      (unit ->. unit)
        (Printf.sprintf "CREATE INDEX IF NOT EXISTS %s ON %s (%s)" name table
           column))
    [
      ("idx_samples_community_id", "samples", "community_id");
      ("idx_community_members_community_id", "community_members", "community_id");
      ("idx_community_members_strain_id", "community_members", "strain_id");
      ( "idx_community_external_links_community_id",
        "community_external_links",
        "community_id" );
    ]

let m005_communities (module Conn : Caqti_lwt.CONNECTION) =
  let* () = Conn.exec create_communities_table () in
  let* () = Conn.exec drop_old_communities_trigger () in
  let* () = Conn.exec attach_communities_trigger () in
  let* () = Conn.exec create_community_members_table () in
  let* () = Conn.exec create_community_external_links_table () in
  let* () = Conn.exec alter_samples_add_community_id () in
  Lwt_list.fold_left_s
    (fun acc query ->
      match acc with
      | Error e -> Lwt.return (Error e)
      | Ok () -> Conn.exec query ())
    (Ok ()) create_community_indices

(* ========================================================================== *)
(* MIGRATION V6: COMMUNITY METADATA TEMPLATE                                  *)
(* ========================================================================== *)

let seed_community_metadata_template =
  (unit ->. unit)
    "INSERT INTO system_settings (key, value, description) VALUES \
     ('community_metadata_template', '[]'::jsonb, 'Default metadata keys for \
     new communities') ON CONFLICT (key) DO NOTHING;"

let m006_community_metadata_template (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec seed_community_metadata_template ()

(* ========================================================================== *)
(* MIGRATION V7: SPLIT STRAIN SPECIES                                         *)
(* ========================================================================== *)

let m007_do_block =
  (unit ->. unit)
    {|
      DO $$
      BEGIN
        IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name='strains' AND column_name='species_name') THEN
            ALTER TABLE strains ADD COLUMN IF NOT EXISTS genus TEXT;
            ALTER TABLE strains ADD COLUMN IF NOT EXISTS species TEXT;
            UPDATE strains SET genus = split_part(species_name, ' ', 1), species = COALESCE(NULLIF(substring(species_name from position(' ' in species_name) + 1), ''), 'unknown');
            ALTER TABLE strains ALTER COLUMN genus SET NOT NULL;
            ALTER TABLE strains ALTER COLUMN species SET NOT NULL;
            ALTER TABLE strains DROP COLUMN species_name;
        END IF;

        IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name='community_members' AND column_name='taxon_path') THEN
            ALTER TABLE community_members RENAME COLUMN taxon_path TO taxon;
        END IF;
      END $$;
    |}

let m007_split_strain_species (module Conn : Caqti_lwt.CONNECTION) =
  Conn.exec m007_do_block ()

(* ========================================================================== *)
(* MIGRATION REGISTRY AND RUNNER                                              *)
(* ========================================================================== *)

let migrations =
  [
    { version = 1; name = "initial_schema"; up = m001_initial_schema };
    {
      version = 2;
      name = "add_status_constraints";
      up = m002_add_status_constraints;
    };
    {
      version = 3;
      name = "add_project_metadata";
      up = m003_add_project_metadata;
    };
    { version = 4; name = "system_settings"; up = m004_system_settings };
    { version = 5; name = "communities"; up = m005_communities };
    {
      version = 6;
      name = "community_metadata_template";
      up = m006_community_metadata_template;
    };
    {
      version = 7;
      name = "split_strain_species";
      up = m007_split_strain_species;
    };
  ]

(** [run ()] executes all pending database schema migrations. *)
let run () =
  let open Lwt.Syntax in
  let* () = Lwt_io.printl "Checking database schema..." in

  let runner (module Conn : Caqti_lwt.CONNECTION) =
    let open Lwt_result.Syntax in
    (* 1. Ensure the schema_migrations table exists *)
    let* () = Conn.exec create_schema_migrations_table () in

    (* 2. Fetch already applied versions *)
    let* applied_versions = get_applied_versions (module Conn) in

    (* 3. Run pending migrations in order *)
    Lwt_list.fold_left_s
      (fun acc migration ->
        match acc with
        | Error e -> Lwt.return (Error e)
        | Ok () ->
            if not (List.mem migration.version applied_versions) then
              let* _ =
                Lwt_io.printf "Applying migration %03d: %s...\n"
                  migration.version migration.name
                |> Lwt_result.ok
              in

              (* Run each migration's UP step in order. *)
              let* () = migration.up (module Conn) in

              (* And record that it was applied *)
              let* () =
                Conn.exec insert_migration_version_query migration.version
              in
              Lwt.return (Ok ())
            else Lwt.return (Ok ()))
      (Ok ()) migrations
  in

  let* result = Db.transaction runner in

  match result with
  | Ok () ->
      let* () = Lwt_io.printl "Database schema is up to date." in
      Lwt.return_unit
  | Error err ->
      let* () =
        Lwt_io.eprintf "FATAL: Database migration failed: %s\n"
          (Caqti_error.show err)
      in
      exit 1
