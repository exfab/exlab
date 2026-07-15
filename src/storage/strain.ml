(** Handling for the Strain data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

type create_strain = {
  genus : string;
  species : string;
  strain_name : string;
  genotype : string option;
  parent_strain_id : int option;
  notes : string option;
}
(** Data required to create a new strain. *)

let select_fields =
  "id, uid, genus, species, strain_name, genotype, parent_strain_id, notes, \
   created_at, updated_at"

let strain_t =
  let encode (s : Exlab_core.Types.strain) =
    Ok
      ( s.id,
        s.uid,
        s.genus,
        s.species,
        s.strain_name,
        s.genotype,
        s.parent_strain_id,
        s.notes,
        s.created_at,
        s.updated_at )
  in
  let decode
      ( id,
        uid,
        genus,
        species,
        strain_name,
        genotype,
        parent_strain_id,
        notes,
        created_at,
        updated_at ) =
    Ok
      ({
         id;
         uid;
         genus;
         species;
         strain_name;
         genotype;
         parent_strain_id;
         notes;
         external_links = None;
         created_at;
         updated_at;
       }
        : Exlab_core.Types.strain)
  in
  let rep =
    t10 int string string string string (option string) (option int)
      (option string) float float
  in
  Caqti_type.custom ~encode ~decode rep

let strain_link_t =
  let encode (l : Exlab_core.Types.strain_external_link) =
    Ok
      ( l.id,
        l.uid,
        l.strain_id,
        l.external_db_definition_id,
        l.value,
        l.created_at )
  in
  let decode (id, uid, strain_id, external_db_definition_id, value, created_at)
      =
    Ok
      ({ id; uid; strain_id; external_db_definition_id; value; created_at }
        : Exlab_core.Types.strain_external_link)
  in
  Caqti_type.custom ~encode ~decode (t6 int string int int string float)

(* SQL Queries *)
let get_external_links_by_strain_id_query =
  (int ->* strain_link_t)
    "SELECT id, uid, strain_id, external_db_definition_id, value, created_at\n\
    \     FROM strain_external_links\n\
    \     WHERE strain_id = ?"

let get_all_query =
  (unit ->* strain_t) (Printf.sprintf "SELECT %s FROM strains" select_fields)

type strain_filters = {
  filter_term : string;
  filter_genus : string option;
  filter_species : string option;
}

let filters_t =
  let encode f =
    Ok
      ( (f.filter_term, f.filter_term, f.filter_term),
        (f.filter_genus, f.filter_genus, f.filter_species, f.filter_species) )
  in
  let decode _ = Error "Decode not implemented for filters" in
  Caqti_type.custom ~encode ~decode
    Caqti_type.Std.(
      t2 (t3 string string string)
        (t4 (option string) (option string) (option string) (option string)))

let search_with_filters_query =
  (filters_t ->* strain_t)
    (Printf.sprintf
       "SELECT %s FROM strains WHERE (strain_name ILIKE '%%' || ? || '%%' OR \
        genus ILIKE '%%' || ? || '%%' OR species ILIKE '%%' || ? || '%%') AND \
        (?::text IS NULL OR genus = ?) AND (?::text IS NULL OR species = ?) \
        ORDER BY created_at DESC LIMIT 200"
       select_fields)

let search_query =
  (t3 string string string ->* strain_t)
    (Printf.sprintf
       "SELECT %s FROM strains WHERE strain_name ILIKE '%%' || ? || '%%' OR \
        genus ILIKE '%%' || ? || '%%' OR species ILIKE '%%' || ? || '%%'LIMIT \
        50"
       select_fields)

let add_query =
  (t8 string string string string (option string) (option int) (option string)
     float
  ->! strain_t)
    (Printf.sprintf
       "INSERT INTO strains (uid, genus, species, strain_name, genotype, \
        parent_strain_id, notes, created_at)\n\
       \        VALUES (?, ?, ?, ?, ?, ?, ?, ?)\n\
       \        RETURNING %s"
       select_fields)

let get_by_id_query =
  (int ->? strain_t)
    (Printf.sprintf "SELECT %s FROM strains WHERE id = ?" select_fields)

let find_exact_query =
  (Caqti_type.t4 string string string (option string) ->? strain_t)
    (Printf.sprintf
       "SELECT %s FROM strains WHERE LOWER(genus) = LOWER(?) AND LOWER(species) = LOWER(?) AND LOWER(strain_name) = LOWER(?) AND LOWER(genotype) IS NOT DISTINCT FROM LOWER(?)"
       select_fields)

let get_by_uid_query =
  (string ->? strain_t)
    (Printf.sprintf "SELECT %s FROM strains WHERE uid = ?" select_fields)

let update_query =
  (t7 string string string (option string) (option int) (option string) int
  ->! strain_t)
    (Printf.sprintf
       "UPDATE strains\n\
       \        SET genus = ?, species = ?, strain_name = ?, genotype = ?, \
        parent_strain_id = ?, notes = ?\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

(* OCaml functions *)

(** [get_links conn strain_id] retrieves external links for a strain. *)
let get_links (module Conn : Caqti_lwt.CONNECTION) strain_id =
  let open Lwt_result.Syntax in
  let* rows =
    Conn.collect_list get_external_links_by_strain_id_query strain_id
  in
  Lwt_result.return rows

(** [attach_links conn strain] fetches and attaches external links to a strain.
*)
let attach_links (module Conn : Caqti_lwt.CONNECTION) (strain : strain) =
  let%lwt links_res = get_links (module Conn) strain.id in
  match links_res with
  | Ok links -> Lwt.return { strain with external_links = Some links }
  | Error _ -> Lwt.return strain
(* If links fail, return strain without them (graceful degradation) *)

(** [get_all ()] retrieves all strains. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      let%lwt strains = Lwt_list.map_s (attach_links (module Conn)) rows in
      Lwt.return (Ok strains))

(** [search_with_filters filters] searches for strains matching a string and
    optional genus/species filters. *)
let search_with_filters filters =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list search_with_filters_query filters in
      let%lwt strains = Lwt_list.map_s (attach_links (module Conn)) rows in
      Lwt.return (Ok strains))

(** [search term] searches for strains matching a string in name, genus, or
    species. *)
let search term =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list search_query (term, term, term) in
      let%lwt strains = Lwt_list.map_s (attach_links (module Conn)) rows in
      Lwt.return (Ok strains))

(** [get_by_id id] retrieves a strain by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row_opt = Conn.find_opt get_by_id_query id in
      match row_opt with
      | Some row ->
          let%lwt strain = attach_links (module Conn) row in
          Lwt.return (Ok (Some strain))
      | None -> Lwt.return (Ok None))

let find_exact ~genus ~species ~strain_name ~genotype =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row_opt = Conn.find_opt find_exact_query (genus, species, strain_name, genotype) in
      match row_opt with
      | Some row ->
          let%lwt strain = attach_links (module Conn) row in
          Lwt.return (Ok (Some strain))
      | None -> Lwt.return (Ok None))

(** [get_by_uid uid] retrieves a strain by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row_opt = Conn.find_opt get_by_uid_query uid in
      match row_opt with
      | Some row ->
          let%lwt strain = attach_links (module Conn) row in
          Lwt.return (Ok (Some strain))
      | None -> Lwt.return (Ok None))

(** [insert conn ~genus ~species ~strain_name ~genotype ~parent_strain_id
     ~notes] inserts a new strain within a transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~genus ~species ~strain_name
    ~genotype ~parent_strain_id ~notes =
  let uid = Utils.make_uuid () in
  let created_at = Unix.time () in

  let* row =
    Conn.find add_query
      ( uid,
        genus,
        species,
        strain_name,
        genotype,
        parent_strain_id,
        notes,
        created_at )
  in
  Lwt.return (Ok row)

(** [add ~genus ~species ~strain_name ~genotype ~parent_strain_id ~notes]
    creates a new strain. *)
let add ~genus ~species ~strain_name ~genotype ~parent_strain_id ~notes =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert
        (module Conn)
        ~genus ~species ~strain_name ~genotype ~parent_strain_id ~notes)

(** [modify conn ~id ~genus ~species ~strain_name ~genotype ~parent_strain_id
     ~notes] updates a strain within a transaction. *)
let modify (module Conn : Caqti_lwt.CONNECTION) ~id ~genus ~species ~strain_name
    ~genotype ~parent_strain_id ~notes =
  let* row =
    Conn.find update_query
      (genus, species, strain_name, genotype, parent_strain_id, notes, id)
  in
  let%lwt strain = attach_links (module Conn) row in
  Lwt.return (Ok strain)

(** [update ~id ~genus ~species ~strain_name ~genotype ~parent_strain_id ~notes]
    updates an existing strain. *)
let update ~id ~genus ~species ~strain_name ~genotype ~parent_strain_id ~notes =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify
        (module Conn)
        ~id ~genus ~species ~strain_name ~genotype ~parent_strain_id ~notes)

(** [create_many items] creates multiple strains in a single transaction. *)
let create_many (items : create_strain list) =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* created_strains =
        Lwt_list.fold_left_s
          (fun acc item ->
            match acc with
            | Error _ as e -> Lwt.return e
            | Ok strains ->
                let* new_strain =
                  insert
                    (module Conn)
                    ~genus:item.genus ~species:item.species
                    ~strain_name:item.strain_name ~genotype:item.genotype
                    ~parent_strain_id:item.parent_strain_id ~notes:item.notes
                in
                Lwt.return (Ok (new_strain :: strains)))
          (Ok []) items
      in
      Lwt.return (Ok (List.rev created_strains)))
