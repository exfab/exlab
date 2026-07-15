(** Handling for the Product data type. *)

open Exlab_core.Types
open Lwt_result.Syntax
open Caqti_request.Infix
open Caqti_type.Std

(* Helpers *)

let select_fields =
  "id, uid, short_id, name, brand, manufacturer_part_number, description, \
   created_at, updated_at"

let product_t =
  let encode (p : Exlab_core.Types.product) =
    Ok
      ( p.id,
        p.uid,
        p.short_id,
        p.name,
        p.brand,
        p.manufacturer_part_number,
        p.description,
        p.created_at,
        p.updated_at )
  in
  let decode
      ( id,
        uid,
        short_id,
        name,
        brand,
        manufacturer_part_number,
        description,
        created_at,
        updated_at ) =
    Ok
      ({
         id;
         uid;
         short_id;
         name;
         brand;
         manufacturer_part_number;
         description;
         created_at;
         updated_at;
       }
        : Exlab_core.Types.product)
  in
  let rep =
    t9 int string string string (option string) (option string) (option string)
      float float
  in
  Caqti_type.custom ~encode ~decode rep

(* SQL Queries *)

(* Get all products*)
let get_all_query =
  (unit ->* product_t) (Printf.sprintf "SELECT %s FROM products" select_fields)

let search_query =
  (t2 string string ->* product_t)
    (Printf.sprintf
       "SELECT %s FROM products WHERE name ILIKE '%%' || ? || '%%' OR brand \
        ILIKE '%%' || ? || '%%'"
       select_fields)

let get_by_id_query =
  (int ->? product_t)
    (Printf.sprintf "SELECT %s FROM products WHERE id = ?" select_fields)

let get_by_uid_query =
  (string ->? product_t)
    (Printf.sprintf "SELECT %s FROM products WHERE uid = ?" select_fields)

let get_by_short_id_query =
  (string ->? product_t)
    (Printf.sprintf "SELECT %s FROM products WHERE short_id = ?" select_fields)

(* Add a new product *)
let add_query =
  (t8 string string string (option string) (option string) (option string) float
     float
  ->! product_t)
    (Printf.sprintf
       "INSERT INTO products (uid, short_id, name, brand, \
        manufacturer_part_number, description, created_at, updated_at)\n\
       \        VALUES (?, ?, ?, ?, ?, ?, ?, ?)\n\
       \        RETURNING %s"
       select_fields)

let set_short_id_query =
  (t2 string int ->. unit) "UPDATE products SET short_id = ? WHERE id = ?"

let update_query =
  (t5 string (option string) (option string) (option string) int ->! product_t)
    (Printf.sprintf
       "UPDATE products\n\
       \        SET name = ?, brand = ?, manufacturer_part_number = ?, \
        description = ?, updated_at = extract(epoch from now())\n\
       \        WHERE id = ?\n\
       \        RETURNING %s"
       select_fields)

(* OCaml functions *)

(** [get_all ()] retrieves all products. *)
let get_all () =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list get_all_query () in
      Lwt_result.return rows)

(** [search term] retrieves products matching the term in name or brand. *)
let search term =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* rows = Conn.collect_list search_query (term, term) in
      Lwt_result.return rows)

(** [get_by_id id] retrieves a product by its internal ID. *)
let get_by_id id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_id_query id in
      Lwt_result.return row)

(** [get_by_uid uid] retrieves a product by its UID. *)
let get_by_uid uid =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_uid_query uid in
      Lwt_result.return row)

(** [get_by_short_id short_id] retrieves a product by its short ID. *)
let get_by_short_id short_id =
  Db.request (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* row = Conn.find_opt get_by_short_id_query short_id in
      Lwt_result.return row)

(** [insert conn ~name ~brand ~manufacturer_part_number ~description] inserts a
    new product within a transaction. *)
let insert (module Conn : Caqti_lwt.CONNECTION) ~name ~brand
    ~manufacturer_part_number ~description =
  let uid = Utils.make_uuid () in

  let temp_short_id = uid in
  let now = Unix.time () in

  let* product =
    Conn.find add_query
      ( uid,
        temp_short_id,
        name,
        brand,
        manufacturer_part_number,
        description,
        now,
        now )
  in

  let short_id = Exlab_core.Product.generate_short_id ~product_id:product.id in

  let* () = Conn.exec set_short_id_query (short_id, product.id) in

  let updated_product = { product with short_id } in
  Lwt.return (Ok updated_product)

(** [add ~name ~brand ~manufacturer_part_number ~description] creates a new
    product. *)
let add ~name ~brand ~manufacturer_part_number ~description =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      insert (module Conn) ~name ~brand ~manufacturer_part_number ~description)

(** [modify conn ~id ~name ~brand ~manufacturer_part_number ~description]
    updates a product within a transaction. *)
let modify (module Conn : Caqti_lwt.CONNECTION) ~id ~name ~brand
    ~manufacturer_part_number ~description =
  let* row =
    Conn.find update_query
      (name, brand, manufacturer_part_number, description, id)
  in
  Lwt.return (Ok row)

(** [update ~id ~name ~brand ~manufacturer_part_number ~description] updates an
    existing product. *)
let update ~id ~name ~brand ~manufacturer_part_number ~description =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      modify
        (module Conn)
        ~id ~name ~brand ~manufacturer_part_number ~description)

type create_product = {
  name : string;
  brand : string option;
  manufacturer_part_number : string option;
  description : string option;
}
(** Data required to create a new product. *)

(** [create_many items] creates multiple products in a single transaction. *)
let create_many (items : create_product list) =
  Db.transaction (fun (module Conn : Caqti_lwt.CONNECTION) ->
      let* created_products =
        Lwt_list.fold_left_s
          (fun acc item ->
            match acc with
            | Error _ as e -> Lwt.return e
            | Ok products ->
                let* new_product =
                  insert
                    (module Conn)
                    ~name:item.name ~brand:item.brand
                    ~manufacturer_part_number:item.manufacturer_part_number
                    ~description:item.description
                in
                Lwt.return (Ok (new_product :: products)))
          (Ok []) items
      in
      Lwt.return (Ok (List.rev created_products)))
