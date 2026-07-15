(** * This module defines the API routes for managing Products. * Products can
    represent labware, reagents, or any other physical items * used in the
    laboratory that need to be tracked. * * This module handles the HTTP layer,
    translating requests and responses * between the client and the underlying
    storage and core logic layers. It * uses helper functions from the [Utils]
    module for common tasks like * parameter parsing and response serialization.
    * * Key functionalities include: * - Listing all products, with support for
    CSV export. * - Creating a new product. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

let create_products_batch (items : Api_types.Product.create list) =
  let storage_items =
    List.map
      (fun (item : Api_types.Product.create) ->
        {
          Storage.Product.name = item.name;
          brand = item.brand;
          manufacturer_part_number = item.manufacturer_part_number;
          description = item.description;
        })
      items
  in
  Storage.Product.create_many storage_items

(** Handles `GET /api/v1/products`.

    Retrieves a list of all products.

    @return
      Returns a JSON response containing a list of all products and a total
      count. *)
let get_all_products_handler request =
  let search_term_opt = Dream.query request "search" in
  let result =
    let* products =
      match search_term_opt with
      | Some term when String.length term > 0 -> Storage.Product.search term
      | _ -> Storage.Product.get_all ()
    in
    let response : Api_types.Product.list_response =
      Api_types.Product.{ data = products; count = List.length products }
    in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response
    ~serializer:Api_types.Product.yojson_of_list_response result

let export_products_handler _request =
  let result =
    let* products = Storage.Product.get_all () in
    let csv_content = Core.Export_data.products_to_csv products in
    let filename = "products.csv" in

    Dream.respond
      ~headers:
        [
          ("Content-Type", "text/csv");
          ( "Content-Disposition",
            Printf.sprintf "attachment; filename=\"%s\"" filename );
        ]
      csv_content
    |> Lwt.return_ok
  in
  match%lwt result with
  | Ok response -> response
  | Error err -> Api_utils.respond_with_error err

(** Handles `GET /api/v1/products/:identifier`.

    Retrieves a specific product by its ID, UUID, or Short ID.

    @param identifier The ID, UUID, or Short ID of the product.
    @return A JSON response containing the detailed information of the product.
*)
let get_product_handler request =
  let result =
    let identifier = Dream.param request "identifier" in
    Api_utils.find_product_by_identifier identifier
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_product result

(** Handles `POST /api/v1/products`.

    Creates a new product. The request body should contain the product's name
    and other optional details like brand and manufacturer part number.

    - **Body**: (json) The product creation payload, defined by
      [Api_types.Product.create].

    @return
      A JSON response with the newly created product and a [201 Created] status.
*)
let create_product_handler request =
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.Product.create_of_yojson request
    in

    match
      Core.Product.validate_creation ~name:req.name
        ~manufacturer_part_number:req.manufacturer_part_number
    with
    | Error msg -> Lwt.return (Error (`Bad_Request msg))
    | Ok (valid_name, valid_pn) ->
        Storage.Product.add ~name:valid_name ~brand:req.brand
          ~manufacturer_part_number:valid_pn ~description:req.description
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_product result

(** Handles `PUT /api/v1/products/:identifier`.

    Updates an existing product.

    @param identifier
      The identifier of the product.
      - **Body**: (json) The product update payload, defined by
        [Api_types.Product.update].
    @return A JSON response with the updated product. *)
let update_product_handler request =
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.Product.update_of_yojson request
    in
    let identifier = Dream.param request "identifier" in
    let* product = Api_utils.find_product_by_identifier identifier in
    match
      Core.Product.validate_creation ~name:req.name
        ~manufacturer_part_number:req.manufacturer_part_number
    with
    | Error msg -> Lwt.return (Error (`Bad_Request msg))
    | Ok (valid_name, valid_pn) ->
        Storage.Product.update ~id:product.id ~name:valid_name ~brand:req.brand
          ~manufacturer_part_number:valid_pn ~description:req.description
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_product result

let bulk_create_products_handler request =
  let result =
    let* req =
      Api_utils.parse_body_json Api_types.Product.bulk_create_request_of_yojson
        request
    in
    create_products_batch req.products
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:(fun list ->
      `Assoc
        [ ("products", `List (List.map Core.Types.yojson_of_product list)) ])
    result

let bulk_csv_create_products_handler request =
  let result =
    let* create_items =
      Api_utils.parse_body_csv Decoders.product_create request
    in
    create_products_batch create_items
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:(fun list ->
      `Assoc
        [ ("products", `List (List.map Core.Types.yojson_of_product list)) ])
    result

let routes =
  [
    Dream.get "/api/v1/products" get_all_products_handler;
    Dream.get "/api/v1/products/export" export_products_handler;
    Dream.get "/api/v1/products/:identifier" get_product_handler;
    Dream.post "/api/v1/products" create_product_handler;
    Dream.put "/api/v1/products/:identifier" update_product_handler;
    Dream.post "/api/v1/products/bulk" bulk_create_products_handler;
    Dream.post "/api/v1/products/bulk-csv" bulk_csv_create_products_handler;
  ]
