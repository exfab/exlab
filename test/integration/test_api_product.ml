(* Test Definitions for API endpoint /api/products *)

let handler = Test_utils.admin_app

let test_create_malformed_json _switch () =
  let body = {| {"name": "Broken JSON", |} in
  let req = Test_utils.json_post ~path:"/api/v1/products" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_name _switch () =
  let body = {| {"description": "This is a test product"} |} in
  let req = Test_utils.json_post ~path:"/api/v1/products" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing name" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let counter = ref 0

let get_unique_name prefix =
  counter := !counter + 1;
  let time = Unix.gettimeofday () in
  Printf.sprintf "%s-%f-%d" prefix time !counter

let test_product_lifecycle _switch () =
  let open Lwt.Syntax in
  let name = get_unique_name "Lifecycle Product" in
  let body = Printf.sprintf {| {"name": "%s", "brand": "Brand X"} |} name in
  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/products" ~body
      ~expected_status:201 "Create successful"
  in
  let* create_body = Dream.body create_res in
  let json = Yojson.Safe.from_string create_body in
  let product_id = Yojson.Safe.Util.(member "id" json |> to_int) in

  (* Fetch *)
  let path = Printf.sprintf "/api/v1/products/%d" product_id in
  let* _ =
    Test_utils.assert_json_get ~handler ~path ~expected_status:200
      "Fetch successful"
  in

  (* Update *)
  let update_body =
    Printf.sprintf {| {"name": "%s Updated", "brand": "Brand Y"} |} name
  in
  let* _ =
    Test_utils.assert_json_put ~handler ~path ~body:update_body
      ~expected_status:200 "Update successful"
  in

  (* Fetch All *)
  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/products"
      ~expected_status:200 "Fetch all successful"
  in

  (* Export *)
  let* export_res =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/products/export"
      ~expected_status:200 "Export successful"
  in
  let headers = Dream.all_headers export_res in
  let content_type_headers =
    List.filter_map
      (fun (k, v) ->
        if String.lowercase_ascii k = "content-type" then Some v else None)
      headers
  in
  Alcotest.(check bool)
    "Has CSV Content-Type" true
    (List.exists (fun h -> h = "text/csv") content_type_headers);
  Lwt.return ()

let test_update_invalid_name _switch () =
  let open Lwt.Syntax in
  let name = get_unique_name "Invalid Update Product" in
  let body = Printf.sprintf {| {"name": "%s"} |} name in
  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/products" ~body
      ~expected_status:201 "Create successful"
  in
  let* create_body = Dream.body create_res in
  let json = Yojson.Safe.from_string create_body in
  let product_id = Yojson.Safe.Util.(member "id" json |> to_int) in

  let path = Printf.sprintf "/api/v1/products/%d" product_id in
  let update_body = {| {"name": "   "} |} in
  let* _ =
    Test_utils.assert_json_put ~handler ~path ~body:update_body
      ~expected_status:400 "Update with empty name fails"
  in
  Lwt.return ()

let test_update_not_found _switch () =
  let open Lwt.Syntax in
  let update_body = {| {"name": "Does not exist"} |} in
  let* _ =
    Test_utils.assert_json_put ~handler ~path:"/api/v1/products/999999"
      ~body:update_body ~expected_status:404 "Update non-existent fails"
  in
  Lwt.return ()

let test_get_not_found _switch () =
  let open Lwt.Syntax in
  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/products/999999"
      ~expected_status:404 "Get non-existent fails"
  in
  Lwt.return ()

let test_bulk_create_json_success _switch () =
  let name_a = get_unique_name "Product A" in
  let name_b = get_unique_name "Product B" in
  let body =
    Printf.sprintf
      {|
              { "products": [
                  { "name": "%s", "brand": "Brand A" },
                  { "name": "%s", "brand": "Brand B" }
                ]
              }
              |}
      name_a name_b
  in
  let req = Test_utils.json_post ~path:"/api/v1/products/bulk" ~body in
  let res = Dream.test handler req in

  let%lwt body = Dream.body res in

  Alcotest.(check int)
    "Should return 201 Created" 201
    (Dream.status res |> Dream.status_to_int);

  let json = Yojson.Safe.from_string body in
  let products = Yojson.Safe.Util.(member "products" json |> to_list) in

  Alcotest.(check int) "Should create 2 products" 2 (List.length products);
  Lwt.return ()

let test_bulk_create_json_malformed _switch () =
  let body =
    {|
              { "products": [
                  { "name": "Product A", "brand": "Brand A" },
                  { "name": "Product B", 
                ]
              }
              |}
  in
  let req = Test_utils.json_post ~path:"/api/v1/products/bulk" ~body in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for malformed JSON" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_create_csv_success _switch () =
  let name_c = get_unique_name "Product C" in
  let name_d = get_unique_name "Product D" in
  let csv_body =
    Printf.sprintf "name,brand\n%s,Brand C\n%s,Brand D" name_c name_d
  in
  let req =
    Test_utils.csv_post ~path:"/api/v1/products/bulk-csv" ~body:csv_body
  in
  let res = Dream.test handler req in

  let%lwt body = Dream.body res in

  Alcotest.(check int)
    "Should return 201 Created" 201
    (Dream.status res |> Dream.status_to_int);

  let json = Yojson.Safe.from_string body in
  let products = Yojson.Safe.Util.(member "products" json |> to_list) in

  Alcotest.(check int) "Should create 2 products" 2 (List.length products);
  Lwt.return ()

let test_bulk_create_csv_malformed_data _switch () =
  let csv_body = "name,brand\n,Brand E\nProduct F,Brand F" in
  let req =
    Test_utils.csv_post ~path:"/api/v1/products/bulk-csv" ~body:csv_body
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for malformed CSV data" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_create_csv_missing_header _switch () =
  let csv_body = "brand\nBrand G" in
  let req =
    Test_utils.csv_post ~path:"/api/v1/products/bulk-csv" ~body:csv_body
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing header" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_search_products _switch () =
  let open Lwt.Syntax in
  let name_a = get_unique_name "SearchableProdA" in
  let name_b = get_unique_name "UnrelatedProdB" in

  let body_a =
    Printf.sprintf {| {"name": "%s", "brand": "SearchBrand"} |} name_a
  in
  let body_b =
    Printf.sprintf {| {"name": "%s", "brand": "OtherBrand"} |} name_b
  in

  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/products" ~body:body_a
      ~expected_status:201 "Create Product A"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/products" ~body:body_b
      ~expected_status:201 "Create Product B"
  in

  let search_path = Printf.sprintf "/api/v1/products?search=%s" name_a in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search products by name"
  in

  let* search_body = Dream.body search_res in
  let json = Yojson.Safe.from_string search_body in
  let data = Yojson.Safe.Util.(member "data" json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching product" 1 (List.length data);

  let returned_name =
    Yojson.Safe.Util.(List.hd data |> member "name" |> to_string)
  in
  Alcotest.(check string) "Returned product name matches" name_a returned_name;

  Lwt.return ()

let suite =
  [
    ( "Product API",
      [
        Alcotest_lwt.test_case "Product Lifecycle (Create, Get, Update, List)"
          `Quick test_product_lifecycle;
        Alcotest_lwt.test_case "Search Products" `Quick test_search_products;
        Alcotest_lwt.test_case "Reject Update Invalid Name" `Quick
          test_update_invalid_name;
        Alcotest_lwt.test_case "Reject Update Not Found" `Quick
          test_update_not_found;
        Alcotest_lwt.test_case "Reject Get Not Found" `Quick test_get_not_found;
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "Reject Missing Name" `Quick
          test_create_missing_name;
        Alcotest_lwt.test_case "Bulk JSON: Success" `Quick
          test_bulk_create_json_success;
        Alcotest_lwt.test_case "Bulk JSON: Malformed" `Quick
          test_bulk_create_json_malformed;
        Alcotest_lwt.test_case "Bulk CSV: Success" `Quick
          test_bulk_create_csv_success;
        Alcotest_lwt.test_case "Bulk CSV: Malformed Data" `Quick
          test_bulk_create_csv_malformed_data;
        Alcotest_lwt.test_case "Bulk CSV: Missing Header" `Quick
          test_bulk_create_csv_missing_header;
      ] );
  ]
