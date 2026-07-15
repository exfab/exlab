open Alcotest
open Exlab_core.Types
open Test_helpers

let product_t = testable pp_product equal_product

let make_dummy_product () : product =
  {
    id = 10;
    uid = "product123";
    short_id = "PRD-0001";
    name = "Test Product A";
    brand = Some "Brand";
    manufacturer_part_number = Some "PROD123";
    description = Some "Test Description";
    created_at = 0.0;
    updated_at = 0.0;
  }

let test_product_roundtrip () =
  check_roundtrip product_t yojson_of_product product_of_yojson
    "Product Roundtrip" (make_dummy_product ())

let test_validate_name () =
  match Exlab_core.Product.validate_name " Valid Name " with
  | Ok name -> check string "Trimes Whitespace" "Valid Name" name
  | Error e -> fail ("Unexpected Error: " ^ e)

let test_normalize_part_number () =
  let norm = Exlab_core.Product.normalize_part_number in
  check (option string) "Normalize SKU" (Some "ABC-123")
    (norm (Some " abc-123 "));
  check (option string) "Empty becomes None" None (norm (Some ""));
  check (option string) "Whitespace becomes None" None (norm (Some "  "));
  check (option string) "None stays None" None (norm None)

let test_validate_creation_success () =
  let res =
    Exlab_core.Product.validate_creation ~name:" Beaker"
      ~manufacturer_part_number:(Some " bk-50 ")
  in
  match res with
  | Ok (name, pn) ->
      check string "Name cleaned" "Beaker" name;
      check (option string) "Part Number cleaned" (Some "BK-50") pn
  | Error e -> fail e

let test_matches_requirement () =
  let p =
    { (make_dummy_product ()) with manufacturer_part_number = Some "ABC-123" }
  in

  let check_match req expected msg =
    let result =
      Exlab_core.Product.matches_requirement ~required_part_number:req p
    in
    Alcotest.(check bool) msg expected result
  in

  check_match "ABC-123" true "Exact match";
  check_match " abc-123 " true "Trimmed and case-insensitive match";
  check_match "ABC-456" false "Non-match";
  check_match "" false "Empty required part number";

  let p_no_pn = { p with manufacturer_part_number = None } in
  let res_no_pn =
    Exlab_core.Product.matches_requirement ~required_part_number:"ABC-123"
      p_no_pn
  in
  Alcotest.(check bool) "Product with no part number" false res_no_pn

let suite =
  [
    ("Product", [ test_case "Roundtrip" `Quick test_product_roundtrip ]);
    ( "Logic",
      [
        test_case "Validate Name" `Quick test_validate_name;
        test_case "Normalize SKU" `Quick test_normalize_part_number;
        test_case "Creation Logic" `Quick test_validate_creation_success;
        test_case "Matches Requirement" `Quick test_matches_requirement;
      ] );
  ]
