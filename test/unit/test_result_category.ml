open Alcotest
open Exlab_core.Types
open Test_helpers

let result_category_t = testable pp_result_category equal_result_category

let test_result_category_roundtrip () =
  let original =
    {
      id = 1;
      uid = "cat_uid1";
      name = "Test Category";
      description = Some "A test category";
    }
  in
  check_roundtrip result_category_t yojson_of_result_category
    result_category_of_yojson "Result Category Roundtrip" original

let suite =
  [
    ( "Result Category",
      [ test_case "Roundtrip" `Quick test_result_category_roundtrip ] );
  ]
