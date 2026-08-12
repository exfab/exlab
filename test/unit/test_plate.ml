open Alcotest
open Exlab_core.Types

let plate_t = testable pp_plate equal_plate
let well_t = testable pp_well equal_well

let make_dummy_plate () : plate =
  {
    id = 10;
    uid = "plate123";
    short_id = "PLT-0001";
    name = "Test Plate A";
    project_id = 1;
    product_id = Some 99;
    plate_format = Well_96;
    created_at = 1000.0;
    updated_at = 2000.0;
  }

let test_plate_roundtrip () =
  Test_helpers.check_roundtrip plate_t yojson_of_plate plate_of_yojson
    "Plate Roundtrip" (make_dummy_plate ())

let test_well_roundtrip () =
  let w = { id = 1; plate_id = 50; sample_id = Some 10; coordinate = "A1" } in
  Test_helpers.check_roundtrip well_t yojson_of_well well_of_yojson
    "Well Roundtrip" w

let test_plate_format_unknown () =
  match plate_format_of_string "1000-well" with
  | Ok _ -> fail "Should have returned Error"
  | Error msg ->
      check string "Error message correct" "Unknown plate format: 1000-well" msg

let test_generate_well_coordinates () =
  let check_count format expected_count =
    let coords = Exlab_core.Plate.generate_well_coordinates format in
    Alcotest.(check int)
      ("Correct number of wells for " ^ string_of_plate_format format)
      expected_count (List.length coords)
  in
  check_count Well_24 24;
  check_count Well_48 48;
  check_count Well_96 96;
  check_count Well_384 384

let test_validate_mixture_success () =
  let existing = [ Source; Source ] in
  let new_cat = Source in

  match
    Exlab_core.Plate.validate_sample_mixture ~existing_categories:existing
      ~new_category:new_cat
  with
  | Ok () -> check bool "Same category is allowed" true true
  | Error msg -> fail ("Unexpected error: " ^ msg)

let test_validate_mixture_conflict () =
  let existing = [ Source ] in
  let new_cat = Experimental in

  match
    Exlab_core.Plate.validate_sample_mixture ~existing_categories:existing
      ~new_category:new_cat
  with
  | Ok () -> fail "Should have failed mixing Source and Experimental"
  | Error msg ->
      check string "Error message is correct"
        "Compliance Violation: Source and Experimental samples cannot be mixed \
         on the same plate."
        msg

let test_validate_mixture_empty () =
  let existing = [] in
  let new_cat = Experimental in

  match
    Exlab_core.Plate.validate_sample_mixture ~existing_categories:existing
      ~new_category:new_cat
  with
  | Ok () -> check bool "Empty plate accepts any category" true true
  | Error msg -> fail ("Unexpected error on empty plate: " ^ msg)

let test_validate_name_valid () =
  let result = Exlab_core.Plate.validate_name "Valid Name" in
  Alcotest.(check (result string string))
    "Valid plate name" (Ok "Valid Name") result

let test_validate_name_invalid () =
  let result = Exlab_core.Plate.validate_name "  " in
  Alcotest.(check (result string string))
    "Invalid plate name" (Error "Plate name cannot be empty") result

let test_coordinate_of_index () =
  (* Helper to check a specific index -> coord mapping *)
  let check_case format idx expected =
    let result = Exlab_core.Plate.coordinate_of_index format idx in
    let fmt_str = Exlab_core.Types.string_of_plate_format format in
    Alcotest.(check (result string string))
      (Printf.sprintf "%s: Index %d -> %s" fmt_str idx expected)
      (Ok expected) result
  in

  let check_fail format idx =
    let result = Exlab_core.Plate.coordinate_of_index format idx in
    match result with
    | Ok s ->
        Alcotest.fail
          (Printf.sprintf "Should have failed for index %d, but got %s" idx s)
    | Error _ -> Alcotest.(check pass) "Correctly failed" true true
  in

  (* 24-Well Plate (4 Rows [A-D] x 6 Cols) *)
  check_case Well_24 1 "A1";
  check_case Well_24 4 "D1";
  check_case Well_24 5 "A2";
  check_case Well_24 24 "D6";
  check_fail Well_24 25;

  (* 48-Well Plate (6 Rows [A-F] x 8 Cols) *)
  check_case Well_48 1 "A1";
  check_case Well_48 6 "F1";
  check_case Well_48 7 "A2";
  check_case Well_48 48 "F8";
  check_fail Well_48 49;

  (* 96-Well Plate (8 Rows [A-H] x 12 Cols) *)
  check_case Well_96 1 "A1";
  check_case Well_96 8 "H1";
  check_case Well_96 9 "A2";
  check_case Well_96 12 "D2";
  check_case Well_96 96 "H12";
  check_fail Well_96 97;

  (* 384-Well Plate (16 Rows [A-P] x 24 Cols)*)
  check_case Well_384 1 "A1";
  check_case Well_384 16 "P1";
  check_case Well_384 17 "A2";
  check_case Well_384 384 "P24";
  check_fail Well_384 385;

  let res_low = Exlab_core.Plate.coordinate_of_index Well_96 0 in
  Alcotest.(check (result string string))
    "Index 0 should fail with specific message"
    (Error "Index must be a positive integer.") res_low;

  check_fail Well_96 (-1)

(* let test_coordinate_conversion_bounds () = *)
(*   let check_fail format idx = *)
(*     let result = Exlab_core.Plate.coordinate_of_index format idx in *)
(*     match result with *)
(*     | Ok s -> Alcotest.fail (Printf.sprintf "Should have failed for %s index %d, but got %s" (string_of_plate_format format) idx s) *)
(*     | Error _ -> Alcotest.(check pass) "Correctly failed" true true *)
(*   in *)

(*   (\* 24-Well (Max 24) *\) *)

(*   (\* 48-Well (Max 48) *\) *)

(*   (\* 96-Well (Max 96) *\) *)

(*   (\* 384-Well (Max 384) *\) *)

(*   let res_low = Exlab_core.Plate.coordinate_of_index Well_96 0 in *)
(*   Alcotest.(check (result string string)) "Index 0 should fail" (Error "Index must be a positive integer.") res_low *)

let test_index_of_coordinate () =
  let check_case format coord expected_idx =
    let result = Exlab_core.Plate.index_of_coordinate format coord in
    let fmt_str = Exlab_core.Types.string_of_plate_format format in
    Alcotest.(check (result int string))
      (Printf.sprintf "[%s] %s -> Index %d" fmt_str coord expected_idx)
      (Ok expected_idx) result
  in

  let check_fail format coord =
    match Exlab_core.Plate.index_of_coordinate format coord with
    | Ok i ->
        Alcotest.fail (Printf.sprintf "Should fail for %s but got %d" coord i)
    | Error _ -> Alcotest.(check pass) "Correctly failed" true true
  in

  (* 24-Well Plate (4 Rows [A-D] x 6 Cols) *)
  check_case Well_24 "A1" 1;
  check_case Well_24 "D1" 4;
  check_case Well_24 "A2" 5;
  check_case Well_24 "D6" 24;
  check_fail Well_24 "E1";
  check_fail Well_24 "A7";

  (* 48-Well Plate (6 Rows [A-F] x 8 Cols) *)
  check_case Well_48 "A1" 1;
  check_case Well_48 "F1" 6;
  check_case Well_48 "A2" 7;
  check_case Well_48 "F8" 48;
  check_fail Well_48 "G1";
  check_fail Well_48 "A9";

  (* 96-Well Plate (8 Rows [A-H] x 12 Cols) *)
  check_case Well_96 "A1" 1;
  check_case Well_96 "H1" 8;
  check_case Well_96 "A2" 9;
  check_case Well_96 "D2" 12;
  check_case Well_96 "H12" 96;
  check_fail Well_96 "I1";
  check_fail Well_96 "A13";

  (* 384-Well Plate (16 Rows [A-P] x 24 Cols) *)
  check_case Well_384 "A1" 1;
  check_case Well_384 "P1" 16;
  check_case Well_384 "A2" 17;
  check_case Well_384 "P24" 384;
  check_fail Well_384 "Q1";
  check_fail Well_384 "A25";

  (* General Invalid Inputs *)
  check_fail Well_96 "BadFormat";
  check_fail Well_96 ""

let test_coordinate_index_roundtrip () =
  let formats = [ Exlab_core.Types.Well_24; Well_48; Well_96; Well_384 ] in

  List.iter
    (fun format ->
      let rows, cols = Exlab_core.Plate.get_plate_dimensions format in
      let max_wells = rows * cols in
      let fmt_name = Exlab_core.Types.string_of_plate_format format in

      (* Loop through EVERY valid index for this plate type *)
      for i = 1 to max_wells do
        match Exlab_core.Plate.coordinate_of_index format i with
        | Error e ->
            Alcotest.fail
              (Printf.sprintf "[%s] Forward conversion failed for index %d: %s"
                 fmt_name i e)
        | Ok coord -> (
            match Exlab_core.Plate.index_of_coordinate format coord with
            | Error e ->
                Alcotest.fail
                  (Printf.sprintf
                     "[%s] Reverse conversion failed for coord %s: %s" fmt_name
                     coord e)
            | Ok final_index ->
                if i <> final_index then
                  Alcotest.fail
                    (Printf.sprintf
                       "[%s] Roundtrip mismatch! Started: %d -> Coord: %s -> \
                        Ended: %d"
                       fmt_name i coord final_index))
      done)
    formats;

  (* Reaching this point means every loop iteration completed without error. *)
  Alcotest.(check pass)
    "All formats round-tripped successfully over full range" true true

let test_normalize_coordinate () =
  let format = Well_96 in

  (* Numeric Input Checks *)
  Alcotest.(check (result string string))
    "Numeric 1 -> A1" (Ok "A1")
    (Exlab_core.Plate.normalize_coordinate format "1");
  Alcotest.(check (result string string))
    "Numeric 14 -> F2" (Ok "F2")
    (Exlab_core.Plate.normalize_coordinate format "14");

  (* Numeric Bounds Check *)
  let res_num_err = Exlab_core.Plate.normalize_coordinate format "140" in
  Alcotest.(check (result string string))
    "Numeric 140 -> Error"
    (Error "Index 140 is out of bounds for this plate format (max 96)")
    res_num_err;

  (* Alphanumeric Input Checks *)
  Alcotest.(check (result string string))
    "Alpha A1 -> A1" (Ok "A1")
    (Exlab_core.Plate.normalize_coordinate format "A1");

  (* Alphanumeric Bounds Check (Row) *)
  let res_row_err = Exlab_core.Plate.normalize_coordinate format "I1" in
  (* 96 well only goes to H *)
  Alcotest.(check (result string string))
    "Row I -> Error" (Error "Well I1 is out of bounds") res_row_err;

  (* Alphanumeric Bounds Check (Col) *)
  let res_col_err = Exlab_core.Plate.normalize_coordinate format "A13" in
  (* 96 well only goes to 12 *)
  Alcotest.(check (result string string))
    "Col 13 -> Error" (Error "Well A13 is out of bounds") res_col_err

let test_compare_coordinates_column_major () =
  let compare = Exlab_core.Plate.compare_coordinates in

  Alcotest.(check int) "A1 < B1" (-1) (compare "A1" "B1");
  Alcotest.(check int) "B1 > A1" 1 (compare "B1" "A1");

  Alcotest.(check int) "H1 < A2" (-1) (compare "H1" "A2");
  Alcotest.(check int) "A2 > H1" 1 (compare "A2" "H1");

  Alcotest.(check int) "A2 < B2" (-1) (compare "A2" "B2");
  Alcotest.(check int) "B2 > A2" 1 (compare "B2" "A2");

  Alcotest.(check int) "B2 < B10" (-1) (compare "B2" "B10");
  Alcotest.(check int) "B10 > B2" 1 (compare "B10" "B2");

  Alcotest.(check int) "C2 = C2" 0 (compare "C2" "C2");

  Alcotest.(check int) "Valid < Invalid" (-1) (compare "A1" "BadData");
  Alcotest.(check int) "Invalid > Valid" 1 (compare "BadData" "A1");
  Alcotest.(check int) "Invalid = Invalid" 0 (compare "BadData" "BadData");
  Alcotest.(check int) "Empty string comparison" (-1) (compare "A1" "");
  Alcotest.(check int) "Empty string comparison" 1 (compare "" "A1")

let test_is_edge_well () =
  let check format coord expected msg =
    let result = Exlab_core.Plate.is_edge_well format coord in
    let fmt_str = Exlab_core.Types.string_of_plate_format format in
    Alcotest.(check bool)
      (Printf.sprintf "[%s] %s (%s)" fmt_str msg coord)
      expected result
  in

  (* 24-Well Plate (4 Rows [A-D] x 6 Cols) *)
  check Well_24 "A1" true "Top-Left Corner";
  check Well_24 "D6" true "Bottom-Right Corner";
  check Well_24 "B1" true "Left Edge";
  check Well_24 "A3" true "Top Edge";
  check Well_24 "B2" false "Interior Well";
  check Well_24 "C5" false "Interior Well";

  (* 48-Well Plate (6 Rows [A-F] x 8 Cols) *)
  check Well_48 "F8" true "Bottom-Right Corner";
  check Well_48 "F4" true "Bottom Edge (Row F)";
  check Well_48 "E7" false "Interior (One away from corner)";

  (* 96-Well Plate (8 Rows [A-H] x 12 Cols) *)
  check Well_96 "H12" true "Bottom-Right Corner";
  check Well_96 "D1" true "Left Edge";
  check Well_96 "G11" false "Interior (One away from corner)";

  (* 384-Well Plate (16 Rows [A-P] x 24 Cols) *)
  check Well_384 "P24" true "Bottom-Right Corner (Row 16, Col 24)";
  check Well_384 "A15" true "Top Edge";
  check Well_384 "H24" true "Right Edge";
  check Well_384 "O23" false "Interior (Row 15, Col 23)";
  check Well_384 "B2" false "Interior (Top-Left inward)";

  (* If a coordinate doesn't exist (parsing fails), it cannot be an edge well *)
  check Well_96 "Z99" false "Invalid coordinate is not an edge"

let test_parse_coordinate () =
  let check input expected msg =
    let result = Exlab_core.Plate.parse_coordinate input in
    Alcotest.(check (result (pair int int) string)) msg expected result
  in

  check "A1" (Ok (0, 0)) "A1 -> (0, 0)";
  check "B2" (Ok (1, 1)) "B2 -> (1, 1)";
  check "H12" (Ok (7, 11)) "H12 -> (7, 11)";

  (* Parse coordinate just needs to interpret the data, not validate it's existence on a plate. *)
  check "Z100" (Ok (25, 99)) "Z100 -> (25, 99)";

  let is_error result msg =
    match result with
    | Ok _ -> Alcotest.fail (msg ^ ": Expected Error, got Ok")
    | Error _ -> Alcotest.(check pass) msg true true
  in

  is_error
    (Exlab_core.Plate.parse_coordinate "B0")
    "Reject 'B0' (Column 0 does not exist)";
  is_error (Exlab_core.Plate.parse_coordinate "A-1") "Reject negative numbers";
  is_error (Exlab_core.Plate.parse_coordinate "A") "Reject 'A' (no number)";
  is_error (Exlab_core.Plate.parse_coordinate "1") "Reject '1' (no letter)";
  is_error (Exlab_core.Plate.parse_coordinate "") "Reject empty string";
  is_error (Exlab_core.Plate.parse_coordinate "@@") "Reject symbols";
  is_error (Exlab_core.Plate.parse_coordinate "1A") "Reject inverted '1A'";
  is_error (Exlab_core.Plate.parse_coordinate "@1") "Reject '@1' (Row < A)";
  is_error
    (Exlab_core.Plate.parse_coordinate "[1")
    "Reject '[1' (Row char beyond Z, usually caught by upper bound check \
     later, but good to test)";
  is_error
    (Exlab_core.Plate.parse_coordinate "11")
    "Reject '11' (First digit interpreted as char)"

(* Test Suite *)
let suite =
  [
    ( "Plate",
      [
        test_case "Roundtrip" `Quick test_plate_roundtrip;
        test_case "Validate Name - Valid" `Quick test_validate_name_valid;
        test_case "Validate Name - Invalid" `Quick test_validate_name_invalid;
        test_case "Unknown Format Error" `Quick test_plate_format_unknown;
        test_case "Generate Well Coordinates" `Quick
          test_generate_well_coordinates;
      ] );
    ( "Coordinate Logic",
      [
        test_case "Coordinate of Index" `Quick test_coordinate_of_index;
        test_case "Index of Coordinate" `Quick test_index_of_coordinate;
        test_case "Full Coordinate-Index Roundtrip" `Quick
          test_coordinate_index_roundtrip;
        test_case "Parse Coordinate" `Quick test_parse_coordinate;
        test_case "Normalize Coordinate (Mixed Inputs)" `Quick
          test_normalize_coordinate;
        test_case "Compare Coordinates (Column-Major)" `Quick
          test_compare_coordinates_column_major;
        test_case "Is Edge Well" `Quick test_is_edge_well;
      ] );
    ( "Plate Compliance",
      [
        test_case "Allow same category" `Quick test_validate_mixture_success;
        test_case "Reject mixed categories" `Quick
          test_validate_mixture_conflict;
        test_case "Allow anything on empty" `Quick test_validate_mixture_empty;
      ] );
    ("Well", [ test_case "Roundtrip" `Quick test_well_roundtrip ]);
  ]
