open Exlab_server.Csv_utils
module S = Exlab_core.String_utils

let test_safe_associate_pads_missing_data () =
  let header = [ "A"; "B"; "C" ] in
  let row = [ "1"; "2" ] in
  let expected = [ ("A", "1"); ("B", "2"); ("C", "") ] in
  let result = safe_associate header row in
  Alcotest.(check (list (pair string string)))
    "should pad missing data" expected result

let test_safe_associate_truncates_extra_data () =
  let header = [ "A"; "B" ] in
  let row = [ "1"; "2"; "3" ] in
  let expected = [ ("A", "1"); ("B", "2") ] in
  let result = safe_associate header row in
  Alcotest.(check (list (pair string string)))
    "should truncate extra data" expected result

let test_read_csv_with_headers_trims_headers () =
  let body = " A , B , C \n1,2,3" in
  let expected = [ [ ("A", "1"); ("B", "2"); ("C", "3") ] ] in
  let result = read_csv_with_headers body in
  Alcotest.(check (list (list (pair string string))))
    "should trim headers" expected result

type test_record = { a : int; b : string }

let test_record_testable =
  Alcotest.testable
    (Fmt.of_to_string (fun r -> Printf.sprintf "{a=%d; b=%s}" r.a r.b))
    ( = )

let decoder get = Ok { a = S.to_int (get "A"); b = String.trim (get "B") }

let test_parse () =
  let body = "A,B\n1,one\n2,two" in
  let expected = Ok [ { a = 1; b = "one" }; { a = 2; b = "two" } ] in
  let result = parse body decoder in
  Alcotest.(check (result (list test_record_testable) string))
    "records" expected result

let test_read_rows () =
  Alcotest.(check (result (list (list string)) string))
    "reads valid CSV rows"
    (Ok [ [ "A"; "B" ]; [ "1"; "2" ] ])
    (read_rows "A,B\n1,2");
  match read_rows "A,B\n\"unclosed,2" with
  | Error _ -> Alcotest.(check pass) "returns Error on malformed CSV" true true
  | Ok _ -> Alcotest.fail "Expected Error on malformed CSV quote"

let test_parse_skips_blank_rows_and_preserves_line_numbers () =
  let strict_decoder get =
    match S.non_empty (get "B") with
    | None -> Error "B cannot be empty"
    | Some b -> Ok { a = S.to_int (get "A"); b }
  in
  let body = "A,B\n1,one\n   \n,\n2,two\n\n" in
  let expected = Ok [ { a = 1; b = "one" }; { a = 2; b = "two" } ] in
  Alcotest.(check (result (list test_record_testable) string))
    "skips completely blank rows" expected
    (parse body strict_decoder);
  let body_with_err = "A,B\n1,one\n   \n2,   " in
  Alcotest.(check (result (list test_record_testable) string))
    "reports accurate original CSV row number after blank row"
    (Error "Row 4: B cannot be empty")
    (parse body_with_err strict_decoder);
  match parse "A,B\n\"unclosed,1" strict_decoder with
  | Error _ ->
      Alcotest.(check pass) "returns Error on malformed CSV syntax" true true
  | Ok _ -> Alcotest.fail "Expected Error on malformed CSV syntax"

let test_parse_matrix () =
  let body = " , 1 , 2 \n A , S-1 , \n   \n B , , S-2 \n" in
  let expected =
    [
      { Exlab_server.Api_types.Plate.well = "A1"; sample_short_id = "S-1" };
      { Exlab_server.Api_types.Plate.well = "B2"; sample_short_id = "S-2" };
    ]
  in
  let item_testable =
    Alcotest.testable Exlab_server.Api_types.Plate.pp_well_layout_item
      Exlab_server.Api_types.Plate.equal_well_layout_item
  in
  (match parse_matrix body with
  | Ok items ->
      Alcotest.(check (list item_testable))
        "parses trimmed matrix coordinates and skips blank rows" expected items
  | Error (`Bad_Request msg) -> Alcotest.fail ("Unexpected error: " ^ msg));
  match parse_matrix ",1,2\n\"unclosed,S-1" with
  | Error (`Bad_Request _) ->
      Alcotest.(check pass)
        "returns Bad_Request on malformed matrix CSV" true true
  | Ok _ -> Alcotest.fail "Expected Bad_Request on malformed matrix CSV"

let suite =
  [
    ( "CSV Utils",
      [
        Alcotest.test_case "safe_associate pads missing data" `Quick
          test_safe_associate_pads_missing_data;
        Alcotest.test_case "safe_associate truncates extra data" `Quick
          test_safe_associate_truncates_extra_data;
        Alcotest.test_case "read_csv_with_headers trims headers" `Quick
          test_read_csv_with_headers_trims_headers;
        Alcotest.test_case "read_rows handles valid and malformed CSV" `Quick
          test_read_rows;
        Alcotest.test_case "parse" `Quick test_parse;
        Alcotest.test_case
          "parse skips blank rows, preserves line numbers, handles bad quotes"
          `Quick test_parse_skips_blank_rows_and_preserves_line_numbers;
        Alcotest.test_case "parse_matrix" `Quick test_parse_matrix;
      ] );
  ]
