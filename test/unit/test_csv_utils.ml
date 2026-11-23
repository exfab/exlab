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

let test_parse_matrix () =
  let body = ",1,2\nA, S-1 , \nB,,S-2" in
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
  match parse_matrix body with
  | Ok items ->
      Alcotest.(check (list item_testable))
        "parses non-empty trimmed matrix cells" expected items
  | Error (`Bad_Request msg) -> Alcotest.fail ("Unexpected error: " ^ msg)

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
        Alcotest.test_case "parse" `Quick test_parse;
        Alcotest.test_case "parse_matrix" `Quick test_parse_matrix;
      ] );
  ]
