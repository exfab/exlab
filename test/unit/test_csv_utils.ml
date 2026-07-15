open Exlab_server.Csv_utils

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

let test_to_string_option () =
  Alcotest.(check (option string))
    "some" (Some "hello")
    (to_string_option " hello ");
  Alcotest.(check (option string)) "none" None (to_string_option "  ")

let test_to_int_option () =
  Alcotest.(check (option int)) "some" (Some 123) (to_int_option " 123 ");
  Alcotest.(check (option int)) "none" None (to_int_option " abc ");
  Alcotest.(check (option int)) "none empty" None (to_int_option "")

let test_to_int () =
  Alcotest.(check int) "valid" 123 (to_int "123");
  Alcotest.(check int) "invalid" 0 (to_int "abc");
  Alcotest.(check int) "default" 42 (to_int ~default:42 "abc")

let test_to_float_option () =
  Alcotest.(check (option (float 0.0)))
    "some" (Some 1.23) (to_float_option " 1.23 ");
  Alcotest.(check (option (float 0.0))) "none" None (to_float_option " abc ");
  Alcotest.(check (option (float 0.0))) "none empty" None (to_float_option "")

let test_to_float () =
  Alcotest.(check (float 0.0)) "valid" 1.23 (to_float "1.23");
  Alcotest.(check (float 0.0)) "invalid" 0.0 (to_float "abc");
  Alcotest.(check (float 0.0)) "default" 42.0 (to_float ~default:42.0 "abc")

let test_to_bool_option () =
  Alcotest.(check (option bool)) "true" (Some true) (to_bool_option "true");
  Alcotest.(check (option bool)) "false" (Some false) (to_bool_option "false");
  Alcotest.(check (option bool)) "none" None (to_bool_option "abc")

let test_to_bool () =
  Alcotest.(check bool) "true" true (to_bool "true");
  Alcotest.(check bool) "false" false (to_bool "false");
  Alcotest.(check bool) "invalid" false (to_bool "abc");
  Alcotest.(check bool) "default" true (to_bool ~default:true "abc")

let test_to_list () =
  Alcotest.(check (list string)) "semicolon" [ "a"; "b"; "c" ] (to_list "a;b;c");
  Alcotest.(check (list string))
    "pipe" [ "a"; "b"; "c" ] (to_list ~sep:'|' "a|b|c");
  Alcotest.(check (list string)) "empty" [] (to_list "")

let test_to_int_list () =
  Alcotest.(check (list int)) "semicolon" [ 1; 2; 3 ] (to_int_list "1;2;3");
  Alcotest.(check (list int)) "mixed" [ 1; 3 ] (to_int_list "1;a;3")

let test_to_float_list () =
  Alcotest.(check (list (float 0.0)))
    "semicolon" [ 1.1; 2.2; 3.3 ]
    (to_float_list "1.1;2.2;3.3");
  Alcotest.(check (list (float 0.0)))
    "mixed" [ 1.1; 3.3 ]
    (to_float_list "1.1;a;3.3")

type test_record = { a : int; b : string }

let test_record_testable =
  Alcotest.testable
    (Fmt.of_to_string (fun r -> Printf.sprintf "{a=%d; b=%s}" r.a r.b))
    ( = )

let decoder get = Ok { a = to_int (get "A"); b = to_string (get "B") }

let test_parse () =
  let body = "A,B\n1,one\n2,two" in
  let expected = Ok [ { a = 1; b = "one" }; { a = 2; b = "two" } ] in
  let result = parse body decoder in
  Alcotest.(check (result (list test_record_testable) string))
    "records" expected result

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
        Alcotest.test_case "to_string_option" `Quick test_to_string_option;
        Alcotest.test_case "to_int_option" `Quick test_to_int_option;
        Alcotest.test_case "to_int" `Quick test_to_int;
        Alcotest.test_case "to_float_option" `Quick test_to_float_option;
        Alcotest.test_case "to_float" `Quick test_to_float;
        Alcotest.test_case "to_bool_option" `Quick test_to_bool_option;
        Alcotest.test_case "to_bool" `Quick test_to_bool;
        Alcotest.test_case "to_list" `Quick test_to_list;
        Alcotest.test_case "to_int_list" `Quick test_to_int_list;
        Alcotest.test_case "to_float_list" `Quick test_to_float_list;
        Alcotest.test_case "parse" `Quick test_parse;
      ] );
  ]
