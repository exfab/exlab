let test_natural_compare_basic () =
  let input = [ "Plate_10"; "Plate_2"; "Plate_1" ] in
  let expected = [ "Plate_1"; "Plate_2"; "Plate_10" ] in
  let sorted = List.sort Exlab_core.String_utils.natural_compare input in
  Alcotest.(check (list string)) "Sorts plates naturally" expected sorted

let test_natural_compare_no_separator () =
  let input = [ "Plate10"; "Plate2"; "Plate1" ] in
  let expected = [ "Plate1"; "Plate2"; "Plate10" ] in
  let sorted = List.sort Exlab_core.String_utils.natural_compare input in
  Alcotest.(check (list string))
    "Sorts plates without separator naturally" expected sorted

let test_natural_compare_complex () =
  let input = [ "S-10-A"; "S-2-B"; "S-2-A"; "S-1" ] in
  let expected = [ "S-1"; "S-2-A"; "S-2-B"; "S-10-A" ] in
  let sorted = List.sort Exlab_core.String_utils.natural_compare input in
  Alcotest.(check (list string)) "Sorts complex IDs naturally" expected sorted

let test_natural_compare_mixed () =
  let input = [ "10"; "2"; "a10"; "a2" ] in
  let expected = [ "2"; "10"; "a2"; "a10" ] in
  let sorted = List.sort Exlab_core.String_utils.natural_compare input in
  Alcotest.(check (list string))
    "Sorts mixed numbers and strings" expected sorted

let test_natural_compare_short_ids () =
  let exp_input =
    [
      "UCR-001-0001-100";
      "UCR-001-0001-11";
      "UCR-001-0001-01";
      "UCR-001-0001-20";
      "UCR-001-0001-10";
    ]
  in
  let exp_expected =
    [
      "UCR-001-0001-01";
      "UCR-001-0001-10";
      "UCR-001-0001-11";
      "UCR-001-0001-20";
      "UCR-001-0001-100";
    ]
  in
  Alcotest.(check (list string))
    "Sorts experimental sample short IDs past 99 naturally" exp_expected
    (List.sort Exlab_core.String_utils.natural_compare exp_input);

  let plate_input = [ "P-10-0001"; "P-2-10000"; "P-2-9999"; "P-2-0001" ] in
  let plate_expected = [ "P-2-0001"; "P-2-9999"; "P-2-10000"; "P-10-0001" ] in
  Alcotest.(check (list string))
    "Sorts plate short IDs across digit widths naturally" plate_expected
    (List.sort Exlab_core.String_utils.natural_compare plate_input)

let test_natural_compare_leading_zero_antisymmetry () =
  let cmp =
    Exlab_core.String_utils.natural_compare "UCR-001-0001-01" "UCR-001-0001-1"
  in
  Alcotest.(check bool)
    "Distinct strings differing only by leading zeros do not compare equal" true
    (cmp <> 0);
  let deduped =
    List.sort_uniq Exlab_core.String_utils.natural_compare
      [ "UCR-001-0001-01"; "UCR-001-0001-1"; "UCR-001-0001-01" ]
  in
  Alcotest.(check int)
    "sort_uniq keeps both distinct leading-zero variants" 2
    (List.length deduped)

let test_strip_trailing_slashes () =
  Alcotest.(check string)
    "strips multiple trailing slashes" "gs://bucket/path"
    (Exlab_core.String_utils.strip_trailing_slashes "gs://bucket/path///");
  Alcotest.(check string)
    "leaves string without trailing slash untouched" "gs://bucket"
    (Exlab_core.String_utils.strip_trailing_slashes "gs://bucket");
  Alcotest.(check string)
    "handles all slashes" ""
    (Exlab_core.String_utils.strip_trailing_slashes "///")

let test_non_empty () =
  Alcotest.(check (option string))
    "trims non-empty" (Some "hello")
    (Exlab_core.String_utils.non_empty "  hello  ");
  Alcotest.(check (option string))
    "empty becomes None" None
    (Exlab_core.String_utils.non_empty "");
  Alcotest.(check (option string))
    "whitespace becomes None" None
    (Exlab_core.String_utils.non_empty "   \t\n ")

let test_clean_option () =
  Alcotest.(check (option string))
    "None stays None" None
    (Exlab_core.String_utils.clean_option None);
  Alcotest.(check (option string))
    "blank Some becomes None" None
    (Exlab_core.String_utils.clean_option (Some "   "));
  Alcotest.(check (option string))
    "trims Some value" (Some "Yeast")
    (Exlab_core.String_utils.clean_option (Some " Yeast "))

let test_find_canonical_ci () =
  let allowed = [ "Fungi"; " Filamentous Fungi "; ""; "Bacteria" ] in
  Alcotest.(check (option string))
    "matches case-insensitively and returns canonical spelling" (Some "Fungi")
    (Exlab_core.String_utils.find_canonical_ci "  fungi " allowed);
  Alcotest.(check (option string))
    "trims canonical entry from allowed list" (Some "Filamentous Fungi")
    (Exlab_core.String_utils.find_canonical_ci "FILAMENTOUS FUNGI" allowed);
  Alcotest.(check (option string))
    "blank target does not match empty entry" None
    (Exlab_core.String_utils.find_canonical_ci "   " allowed);
  Alcotest.(check (option string))
    "unknown value returns None" None
    (Exlab_core.String_utils.find_canonical_ci "Virus" allowed)

let test_to_int () =
  Alcotest.(check (option int))
    "option some" (Some 123)
    (Exlab_core.String_utils.to_int_option " 123 ");
  Alcotest.(check (option int))
    "option invalid" None
    (Exlab_core.String_utils.to_int_option " abc ");
  Alcotest.(check (option int))
    "option empty" None
    (Exlab_core.String_utils.to_int_option "");
  Alcotest.(check int) "valid" 123 (Exlab_core.String_utils.to_int "123");
  Alcotest.(check int) "invalid" 0 (Exlab_core.String_utils.to_int "abc");
  Alcotest.(check int)
    "default" 42
    (Exlab_core.String_utils.to_int ~default:42 "abc")

let test_to_float () =
  Alcotest.(check (option (float 0.0)))
    "option some" (Some 1.23)
    (Exlab_core.String_utils.to_float_option " 1.23 ");
  Alcotest.(check (option (float 0.0)))
    "option invalid" None
    (Exlab_core.String_utils.to_float_option " abc ");
  Alcotest.(check (option (float 0.0)))
    "option empty" None
    (Exlab_core.String_utils.to_float_option "");
  Alcotest.(check (float 0.0))
    "valid" 1.23
    (Exlab_core.String_utils.to_float "1.23");
  Alcotest.(check (float 0.0))
    "invalid" 0.0
    (Exlab_core.String_utils.to_float "abc");
  Alcotest.(check (float 0.0))
    "default" 42.0
    (Exlab_core.String_utils.to_float ~default:42.0 "abc")

let test_to_bool () =
  Alcotest.(check (option bool))
    "true" (Some true)
    (Exlab_core.String_utils.to_bool_option " YES ");
  Alcotest.(check (option bool))
    "false" (Some false)
    (Exlab_core.String_utils.to_bool_option "false");
  Alcotest.(check (option bool))
    "none" None
    (Exlab_core.String_utils.to_bool_option "abc");
  Alcotest.(check bool) "valid true" true (Exlab_core.String_utils.to_bool "1");
  Alcotest.(check bool)
    "valid false" false
    (Exlab_core.String_utils.to_bool "0");
  Alcotest.(check bool) "invalid" false (Exlab_core.String_utils.to_bool "abc");
  Alcotest.(check bool)
    "default" true
    (Exlab_core.String_utils.to_bool ~default:true "abc")

let test_split_and_typed_lists () =
  Alcotest.(check (list string))
    "semicolon" [ "a"; "b"; "c" ]
    (Exlab_core.String_utils.split_trimmed " a ; ; b ; c ");
  Alcotest.(check (list string))
    "pipe" [ "a"; "b"; "c" ]
    (Exlab_core.String_utils.split_trimmed ~sep:'|' "a|b|c");
  Alcotest.(check (list string))
    "empty" []
    (Exlab_core.String_utils.split_trimmed "   ");
  Alcotest.(check (list int))
    "int list" [ 1; 2; 3 ]
    (Exlab_core.String_utils.to_int_list "1;2;3");
  Alcotest.(check (list int))
    "int list drops invalid" [ 1; 3 ]
    (Exlab_core.String_utils.to_int_list "1;a;3");
  Alcotest.(check (list (float 0.0)))
    "float list" [ 1.1; 2.2; 3.3 ]
    (Exlab_core.String_utils.to_float_list "1.1;2.2;3.3");
  Alcotest.(check (list (float 0.0)))
    "float list drops invalid" [ 1.1; 3.3 ]
    (Exlab_core.String_utils.to_float_list "1.1;a;3.3")

let suite =
  [
    ( "String Utils",
      [
        Alcotest.test_case "Natural compare basic" `Quick
          test_natural_compare_basic;
        Alcotest.test_case "Natural compare no separator" `Quick
          test_natural_compare_no_separator;
        Alcotest.test_case "Natural compare complex" `Quick
          test_natural_compare_complex;
        Alcotest.test_case "Natural compare mixed" `Quick
          test_natural_compare_mixed;
        Alcotest.test_case "Natural compare short IDs across digit widths"
          `Quick test_natural_compare_short_ids;
        Alcotest.test_case "Natural compare leading-zero antisymmetry" `Quick
          test_natural_compare_leading_zero_antisymmetry;
        Alcotest.test_case "strip_trailing_slashes" `Quick
          test_strip_trailing_slashes;
        Alcotest.test_case "non_empty" `Quick test_non_empty;
        Alcotest.test_case "clean_option" `Quick test_clean_option;
        Alcotest.test_case "find_canonical_ci" `Quick test_find_canonical_ci;
        Alcotest.test_case "to_int / to_int_option" `Quick test_to_int;
        Alcotest.test_case "to_float / to_float_option" `Quick test_to_float;
        Alcotest.test_case "to_bool / to_bool_option" `Quick test_to_bool;
        Alcotest.test_case "split_trimmed / to_int_list / to_float_list" `Quick
          test_split_and_typed_lists;
      ] );
  ]
