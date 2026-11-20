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
      ] );
  ]
