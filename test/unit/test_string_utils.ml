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
      ] );
  ]
