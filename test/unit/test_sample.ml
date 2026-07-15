(* test/unit/test_sample.ml *)

open Alcotest
open Exlab_core.Types
open Test_helpers

let sample_t = testable pp_sample equal_sample

let make_dummy_sample () : sample =
  {
    id = 10;
    uid = "sample-001";
    short_id = "SMP-001";
    project_id = 1;
    sample_type = "Test";
    status = Active;
    category = Experimental;
    parent_sample_id = None;
    strain_id = None;
    community_id = None;
    created_at = 1000.0;
    updated_at = 2000.0;
  }

let test_sample_roundtrip () =
  check_roundtrip sample_t yojson_of_sample sample_of_yojson "Sample Roundtrip"
    (make_dummy_sample ())

let test_validate_sample_type_valid () =
  let result = Exlab_core.Sample.validate_sample_type "Liquid Cell Culture" in
  Alcotest.(check (result unit string)) "Valid sample type" (Ok ()) result

let test_validate_sample_type_invalid () =
  let result = Exlab_core.Sample.validate_sample_type "Invalid Type" in
  Alcotest.(check (result unit string))
    "Invalid sample type"
    (Error
       "Invalid sample_type: 'Invalid Type'. Must be one of: Liquid Cell \
        Culture, Solid Cell Culture, Library Sample, Unknown")
    result

let test_validate_creation_source_with_strain_valid () =
  let result =
    Exlab_core.Sample.validate_creation ~sample_type:"Liquid Cell Culture"
      ~category:Source ~parent_sample_id:None ~strain_id:(Some 1)
      ~community_id:None
  in
  Alcotest.(check (result unit string))
    "Source with strain (no parent) is valid" (Ok ()) result

let test_validate_creation_source_with_community_valid () =
  let result =
    Exlab_core.Sample.validate_creation ~sample_type:"Liquid Cell Culture"
      ~category:Source ~parent_sample_id:None ~strain_id:None
      ~community_id:(Some 1)
  in
  Alcotest.(check (result unit string))
    "Source with community (no strain) is valid" (Ok ()) result

let test_validate_creation_source_invalid_no_strain () =
  let result =
    Exlab_core.Sample.validate_creation ~sample_type:"Liquid Cell Culture"
      ~category:Source ~parent_sample_id:None ~strain_id:None ~community_id:None
  in
  Alcotest.(check (result unit string))
    "Source must have a strain or community"
    (Error "A Source sample must have a strain_id or community_id.") result

let test_validate_creation_experimental_valid () =
  let result =
    Exlab_core.Sample.validate_creation ~sample_type:"Liquid Cell Culture"
      ~category:Experimental ~parent_sample_id:(Some 123) ~strain_id:None
      ~community_id:None
  in
  Alcotest.(check (result unit string))
    "Experimental with parent is valid" (Ok ()) result

let test_validate_creation_experimental_invalid () =
  let result =
    Exlab_core.Sample.validate_creation ~sample_type:"Liquid Cell Culture"
      ~category:Experimental ~parent_sample_id:None ~strain_id:None
      ~community_id:None
  in
  Alcotest.(check (result unit string))
    "Experimental without parent is Invalid"
    (Error "An Experimental sample must have a parent_sample_id.") result

let test_generate_short_id_source () =
  let result =
    Exlab_core.Sample.generate_short_id ~category:Exlab_core.Types.Source
      ~parent_short_id_opt:None ~project_prefix_opt:(Some "PROJ") ~count:0
  in
  Alcotest.(check (result string string))
    "Source short_id with prefix" (Ok "PROJ-0001") result;

  let result2 =
    Exlab_core.Sample.generate_short_id ~category:Exlab_core.Types.Source
      ~parent_short_id_opt:None ~project_prefix_opt:None ~count:5
  in
  Alcotest.(check (result string string))
    "Source short_id without prefix" (Ok "SMP-0006") result2

let test_generate_short_id_experimental () =
  let result =
    Exlab_core.Sample.generate_short_id ~category:Exlab_core.Types.Experimental
      ~parent_short_id_opt:(Some "PROJ-0001") ~project_prefix_opt:None ~count:0
  in
  Alcotest.(check (result string string))
    "Experimental short_id" (Ok "PROJ-0001-01") result;

  let result2 =
    Exlab_core.Sample.generate_short_id ~category:Exlab_core.Types.Experimental
      ~parent_short_id_opt:None ~project_prefix_opt:None ~count:0
  in
  Alcotest.(check (result string string))
    "Experimental short_id without parent fails"
    (Error "Experimental sample requires a parent_short_id to generate short_id")
    result2

let suite =
  [
    ( "Sample",
      [
        Alcotest.test_case "Roundtrip" `Quick test_sample_roundtrip;
        Alcotest.test_case "Validate Sample Type (Valid)" `Quick
          test_validate_sample_type_valid;
        Alcotest.test_case "Validate Sample Type (Invalid)" `Quick
          test_validate_sample_type_invalid;
        Alcotest.test_case "Creation Rule: Source with Strain (Valid)" `Quick
          test_validate_creation_source_with_strain_valid;
        Alcotest.test_case "Creation Rule: Source with Community (Valid)" `Quick
          test_validate_creation_source_with_community_valid;
        Alcotest.test_case "Creation Rule: Source (Invalid, no strain)" `Quick
          test_validate_creation_source_invalid_no_strain;
        Alcotest.test_case "Creation Rule: Experimental (Valid)" `Quick
          test_validate_creation_experimental_valid;
        Alcotest.test_case "Creation Rule: Experimental (Invalid)" `Quick
          test_validate_creation_experimental_invalid;
        Alcotest.test_case "Generate Short ID: Source" `Quick
          test_generate_short_id_source;
        Alcotest.test_case "Generate Short ID: Experimental" `Quick
          test_generate_short_id_experimental;
      ] );
  ]
