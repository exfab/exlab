open Alcotest
open Exlab_core.Types
open Test_helpers
open ResultPayload

(* Create testables for both result_definition and result_value *)
let result_definition_t = testable pp_result_definition equal_result_definition
let result_value_t = testable pp_result_value equal_result_value
let result_payload_t = testable ResultPayload.pp ResultPayload.equal

let test_result_definition_roundtrip_with_category () =
  let category =
    {
      id = 1;
      uid = "cat_uid1";
      name = "Test Category";
      description = Some "A test category";
    }
  in
  let original =
    {
      id = 1;
      uid = "uid1";
      short_id = "RD1";
      name = "OD600";
      description = Some "Optical Density at 600nm";
      data_type = ResultType.Float;
      unit = Some "AU";
      category = Some category;
      is_required = false;
      created_at = 0.0;
      updated_at = 0.0;
    }
  in
  check_roundtrip result_definition_t yojson_of_result_definition
    result_definition_of_yojson "Result Definition with category Roundtrip"
    original

let test_result_definition_roundtrip_without_category () =
  let original =
    {
      id = 1;
      uid = "uid1";
      short_id = "RD1";
      name = "OD600";
      description = Some "Optical Density at 600nm";
      data_type = ResultType.Float;
      unit = Some "AU";
      category = None;
      is_required = false;
      created_at = 0.0;
      updated_at = 0.0;
    }
  in
  check_roundtrip result_definition_t yojson_of_result_definition
    result_definition_of_yojson "Result Definition without category Roundtrip"
    original

let test_result_value_roundtrip () =
  let original =
    {
      id = 1;
      uid = "uid1";
      sample_id = Some 1;
      plate_id = None;
      result_definition_id = 1;
      value = Some (ResultPayload.Boolean true);
      created_at = 0.0;
      updated_at = 0.0;
    }
  in
  check_roundtrip result_value_t yojson_of_result_value result_value_of_yojson
    "Result Value Roundtrip" original

let test_result_value_plate_roundtrip () =
  let original =
    {
      id = 2;
      uid = "uid2";
      sample_id = None;
      plate_id = Some 1;
      result_definition_id = 1;
      value = Some (ResultPayload.Boolean false);
      created_at = 0.0;
      updated_at = 0.0;
    }
  in
  check_roundtrip result_value_t yojson_of_result_value result_value_of_yojson
    "Result Value Plate Roundtrip" original

let test_result_payload_string_roundtrip () =
  let original = String "hello" in
  check_roundtrip result_payload_t ResultPayload.yojson_of_t
    ResultPayload.t_of_yojson "String payload roundtrip" original

let test_result_payload_integer_roundtrip () =
  let original = Integer 123 in
  check_roundtrip result_payload_t ResultPayload.yojson_of_t
    ResultPayload.t_of_yojson "Integer payload roundtrip" original

let test_result_payload_float_roundtrip () =
  let original = Float 123.45 in
  check_roundtrip result_payload_t ResultPayload.yojson_of_t
    ResultPayload.t_of_yojson "Float payload roundtrip" original

let test_result_payload_boolean_roundtrip () =
  let original = Boolean true in
  check_roundtrip result_payload_t ResultPayload.yojson_of_t
    ResultPayload.t_of_yojson "Boolean payload roundtrip" original

let test_result_payload_date_roundtrip () =
  let original = Date 1672531200.0 in
  check_roundtrip result_payload_t ResultPayload.yojson_of_t
    ResultPayload.t_of_yojson "Date payload roundtrip" original

let test_result_payload_datetime_roundtrip () =
  let original = Datetime 1672531200.0 in
  check_roundtrip result_payload_t ResultPayload.yojson_of_t
    ResultPayload.t_of_yojson "Datetime payload roundtrip" original

let test_result_payload_filelink_roundtrip () =
  let original = FileLink "gs://bucket/path/to/file" in
  check_roundtrip result_payload_t ResultPayload.yojson_of_t
    ResultPayload.t_of_yojson "FileLink payload roundtrip" original

let test_payload_validation_mismatch () =
  let definition_data_type = ResultType.Integer in
  let payload = ResultPayload.Float 1.23 in
  let result =
    Exlab_core.Result.validate_payload definition_data_type payload
  in
  Alcotest.(check bool)
    "Mismatched payload should be an error" true (Result.is_error result)

let test_payload_validation_match () =
  let definition_data_type = ResultType.Integer in
  let payload = ResultPayload.Integer 123 in
  let result =
    Exlab_core.Result.validate_payload definition_data_type payload
  in
  Alcotest.(check bool)
    "Matched payload should be ok" true (Result.is_ok result)

let test_payload_validation_filelink_match () =
  let definition_data_type = ResultType.FileLink in
  let payload = ResultPayload.FileLink "gs://bucket/path/to/file" in
  let result =
    Exlab_core.Result.validate_payload definition_data_type payload
  in
  Alcotest.(check bool)
    "Matched filelink payload should be ok" true (Result.is_ok result)

let test_timeseries_rejects_dates () =
  let result1 = ResultType.of_string "TimeSeries:Date" in
  let result2 = ResultType.of_string "TimeSeries:Datetime" in
  Alcotest.(check bool)
    "Should reject TimeSeries:Date" true (Result.is_error result1);
  Alcotest.(check bool)
    "Should reject TimeSeries:Datetime" true (Result.is_error result2)

let test_timeseries_auto_sort () =
  let json =
    `Assoc
      [
        ("type", `String "StringSeries");
        ( "value",
          `List
            [
              `Assoc [ ("time", `Float 2.0); ("value", `String "B") ];
              `Assoc [ ("time", `Float 1.0); ("value", `String "A") ];
            ] );
      ]
  in
  let result = ResultPayload.t_of_yojson json in
  let expected = ResultPayload.StringSeries [ (1, "A"); (2, "B") ] in
  Alcotest.(check result_payload_t)
    "TimeSeries points should be automatically sorted by time" expected result

let test_payload_of_string_timeseries () =
  let result =
    Exlab_core.Result.payload_of_string ResultType.FloatSeries
      "[[1.0, 5.0], [2.0, 10.0]]"
  in
  let expected = Ok (ResultPayload.FloatSeries [ (1, 5.0); (2, 10.0) ]) in
  Alcotest.(check (result result_payload_t string))
    "Should parse JSON array out of string for TimeSeries" expected result

let test_required_validation_fails_when_missing () =
  let definition =
    {
      id = 1;
      uid = "u1";
      short_id = "RD1";
      name = "Req";
      description = None;
      data_type = ResultType.String;
      unit = None;
      category = None;
      is_required = true;
      created_at = 0.0;
      updated_at = 0.0;
    }
  in
  let result = Exlab_core.Result.validate_result definition None in
  Alcotest.(check bool)
    "Required field missing should be an error" true (Result.is_error result)

let test_required_validation_succeeds_when_present () =
  let definition =
    {
      id = 1;
      uid = "u1";
      short_id = "RD1";
      name = "Req";
      description = None;
      data_type = ResultType.String;
      unit = None;
      category = None;
      is_required = true;
      created_at = 0.0;
      updated_at = 0.0;
    }
  in
  let payload = Some (ResultPayload.String "val") in
  let result = Exlab_core.Result.validate_result definition payload in
  Alcotest.(check bool)
    "Required field present should be ok" true (Result.is_ok result)

let test_payload_strict_integer_parsing_fails () =
  let json = `Assoc [ ("type", `String "Integer"); ("value", `Float 1.23) ] in
  try
    let _ = ResultPayload.t_of_yojson json in
    Alcotest.fail "Should have failed to parse float as integer"
  with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error _ -> ()

let test_payload_loose_float_parsing_succeeds () =
  let json = `Assoc [ ("type", `String "Float"); ("value", `Int 123) ] in
  let result = ResultPayload.t_of_yojson json in
  Alcotest.(check result_payload_t)
    "Should parse integer as float" (ResultPayload.Float 123.0) result

let test_result_payload_timeseries_roundtrip () =
  let original =
    ResultPayload.StringSeries [ (1, "point 1"); (2, "point 2") ]
  in
  check_roundtrip result_payload_t ResultPayload.yojson_of_t
    ResultPayload.t_of_yojson "TimeSeries payload roundtrip" original

let test_payload_validation_timeseries_match () =
  let definition_data_type = ResultType.FloatSeries in
  let payload = ResultPayload.FloatSeries [ (1, 4.2); (2, 8.4) ] in
  let result =
    Exlab_core.Result.validate_payload definition_data_type payload
  in
  Alcotest.(check bool)
    "Matched timeseries payload should be ok" true (Result.is_ok result)

let test_payload_validation_timeseries_mismatch () =
  let definition_data_type = ResultType.FloatSeries in
  let payload = ResultPayload.StringSeries [ (1, "not a float") ] in
  let result =
    Exlab_core.Result.validate_payload definition_data_type payload
  in
  Alcotest.(check bool)
    "Mismatched inner timeseries payload should be error" true
    (Result.is_error result)

let suite =
  [
    ( "Result Definition",
      [
        test_case "Roundtrip with category" `Quick
          test_result_definition_roundtrip_with_category;
        test_case "Roundtrip without category" `Quick
          test_result_definition_roundtrip_without_category;
      ] );
    ( "Result Value",
      [
        test_case "Roundtrip" `Quick test_result_value_roundtrip;
        test_case "Plate Roundtrip" `Quick test_result_value_plate_roundtrip;
      ] );
    ( "Result Payload",
      [
        test_case "String Roundtrip" `Quick test_result_payload_string_roundtrip;
        test_case "Integer Roundtrip" `Quick
          test_result_payload_integer_roundtrip;
        test_case "Float Roundtrip" `Quick test_result_payload_float_roundtrip;
        test_case "Boolean Roundtrip" `Quick
          test_result_payload_boolean_roundtrip;
        test_case "Date Roundtrip" `Quick test_result_payload_date_roundtrip;
        test_case "Datetime Roundtrip" `Quick
          test_result_payload_datetime_roundtrip;
        test_case "FileLink Roundtrip" `Quick
          test_result_payload_filelink_roundtrip;
        test_case "Strict Integer Parsing Fails" `Quick
          test_payload_strict_integer_parsing_fails;
        test_case "Loose Float Parsing Succeeds" `Quick
          test_payload_loose_float_parsing_succeeds;
        test_case "TimeSeries Roundtrip" `Quick
          test_result_payload_timeseries_roundtrip;
        test_case "TimeSeries rejects Date/Datetime" `Quick
          test_timeseries_rejects_dates;
        test_case "TimeSeries auto sorts points" `Quick
          test_timeseries_auto_sort;
      ] );
    ( "Result Validation",
      [
        test_case "Mismatched payload" `Quick test_payload_validation_mismatch;
        test_case "Matched payload" `Quick test_payload_validation_match;
        test_case "Parse TimeSeries from string" `Quick
          test_payload_of_string_timeseries;
        test_case "Matched filelink payload" `Quick
          test_payload_validation_filelink_match;
        test_case "Matched timeseries payload" `Quick
          test_payload_validation_timeseries_match;
        test_case "Mismatched timeseries payload" `Quick
          test_payload_validation_timeseries_mismatch;
        test_case "Required missing fails" `Quick
          test_required_validation_fails_when_missing;
        test_case "Required present succeeds" `Quick
          test_required_validation_succeeds_when_present;
      ] );
  ]
