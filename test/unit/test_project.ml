open Alcotest
open Exlab_core.Types
open Test_helpers

let project_t = testable pp_project equal_project

let test_project_json_roundtrip () =
  let original =
    {
      id = 1;
      uid = "test-uid-123";
      short_id = "PRJ-0001";
      name = "My Experiment";
      description = Some "Testing stuff";
      status = Active;
      contact_name = None;
      owner = Some "Dr. Test";
      metadata =
        Some (`Assoc [ ("Client", `String "Acme Corp"); ("Priority", `Int 1) ]);
      created_at = 1000.0;
      updated_at = 2000.0;
    }
  in
  check_roundtrip project_t yojson_of_project project_of_yojson
    "Project JSON roundtrip" original

let test_generate_short_id () =
  let result = Exlab_core.Project.generate_short_id ~project_id:42 in
  Alcotest.(check string) "Generates correctly" "PROJ-0042" result

let test_validate_metadata_valid () =
  let template : Exlab_core.Types.metadata_field_def list =
    [
      { key = "Client"; field_type = String };
      { key = "Status"; field_type = Enum [ "A"; "B" ] };
    ]
  in
  let metadata =
    Some (`Assoc [ ("Client", `String "Acme Corp"); ("Status", `String "A") ])
  in
  let result = Exlab_core.Project.validate_metadata metadata template in
  Alcotest.(check (result unit string)) "Valid metadata passes" (Ok ()) result

let test_validate_metadata_invalid_key () =
  let template : Exlab_core.Types.metadata_field_def list =
    [ { key = "Client"; field_type = String } ]
  in
  let metadata = Some (`Assoc [ ("Unknown", `String "Value") ]) in
  let result = Exlab_core.Project.validate_metadata metadata template in
  Alcotest.(check (result unit string))
    "Invalid key fails"
    (Error "Metadata key 'Unknown' is not defined in the template.") result

let suite =
  [
    ( "Project",
      [
        test_case "Roundtrip" `Quick test_project_json_roundtrip;
        test_case "Generate Short ID" `Quick test_generate_short_id;
        test_case "Validate Metadata (Valid)" `Quick
          test_validate_metadata_valid;
        test_case "Validate Metadata (Invalid Key)" `Quick
          test_validate_metadata_invalid_key;
      ] );
  ]
