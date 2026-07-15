open Alcotest
open Exlab_core.Types
open Test_helpers

let setting_t = testable pp_system_setting equal_system_setting

let test_setting_json_roundtrip () =
  let original =
    {
      id = 1;
      key = "project_metadata_template";
      value =
        `List
          [
            `Assoc
              [ ("key", `String "Client"); ("field_type", `String "String") ];
          ];
      description = Some "Default metadata keys for new projects";
      updated_at = 1000.0;
    }
  in
  check_roundtrip setting_t yojson_of_system_setting system_setting_of_yojson
    "System Setting JSON roundtrip" original

let suite =
  [
    ( "System Setting",
      [ test_case "Roundtrip" `Quick test_setting_json_roundtrip ] );
  ]
