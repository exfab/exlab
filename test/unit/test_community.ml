open Alcotest
open Exlab_core.Types
open Test_helpers

let community_t = testable pp_community equal_community

let make_dummy_community () : community =
  {
    id = 1;
    uid = "community-001";
    name = "Soil Metagenome";
    notes = Some "Collected from Site A";
    metadata = None;
    created_at = 1000.0;
    updated_at = 2000.0;
  }

let test_community_roundtrip () =
  check_roundtrip community_t yojson_of_community community_of_yojson
    "Community Roundtrip" (make_dummy_community ())

let community_member_t = testable pp_community_member equal_community_member

let make_dummy_member () : community_member =
  {
    id = 1;
    uid = "member-001";
    community_id = 1;
    strain_id = Some 10;
    label = "E. coli MG1655";
    taxon =
      Some
        "Bacteria;Proteobacteria;Gammaproteobacteria;Enterobacterales;Enterobacteriaceae;Escherichia";
    created_at = 1000.0;
  }

let test_community_member_roundtrip () =
  check_roundtrip community_member_t yojson_of_community_member
    community_member_of_yojson "Community Member Roundtrip"
    (make_dummy_member ())

let suite =
  [
    ( "Community JSON",
      [
        test_case "Community Roundtrip" `Quick test_community_roundtrip;
        test_case "Member Roundtrip" `Quick test_community_member_roundtrip;
      ] );
  ]
