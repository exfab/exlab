open Alcotest
open Exlab_core.Types
open Test_helpers

let strain_t = testable pp_strain equal_strain
let link_t = testable pp_strain_external_link equal_strain_external_link
let db_def_t = testable pp_external_db_definition equal_external_db_definition

let make_dummy_db_def () : external_db_definition =
  {
    id = 1;
    uid = "db-ncbi";
    name = "NCBI Taxonomy";
    url_template = Some "https://ncbi...{}";
    created_at = 1000.0;
    updated_at = 2000.0;
  }

let make_dummy_link () : strain_external_link =
  {
    id = 50;
    uid = "link-1";
    strain_id = 10;
    external_db_definition_id = 1;
    value = "562";
    created_at = 1000.0;
  }

let make_dummy_strain_with_link () : strain =
  {
    id = 10;
    uid = "sample-001";
    genus = "Ralstonia";
    species = "mucilaginosa";
    strain_name = "ATCC 25296";
    genotype = Some "delta_arg";
    parent_strain_id = Some 10;
    notes = Some "This is a strain";
    external_links = Some [ make_dummy_link () ];
    created_at = 1000.0;
    updated_at = 2000.0;
  }

let make_dummy_strain_no_link () : strain =
  {
    id = 10;
    uid = "sample-001";
    genus = "Ralstonia";
    species = "mucilaginosa";
    strain_name = "ATCC 25296";
    genotype = Some "delta_arg";
    parent_strain_id = Some 10;
    notes = Some "This is a strain";
    external_links = None;
    created_at = 1000.0;
    updated_at = 2000.0;
  }

let test_db_def_roundtrip () =
  check_roundtrip db_def_t yojson_of_external_db_definition
    external_db_definition_of_yojson "DB Definition Roundtrip"
    (make_dummy_db_def ())

let test_strain_roundtrip () =
  check_roundtrip strain_t yojson_of_strain strain_of_yojson "Strain Roundtrip"
    (make_dummy_strain_with_link ())

let test_strain_no_link_roundtrip () =
  check_roundtrip strain_t yojson_of_strain strain_of_yojson
    "Strain Roundtrip (No Links)"
    (make_dummy_strain_no_link ())

let test_normalize_name () =
  let norm = Exlab_core.Strain.normalize_name in
  check string "Capitalizes genus" "Escherichia" (norm "escherichia");
  check string "Trims whitespace" "Bacillus" (norm "  bacillus  ");
  check string "Handles empty" "" (norm "")

let test_validate_creation_success () =
  let res =
    Exlab_core.Strain.validate_creation ~genus:" escherichia " ~species:" coli "
      ~strain_name:" DH5alpha " ~genotype:(Some " delta_recA ")
      ~parent_strain_id:(Some 123)
  in
  match res with
  | Ok (gen, sp, st, geno, pid) ->
      check string "Genus Normalized" "Escherichia" gen;
      check string "Species Trimmed" "coli" sp;
      check string "Strain Trimmed" "DH5alpha" st;
      check (option string) "Genotype Trimmed" (Some "delta_recA") geno;
      check (option int) "Parent ID preserved" (Some 123) pid
  | Error e -> fail ("Unexpected validation failure: " ^ e)

let test_validate_creation_failures () =
  let validate g s d =
    Exlab_core.Strain.validate_creation ~genus:g ~species:s ~strain_name:d
      ~genotype:None ~parent_strain_id:None
  in

  (* Empty Genus *)
  match validate "   " "coli" "Strain" with
  | Error msg -> check string "Caught empty genus" "Genus cannot be empty" msg
  | Ok _ -> (
      Alcotest.(check bool) "Should reject empty genus" true false;

      (* Empty Species *)
      match validate "Escherichia" "  " "Strain" with
      | Error msg ->
          check string "Caught empty species" "Species cannot be empty" msg
      | Ok _ -> (
          Alcotest.(check bool) "Should reject empty species" true false;

          (* Empty Strain *)
          match validate "Escherichia" "coli" "" with
          | Error msg ->
              check string "Caught empty strain" "Strain Name cannot be empty"
                msg
          | Ok _ ->
              Alcotest.(check bool) "Should reject empty strain" true false))

let test_genotype_cleaning () =
  let validate_genotype g =
    Exlab_core.Strain.validate_creation ~genus:"G" ~species:"S" ~strain_name:"D"
      ~genotype:g ~parent_strain_id:None
  in

  (* "   " should become None *)
  match validate_genotype (Some "   ") with
  | Ok (_, _, _, None, _) ->
      check pass "Whitespace genotype becomes None" true true
  | Ok (_, _, _, Some s, _) ->
      check (option string) "Expected None (cleaned)" None (Some s)
  | Error e -> (
      Alcotest.(check bool) ("Unexpected Error: " ^ e) true false;

      (* None stays None *)
      match validate_genotype None with
      | Ok (_, _, _, None, _) -> check pass "None stays None" true true
      | _ -> Alcotest.(check bool) "Error handling None genotype" true false)

let test_full_label_formatting () =
  (* Case 1: Standard Format *)
  let s1 =
    {
      (make_dummy_strain_no_link ()) with
      genus = "Escherichia";
      species = "coli";
      strain_name = "DH5alpha";
    }
  in
  check string "Combines species and name" "Escherichia coli DH5alpha"
    (Exlab_core.Strain.format_label s1);

  (* Case 2: Handle redundancy (user typed "Escherichia coli K12" as the name) *)
  let s2 = { s1 with strain_name = "Escherichia coli K12" } in
  check string "Dedups species if present in name" "Escherichia coli K12"
    (Exlab_core.Strain.format_label s2)

let test_resolve_external_url () =
  (* A DB definition with a placeholder {} *)
  let db_def =
    {
      (make_dummy_db_def ()) with
      url_template =
        Some "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id={}";
    }
  in
  let link = { (make_dummy_link ()) with value = "562" } in

  (* Action: Resolve the URL *)
  let resolved = Exlab_core.Strain_link.resolve_url db_def link in

  check (option string) "Resolves {} template correctly"
    (Some "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=562")
    resolved;

  (* Edge Case: No template defined *)
  let db_no_temp = { db_def with url_template = None } in
  check (option string) "Returns None if template missing" None
    (Exlab_core.Strain_link.resolve_url db_no_temp link)

let suite =
  [
    ( "Strain JSON",
      [
        test_case "Roundtrip" `Quick test_strain_roundtrip;
        test_case "Roundtrip No Links" `Quick test_strain_no_link_roundtrip;
        test_case "External Database Definition" `Quick test_db_def_roundtrip;
      ] );
    ( "Strain Logic",
      [
        test_case "Name Normalization" `Quick test_normalize_name;
        test_case "Validation Success" `Quick test_validate_creation_success;
        test_case "Validation Failures" `Quick test_validate_creation_failures;
        test_case "Genotype Cleaning" `Quick test_genotype_cleaning;
      ] );
    ( "Strain Utilities",
      [
        test_case "Label Formatting" `Quick test_full_label_formatting;
        test_case "URL Resolution" `Quick test_resolve_external_url;
      ] );
  ]
