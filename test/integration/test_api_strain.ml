(* Test Definitions for API endpoint /api/v1/strains *)

let handler = Test_utils.admin_app

let test_create_malformed_json _switch () =
  let body = {| {"genus": "Broken JSON", |} in
  let req = Test_utils.json_post ~path:"/api/v1/strains" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_fields _switch () =
  let body = {| {"genus": "Saccharomyces", "species": "cerevisiae"} |} in
  let req = Test_utils.json_post ~path:"/api/v1/strains" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing fields" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_strain_lifecycle _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let strain_name = Printf.sprintf "Test Strain UID %f" time in
  let body =
    Printf.sprintf
      {| {
    "genus": "Saccharomyces",
    "species": "cerevisiae",
    "strain_name": "%s",
    "genotype": "WT",
    "parent_strain_id": null,
    "notes": "A test strain for UID lookup.",
    "links": []
  } |}
      strain_name
  in

  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body
      ~expected_status:201 "Create successful"
  in
  let* create_body = Dream.body create_res in
  let json = Yojson.Safe.from_string create_body in
  let strain_id = Yojson.Safe.Util.(member "id" json |> to_int) in

  let path = Printf.sprintf "/api/v1/strains/%d" strain_id in
  let* _ =
    Test_utils.assert_json_get ~handler ~path ~expected_status:200
      "Get by ID should return 200"
  in

  let update_body =
    Printf.sprintf
      {| {
    "genus": "Saccharomyces",
    "species": "cerevisiae",
    "strain_name": "%s Updated",
    "genotype": "delta-foo",
    "parent_strain_id": null,
    "notes": "Updated notes"
  } |}
      strain_name
  in
  let* _ =
    Test_utils.assert_json_put ~handler ~path ~body:update_body
      ~expected_status:200 "Update successful"
  in

  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/strains"
      ~expected_status:200 "Get all strains"
  in
  Lwt.return ()

let test_bulk_create_json_success _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let name_a = Printf.sprintf "Strain A %f" time in
  let name_b = Printf.sprintf "Strain B %f" time in
  let body =
    Printf.sprintf
      {|
    { "strains": [
        { "genus": "Saccharomyces", "species": "cerevisiae", "strain_name": "%s", "genotype": "WT" },
        { "genus": "Escherichia", "species": "coli", "strain_name": "%s", "genotype": "foo" }
      ]
    }
  |}
      name_a name_b
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains/bulk" ~body
      ~expected_status:201 "Bulk create successful"
  in
  Lwt.return ()

let test_bulk_create_csv_success _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let name_c = Printf.sprintf "Strain C %f" time in
  let name_d = Printf.sprintf "Strain D %f" time in
  let csv_body =
    Printf.sprintf
      "genus,species,strain_name,genotype\n\
       Saccharomyces,cerevisiae,%s,WT\n\
       Escherichia,coli,%s,foo"
      name_c name_d
  in
  let* _ =
    Test_utils.assert_csv_post ~handler ~path:"/api/v1/strains/bulk-csv"
      ~body:csv_body ~expected_status:201 "Bulk CSV create successful"
  in
  Lwt.return ()

let test_search_strains _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let search_term = Printf.sprintf "UniqueSearchTerm%f" time in
  let name_a = Printf.sprintf "Strain A %s" search_term in
  let name_b = "Unrelated Strain" in

  let body_a =
    Printf.sprintf
      {| {"genus": "Saccharomyces", "species": "cerevisiae", "strain_name": "%s", "genotype": "WT", "links": []} |}
      name_a
  in
  let body_b =
    Printf.sprintf
      {| {"genus": "Escherichia", "species": "coli", "strain_name": "%s", "genotype": "foo", "links": []} |}
      name_b
  in

  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body:body_a
      ~expected_status:201 "Create Strain A"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body:body_b
      ~expected_status:201 "Create Strain B"
  in

  let search_path = Printf.sprintf "/api/v1/strains?search=%s" search_term in
  let* search_res =
    Test_utils.assert_json_get ~handler ~path:search_path ~expected_status:200
      "Search strains"
  in

  let* search_body = Dream.body search_res in
  let json = Yojson.Safe.from_string search_body in
  let data = Yojson.Safe.Util.(member "data" json |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching strain" 1 (List.length data);

  let returned_name =
    Yojson.Safe.Util.(List.hd data |> member "strain_name" |> to_string)
  in
  Alcotest.(check string) "Returned strain name matches" name_a returned_name;

  Lwt.return ()

let test_advanced_search_strains _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let search_term = Printf.sprintf "AdvSearchTerm%f" time in
  let name_a = Printf.sprintf "Strain A %s" search_term in
  let name_b = Printf.sprintf "Strain B %s" search_term in

  let body_a =
    Printf.sprintf
      {| {"genus": "Saccharomyces", "species": "cerevisiae", "strain_name": "%s", "genotype": "WT", "links": []} |}
      name_a
  in
  let body_b =
    Printf.sprintf
      {| {"genus": "Escherichia", "species": "coli", "strain_name": "%s", "genotype": "foo", "links": []} |}
      name_b
  in

  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body:body_a
      ~expected_status:201 "Create Strain A"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains" ~body:body_b
      ~expected_status:201 "Create Strain B"
  in

  (* Search by genus *)
  let search_path_genus =
    Printf.sprintf "/api/v1/strains?search=%s&genus=Saccharomyces" search_term
  in
  let* search_res_genus =
    Test_utils.assert_json_get ~handler ~path:search_path_genus
      ~expected_status:200 "Search strains by genus"
  in
  let* search_body_genus = Dream.body search_res_genus in
  let json_genus = Yojson.Safe.from_string search_body_genus in
  let data_genus = Yojson.Safe.Util.(member "data" json_genus |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching strain by genus" 1
    (List.length data_genus);
  let returned_name_genus =
    Yojson.Safe.Util.(List.hd data_genus |> member "strain_name" |> to_string)
  in
  Alcotest.(check string)
    "Returned strain name matches" name_a returned_name_genus;

  (* Search by species *)
  let search_path_species =
    Printf.sprintf "/api/v1/strains?search=%s&species=coli" search_term
  in
  let* search_res_species =
    Test_utils.assert_json_get ~handler ~path:search_path_species
      ~expected_status:200 "Search strains by species"
  in
  let* search_body_species = Dream.body search_res_species in
  let json_species = Yojson.Safe.from_string search_body_species in
  let data_species = Yojson.Safe.Util.(member "data" json_species |> to_list) in

  Alcotest.(check int)
    "Should return exactly 1 matching strain by species" 1
    (List.length data_species);
  let returned_name_species =
    Yojson.Safe.Util.(List.hd data_species |> member "strain_name" |> to_string)
  in
  Alcotest.(check string)
    "Returned strain name matches" name_b returned_name_species;

  Lwt.return ()

let test_strain_get_nested_projects _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Nested Proj %f" time in
  let strain_name = Printf.sprintf "Nested Strain %f" time in

  (* 1. Create Project *)
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
      ~expected_status:201 "Create Project"
  in
  let* proj_body = Dream.body proj_res in
  let proj_id =
    Yojson.Safe.Util.(member "id" (Yojson.Safe.from_string proj_body) |> to_int)
  in

  (* 2. Create Strain *)
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Test", "species": "strain", "strain_name": "%s", "links": []}|}
           strain_name)
      ~expected_status:201 "Create Strain"
  in
  let* strain_body = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body) |> to_int)
  in

  (* 3. Create Source Sample *)
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d}|}
           strain_id)
      ~expected_status:201 "Create Source Sample"
  in

  (* 4. Get Strain and verify nested project data *)
  let path = Printf.sprintf "/api/v1/strains/%d" strain_id in
  let* get_res =
    Test_utils.assert_json_get ~handler ~path ~expected_status:200
      "Get Strain by ID"
  in
  let* get_body = Dream.body get_res in
  let json = Yojson.Safe.from_string get_body in

  let source_samples =
    Yojson.Safe.Util.(member "source_samples" json |> to_list)
  in
  Alcotest.(check int)
    "Should have exactly 1 source sample" 1
    (List.length source_samples);

  let sample_entry = List.hd source_samples in
  let returned_proj_id =
    Yojson.Safe.Util.(member "project" sample_entry |> member "id" |> to_int)
  in
  let returned_proj_name =
    Yojson.Safe.Util.(
      member "project" sample_entry |> member "name" |> to_string)
  in

  Alcotest.(check int) "Returned project ID matches" proj_id returned_proj_id;
  Alcotest.(check string)
    "Returned project name matches" proj_name returned_proj_name;

  Lwt.return ()

let test_strain_classification_crud_filter_and_bulk _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let tag =
    Printf.sprintf "Cls_%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.int 1000000)
  in

  (* 1. Create strain with domain + taxonomic_group (case-insensitive normalization) *)
  let* res1 =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Rhodotorula", "species": "mucilaginosa", "strain_name": "%s_Y", "domain": "fungi", "taxonomic_group": "yeast"}|}
           tag)
      ~expected_status:201 "Create Fungi/Yeast strain"
  in
  let* body1 = Dream.body res1 in
  let j1 = Yojson.Safe.from_string body1 in
  let id1 = j1 |> member "id" |> to_int in
  Alcotest.(check string)
    "Domain normalized" "Fungi"
    (j1 |> member "domain" |> to_string);
  Alcotest.(check string)
    "Taxonomic group normalized" "Yeast"
    (j1 |> member "taxonomic_group" |> to_string);

  (* 2. Create Filamentous Fungi strain *)
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Neurospora", "species": "crassa", "strain_name": "%s_F", "domain": "Fungi", "taxonomic_group": "Filamentous Fungi"}|}
           tag)
      ~expected_status:201 "Create Fungi/Filamentous Fungi strain"
  in

  (* 3. Reject invalid domain and invalid taxonomic_group with 400 *)
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Haloferax", "species": "volcanii", "strain_name": "%s_BadDom", "domain": "NotADomain"}|}
           tag)
      ~expected_status:400 "Reject unknown domain"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Rhodotorula", "species": "mucilaginosa", "strain_name": "%s_BadGrp", "domain": "Fungi", "taxonomic_group": "NotAGroup"}|}
           tag)
      ~expected_status:400 "Reject unknown taxonomic_group"
  in

  (* 4. Filter by domain and taxonomic_group *)
  let* filt_res =
    Test_utils.assert_json_get ~handler
      ~path:
        (Printf.sprintf
           "/api/v1/strains?search=%s&domain=Fungi&taxonomic_group=Filamentous%%20Fungi"
           tag)
      ~expected_status:200 "Filter strains by domain + taxonomic_group"
  in
  let* filt_body = Dream.body filt_res in
  let filt_data =
    Yojson.Safe.from_string filt_body |> member "data" |> to_list
  in
  Alcotest.(check int)
    "Filter returns exactly 1 Filamentous Fungi strain" 1
    (List.length filt_data);
  Alcotest.(check string)
    "Filtered strain is _F" (tag ^ "_F")
    (List.hd filt_data |> member "strain_name" |> to_string);

  (* 5. PUT omitting domain preserves existing domain; PUT changing taxonomic_group updates it *)
  let* put_res =
    Test_utils.assert_json_put ~handler
      ~path:(Printf.sprintf "/api/v1/strains/%d" id1)
      ~body:
        (Printf.sprintf
           {|{"genus": "Rhodotorula", "species": "mucilaginosa", "strain_name": "%s_Y", "taxonomic_group": "Macrofungi"}|}
           tag)
      ~expected_status:200
      "PUT preserving domain while updating taxonomic_group"
  in
  let* put_body = Dream.body put_res in
  let put_j = Yojson.Safe.from_string put_body in
  Alcotest.(check string)
    "Domain preserved on PUT when omitted" "Fungi"
    (put_j |> member "domain" |> to_string);
  Alcotest.(check string)
    "Taxonomic group updated on PUT" "Macrofungi"
    (put_j |> member "taxonomic_group" |> to_string);

  (* 6. Bulk CSV with domain + taxonomic_group *)
  let csv_body =
    Printf.sprintf
      "genus,species,strain_name,domain,taxonomic_group\n\
       Saccharomyces,cerevisiae,%s_CSV,Fungi,Yeast\n\
       Haloferax,volcanii,%s_CSV2,Archaea,"
      tag tag
  in
  let* csv_res =
    Test_utils.assert_csv_post ~handler ~path:"/api/v1/strains/bulk-csv"
      ~body:csv_body ~expected_status:201
      "Bulk CSV with domain and taxonomic_group succeeds"
  in
  let* csv_str = Dream.body csv_res in
  let csv_strains =
    Yojson.Safe.from_string csv_str |> member "strains" |> to_list
  in
  Alcotest.(check int) "Created 2 strains via CSV" 2 (List.length csv_strains);
  Alcotest.(check string)
    "First CSV strain has Fungi" "Fungi"
    (List.nth csv_strains 0 |> member "domain" |> to_string);
  Alcotest.(check string)
    "Second CSV strain has Archaea" "Archaea"
    (List.nth csv_strains 1 |> member "domain" |> to_string);
  Lwt.return ()

let test_bulk_csv_patch_strains _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let tag =
    Printf.sprintf "Patch_%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.int 1000000)
  in

  let* res1 =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Neurospora", "species": "crassa", "strain_name": "%s_1", "genotype": "WT", "notes": "keep-notes-1"}|}
           tag)
      ~expected_status:201 "Create strain 1"
  in
  let* str1 = Dream.body res1 in
  let id1 = Yojson.Safe.from_string str1 |> member "id" |> to_int in

  let* res2 =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Rhodotorula", "species": "mucilaginosa", "strain_name": "%s_2", "genotype": "del-x", "notes": "keep-notes-2"}|}
           tag)
      ~expected_status:201 "Create strain 2"
  in
  let* str2 = Dream.body res2 in
  let j2 = Yojson.Safe.from_string str2 in
  let id2 = j2 |> member "id" |> to_int in
  let uid2 = j2 |> member "uid" |> to_string in

  (* 1. Missing id/uid header returns 400 *)
  let no_id_res =
    Dream.test handler
      (Test_utils.csv_patch ~path:"/api/v1/strains/bulk-csv"
         ~body:"domain,taxonomic_group\nFungi,Yeast")
  in
  Alcotest.(check int)
    "Missing id/uid returns 400" 400
    (Dream.status no_id_res |> Dream.status_to_int);

  (* 2. Invalid domain in row 2 returns 400 and does NOT update row 1 *)
  let bad_csv =
    Printf.sprintf "id,domain,taxonomic_group\n%d,Bacteria,\n%d,NotADomain," id1
      id2
  in
  let bad_res =
    Dream.test handler
      (Test_utils.csv_patch ~path:"/api/v1/strains/bulk-csv" ~body:bad_csv)
  in
  Alcotest.(check int)
    "Invalid domain in bulk PATCH returns 400" 400
    (Dream.status bad_res |> Dream.status_to_int);

  (* 3. Unknown strain ID in row 2 returns 404 and does NOT update row 1 *)
  let missing_id_csv =
    Printf.sprintf
      "id,domain,taxonomic_group\n%d,Bacteria,\n999999999,Fungi,Yeast" id1
  in
  let missing_res =
    Dream.test handler
      (Test_utils.csv_patch ~path:"/api/v1/strains/bulk-csv"
         ~body:missing_id_csv)
  in
  Alcotest.(check int)
    "Unknown strain id in bulk PATCH returns 404" 404
    (Dream.status missing_res |> Dream.status_to_int);

  let* still_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/strains/%d" id1)
      ~expected_status:200 "Fetch strain 1 after failed PATCHes"
  in
  let* still_body = Dream.body still_res in
  Alcotest.(check string)
    "Failed bulk PATCHes did not mutate strain 1" "Unassigned"
    (Yojson.Safe.from_string still_body
    |> member "strain" |> member "domain" |> to_string);

  (* 4. Valid PATCH by id and uid updates classification and preserves untouched fields *)
  let patch_csv =
    Printf.sprintf
      "id,uid,domain,taxonomic_group\n\
       %d,,Fungi,Filamentous Fungi\n\
       ,%s,fungi,yeast"
      id1 uid2
  in
  let commit_res =
    Dream.test handler
      (Test_utils.csv_patch ~path:"/api/v1/strains/bulk-csv" ~body:patch_csv)
  in
  Alcotest.(check int)
    "Commit bulk PATCH returns 200" 200
    (Dream.status commit_res |> Dream.status_to_int);
  let* commit_body = Dream.body commit_res in
  let updated_strains =
    Yojson.Safe.from_string commit_body |> member "strains" |> to_list
  in
  Alcotest.(check int)
    "Response lists 2 updated strains" 2
    (List.length updated_strains);

  let* after1_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/strains/%d" id1)
      ~expected_status:200 "Fetch strain 1 after PATCH"
  in
  let* after1_body = Dream.body after1_res in
  let s1 = Yojson.Safe.from_string after1_body |> member "strain" in
  Alcotest.(check string)
    "Strain 1 domain updated" "Fungi"
    (s1 |> member "domain" |> to_string);
  Alcotest.(check string)
    "Strain 1 taxonomic_group updated" "Filamentous Fungi"
    (s1 |> member "taxonomic_group" |> to_string);
  Alcotest.(check string)
    "Strain 1 genotype preserved" "WT"
    (s1 |> member "genotype" |> to_string);
  Alcotest.(check string)
    "Strain 1 notes preserved" "keep-notes-1"
    (s1 |> member "notes" |> to_string);

  let* after2_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/strains/%d" id2)
      ~expected_status:200 "Fetch strain 2 after PATCH"
  in
  let* after2_body = Dream.body after2_res in
  let s2 = Yojson.Safe.from_string after2_body |> member "strain" in
  Alcotest.(check string)
    "Strain 2 domain updated" "Fungi"
    (s2 |> member "domain" |> to_string);
  Alcotest.(check string)
    "Strain 2 taxonomic_group updated" "Yeast"
    (s2 |> member "taxonomic_group" |> to_string);
  Alcotest.(check string)
    "Strain 2 notes preserved" "keep-notes-2"
    (s2 |> member "notes" |> to_string);
  Lwt.return ()

let test_custom_strain_domains_via_settings _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let tag =
    Printf.sprintf "CustomDom_%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.int 1000000)
  in
  let restore_defaults () =
    Test_utils.assert_json_put ~handler ~path:"/api/v1/settings/strain_domains"
      ~body:{|{"value": ["Fungi", "Bacteria", "Archaea", "Algae"]}|}
      ~expected_status:204 "Restore default strain_domains"
  in
  let* _ =
    Test_utils.assert_json_put ~handler ~path:"/api/v1/settings/strain_domains"
      ~body:{|{"value": ["Fungi", "Bacteria", "Archaea", "Algae", "Virus"]}|}
      ~expected_status:204 "Add custom Virus domain to settings"
  in
  Lwt.finalize
    (fun () ->
      let* create_res =
        Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
          ~body:
            (Printf.sprintf
               {|{"genus": "Tequatrovirus", "species": "T4", "strain_name": "%s_V", "domain": "virus"}|}
               tag)
          ~expected_status:201 "Create strain with custom Virus domain"
      in
      let* create_body = Dream.body create_res in
      let j = Yojson.Safe.from_string create_body in
      let vid = j |> member "id" |> to_int in
      Alcotest.(check string)
        "Custom domain normalized to canonical casing" "Virus"
        (j |> member "domain" |> to_string);

      let* filt_res =
        Test_utils.assert_json_get ~handler
          ~path:(Printf.sprintf "/api/v1/strains?search=%s&domain=virus" tag)
          ~expected_status:200 "Filter strains by custom domain"
      in
      let* filt_body = Dream.body filt_res in
      let filt_data =
        Yojson.Safe.from_string filt_body |> member "data" |> to_list
      in
      Alcotest.(check int)
        "Custom domain filter returns 1 strain" 1 (List.length filt_data);

      (* Remove Virus from settings and verify legacy value is still preserved on PUT *)
      let* _ = restore_defaults () in
      let* _ =
        Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
          ~body:
            (Printf.sprintf
               {|{"genus": "Tequatrovirus", "species": "T4", "strain_name": "%s_V2", "domain": "Virus"}|}
               tag)
          ~expected_status:400
          "Creating new strain with removed domain returns 400"
      in
      let* put_res =
        Test_utils.assert_json_put ~handler
          ~path:(Printf.sprintf "/api/v1/strains/%d" vid)
          ~body:
            (Printf.sprintf
               {|{"genus": "Tequatrovirus", "species": "T4", "strain_name": "%s_V", "domain": "Virus", "notes": "legacy ok"}|}
               tag)
          ~expected_status:200
          "Updating existing strain preserves its legacy domain"
      in
      let* put_body = Dream.body put_res in
      Alcotest.(check string)
        "Legacy domain retained after PUT" "Virus"
        (Yojson.Safe.from_string put_body |> member "domain" |> to_string);
      Lwt.return ())
    (fun () ->
      let* _ = restore_defaults () in
      Lwt.return ())

let suite =
  [
    ( "Strain API",
      [
        Alcotest_lwt.test_case "Strain Lifecycle (Create, Get, Update, List)"
          `Quick test_strain_lifecycle;
        Alcotest_lwt.test_case "Get Strain returns nested projects" `Quick
          test_strain_get_nested_projects;
        Alcotest_lwt.test_case "Search Strains" `Quick test_search_strains;
        Alcotest_lwt.test_case "Advanced Search Strains" `Quick
          test_advanced_search_strains;
        Alcotest_lwt.test_case "Bulk JSON: Success" `Quick
          test_bulk_create_json_success;
        Alcotest_lwt.test_case "Bulk CSV: Success" `Quick
          test_bulk_create_csv_success;
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "Reject Missing Fields" `Quick
          test_create_missing_fields;
        Alcotest_lwt.test_case "Classification CRUD, Filtering, and Bulk CSV"
          `Quick test_strain_classification_crud_filter_and_bulk;
        Alcotest_lwt.test_case "Bulk CSV PATCH Strains" `Quick
          test_bulk_csv_patch_strains;
        Alcotest_lwt.test_case "Custom Strain Domains via System Settings"
          `Quick test_custom_strain_domains_via_settings;
      ] );
  ]
