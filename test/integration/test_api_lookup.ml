(* Test Definitions for API endpoint /api/v1/lookup *)

let handler = Test_utils.admin_app

open Lwt.Syntax

let create_test_project name =
  let body = Printf.sprintf {|{"name": %S, "status": "Active"}|} name in
  let req = Test_utils.json_post ~path:"/api/v1/projects" ~body in
  let res = Dream.test handler req in
  let* body_str = Dream.body res in
  let json = Yojson.Safe.from_string body_str in
  let id = Yojson.Safe.Util.(json |> member "id" |> to_int) in
  let short_id = Yojson.Safe.Util.(json |> member "short_id" |> to_string) in
  Lwt.return (id, short_id)

let create_test_strain strain_name =
  let body =
    Printf.sprintf
      {|{"genus": "Saccharomyces", "species": "cerevisiae", "strain_name": %S, "genotype": "WT", "notes": "", "links": []}|}
      strain_name
  in
  let req = Test_utils.json_post ~path:"/api/v1/strains" ~body in
  let res = Dream.test handler req in
  let* body_str = Dream.body res in
  let json = Yojson.Safe.from_string body_str in
  let id = Yojson.Safe.Util.(json |> member "id" |> to_int) in
  let uid = Yojson.Safe.Util.(json |> member "uid" |> to_string) in
  Lwt.return (id, uid)

let test_lookup_sample_short_id_and_unknown _switch () =
  let* project_id, _ = create_test_project "Lookup Tracer Project" in
  let* strain_id, _ = create_test_strain "BY4741-Tracer" in
  let sample_body =
    Printf.sprintf
      {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": []}|}
      strain_id
  in
  let sample_req =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" project_id)
      ~body:sample_body
  in
  let sample_res = Dream.test handler sample_req in
  let* sample_str = Dream.body sample_res in
  let sample_json = Yojson.Safe.from_string sample_str in
  let sample_short_id =
    Yojson.Safe.Util.(sample_json |> member "short_id" |> to_string)
  in

  let lookup_req =
    Test_utils.json_get
      ~path:(Printf.sprintf "/api/v1/lookup?q=%s" sample_short_id)
  in
  let lookup_res = Dream.test handler lookup_req in
  let* lookup_body = Dream.body lookup_res in
  Alcotest.(check int)
    ("Should return 200 OK for lookup, got: " ^ lookup_body)
    200
    (Dream.status lookup_res |> Dream.status_to_int);

  let json = Yojson.Safe.from_string lookup_body in
  let matches = Yojson.Safe.Util.(json |> member "matches" |> to_list) in
  Alcotest.(check int) "Should find 1 match" 1 (List.length matches);
  let m = List.hd matches in
  Alcotest.(check string)
    "Resource type should be sample" "sample"
    Yojson.Safe.Util.(m |> member "resource_type" |> to_string);
  Alcotest.(check string)
    "Short ID should match" sample_short_id
    Yojson.Safe.Util.(m |> member "short_id" |> to_string);
  Alcotest.(check string)
    "Name should match" "Liquid Cell Culture (Source)"
    Yojson.Safe.Util.(m |> member "name" |> to_string);
  Alcotest.(check string)
    "URL should route to sample"
    ("/samples/" ^ sample_short_id)
    Yojson.Safe.Util.(m |> member "url" |> to_string);

  (* Unknown ID should return 200 OK with empty matches *)
  let unknown_req =
    Test_utils.json_get ~path:"/api/v1/lookup?q=DOES-NOT-EXIST-999999"
  in
  let unknown_res = Dream.test handler unknown_req in
  let* unknown_body = Dream.body unknown_res in
  Alcotest.(check int)
    "Should return 200 OK for unknown ID" 200
    (Dream.status unknown_res |> Dream.status_to_int);
  let unknown_json = Yojson.Safe.from_string unknown_body in
  let unknown_matches =
    Yojson.Safe.Util.(unknown_json |> member "matches" |> to_list)
  in
  Alcotest.(check int)
    "Should return 0 matches for unknown ID" 0
    (List.length unknown_matches);
  Lwt.return ()

let assert_single_lookup ~query ~expected_type ~expected_url =
  let req =
    Test_utils.json_get
      ~path:(Printf.sprintf "/api/v1/lookup?q=%s" (Uri.pct_encode query))
  in
  let res = Dream.test handler req in
  let* body = Dream.body res in
  Alcotest.(check int)
    ("Lookup 200 OK for " ^ query ^ ": " ^ body)
    200
    (Dream.status res |> Dream.status_to_int);
  let json = Yojson.Safe.from_string body in
  let matches = Yojson.Safe.Util.(json |> member "matches" |> to_list) in
  Alcotest.(check int)
    ("Expected 1 match for " ^ query ^ ", got: " ^ body)
    1 (List.length matches);
  let m = List.hd matches in
  Alcotest.(check string)
    ("Resource type for " ^ query)
    expected_type
    Yojson.Safe.Util.(m |> member "resource_type" |> to_string);
  Alcotest.(check string)
    ("URL for " ^ query) expected_url
    Yojson.Safe.Util.(m |> member "url" |> to_string);
  Lwt.return ()

let test_lookup_all_resource_types _switch () =
  let* project_id, project_short_id =
    create_test_project "Multi Resource Lookup Project"
  in
  let* () =
    assert_single_lookup ~query:project_short_id ~expected_type:"project"
      ~expected_url:("/projects/" ^ project_short_id)
  in

  (* Plate by short_id *)
  let plate_req =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Lookup Plate", "project_id": %d, "plate_format": "96-well"}|}
           project_id)
  in
  let plate_res = Dream.test handler plate_req in
  let* plate_str = Dream.body plate_res in
  let plate_json = Yojson.Safe.from_string plate_str in
  let plate_id = Yojson.Safe.Util.(plate_json |> member "id" |> to_int) in
  let plate_short_id =
    Yojson.Safe.Util.(plate_json |> member "short_id" |> to_string)
  in
  let* () =
    assert_single_lookup ~query:plate_short_id ~expected_type:"plate"
      ~expected_url:(Printf.sprintf "/plates/%d" plate_id)
  in

  let unique_suffix =
    Printf.sprintf "%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.))
      (Random.int 10000)
  in

  (* Product by short_id *)
  let prod_req =
    Test_utils.json_post ~path:"/api/v1/products"
      ~body:
        (Printf.sprintf {|{"name": "Lookup Media %s", "description": "YPD"}|}
           unique_suffix)
  in
  let prod_res = Dream.test handler prod_req in
  let* prod_str = Dream.body prod_res in
  let prod_json = Yojson.Safe.from_string prod_str in
  let prod_short_id =
    Yojson.Safe.Util.(prod_json |> member "short_id" |> to_string)
  in
  let* () =
    assert_single_lookup ~query:prod_short_id ~expected_type:"product"
      ~expected_url:("/products/" ^ prod_short_id)
  in

  (* Result definition by short_id *)
  let def_req =
    Test_utils.json_post ~path:"/api/v1/result-definitions"
      ~body:
        (Printf.sprintf
           {|{"short_id": "def_%s", "name": "Lookup OD600 %s", "data_type": "Float"}|}
           unique_suffix unique_suffix)
  in
  let def_res = Dream.test handler def_req in
  let* def_str = Dream.body def_res in
  let def_json = Yojson.Safe.from_string def_str in
  let def_short_id =
    Yojson.Safe.Util.(def_json |> member "short_id" |> to_string)
  in
  let* () =
    assert_single_lookup ~query:def_short_id ~expected_type:"result_definition"
      ~expected_url:("/result-definitions/" ^ def_short_id)
  in

  (* Strain by exact strain_name and by uid *)
  let unique_strain_code = Printf.sprintf "ACC-%s" unique_suffix in
  let* strain_id, strain_uid = create_test_strain unique_strain_code in
  let* () =
    assert_single_lookup ~query:unique_strain_code ~expected_type:"strain"
      ~expected_url:(Printf.sprintf "/strains/%d" strain_id)
  in
  let* () =
    assert_single_lookup ~query:strain_uid ~expected_type:"strain"
      ~expected_url:(Printf.sprintf "/strains/%d" strain_id)
  in

  (* Community by uid *)
  let comm_req =
    Test_utils.json_post ~path:"/api/v1/communities"
      ~body:(Printf.sprintf {|{"name": "Lookup SynCom %s"}|} unique_strain_code)
  in
  let comm_res = Dream.test handler comm_req in
  let* comm_str = Dream.body comm_res in
  let comm_json = Yojson.Safe.from_string comm_str in
  let comm_id = Yojson.Safe.Util.(comm_json |> member "id" |> to_int) in
  let comm_uid = Yojson.Safe.Util.(comm_json |> member "uid" |> to_string) in
  let* () =
    assert_single_lookup ~query:comm_uid ~expected_type:"community"
      ~expected_url:(Printf.sprintf "/communities/%d" comm_id)
  in
  Lwt.return ()

let test_lookup_scope_and_validation _switch () =
  (* 1. Empty or missing q -> 400 Bad Request *)
  let empty_req = Test_utils.json_get ~path:"/api/v1/lookup?q=%20%20" in
  let empty_res = Dream.test handler empty_req in
  Alcotest.(check int)
    "Blank q should return 400" 400
    (Dream.status empty_res |> Dream.status_to_int);

  (* 2. Unknown scope -> 400 Bad Request *)
  let bad_scope_req =
    Test_utils.json_get ~path:"/api/v1/lookup?q=1&scope=not_a_scope"
  in
  let bad_scope_res = Dream.test handler bad_scope_req in
  Alcotest.(check int)
    "Invalid scope should return 400" 400
    (Dream.status bad_scope_res |> Dream.status_to_int);

  (* 3. Scoped lookup restricts to requested resource type *)
  let* project_id, project_short_id =
    create_test_project "Scoped Lookup Project"
  in
  let scoped_req =
    Test_utils.json_get
      ~path:(Printf.sprintf "/api/v1/lookup?q=%d&scope=project" project_id)
  in
  let scoped_res = Dream.test handler scoped_req in
  let* scoped_body = Dream.body scoped_res in
  Alcotest.(check int)
    "Scoped lookup returns 200" 200
    (Dream.status scoped_res |> Dream.status_to_int);
  let json = Yojson.Safe.from_string scoped_body in
  let matches = Yojson.Safe.Util.(json |> member "matches" |> to_list) in
  Alcotest.(check int) "Scoped lookup returns 1 match" 1 (List.length matches);
  let m = List.hd matches in
  Alcotest.(check string)
    "Scoped match is project" "project"
    Yojson.Safe.Util.(m |> member "resource_type" |> to_string);
  Alcotest.(check string)
    "Scoped match short_id" project_short_id
    Yojson.Safe.Util.(m |> member "short_id" |> to_string);

  (* Querying project short_id with scope=sample should return 0 matches *)
  let mismatch_req =
    Test_utils.json_get
      ~path:(Printf.sprintf "/api/v1/lookup?q=%s&scope=sample" project_short_id)
  in
  let mismatch_res = Dream.test handler mismatch_req in
  let* mismatch_body = Dream.body mismatch_res in
  let mismatch_json = Yojson.Safe.from_string mismatch_body in
  let mismatch_matches =
    Yojson.Safe.Util.(mismatch_json |> member "matches" |> to_list)
  in
  Alcotest.(check int)
    "Project short_id with scope=sample returns 0 matches" 0
    (List.length mismatch_matches);
  Lwt.return ()

let test_lookup_project_scope_isolation _switch () =
  let open Yojson.Safe.Util in
  let* proj_a_id, proj_a_short = create_test_project "Lookup Scope Proj A" in
  let* proj_b_id, proj_b_short = create_test_project "Lookup Scope Proj B" in
  let* strain_id, _ = create_test_strain "LookupScopeStrain" in

  let create_sample proj_id =
    let* res =
      Test_utils.assert_json_post ~handler
        ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
        ~body:
          (Printf.sprintf
             {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d}|}
             strain_id)
        ~expected_status:201 "Create sample"
    in
    let* body = Dream.body res in
    Lwt.return (Yojson.Safe.from_string body |> member "short_id" |> to_string)
  in
  let create_plate proj_id name =
    let* res =
      Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
        ~body:
          (Printf.sprintf
             {|{"name": %S, "project_id": %d, "plate_format": "96-well"}|} name
             proj_id)
        ~expected_status:201 ("Create " ^ name)
    in
    let* body = Dream.body res in
    Lwt.return (Yojson.Safe.from_string body |> member "short_id" |> to_string)
  in
  let* sample_a = create_sample proj_a_id in
  let* sample_b = create_sample proj_b_id in
  let* plate_a = create_plate proj_a_id "Lookup Scope Plate A" in
  let* plate_b = create_plate proj_b_id "Lookup Scope Plate B" in

  let* prod_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/products"
      ~body:
        (Printf.sprintf {|{"name": "Lookup Scope Media %f"}|}
           (Unix.gettimeofday ()))
      ~expected_status:201 "Create global product"
  in
  let* prod_body = Dream.body prod_res in
  let prod_short =
    Yojson.Safe.from_string prod_body |> member "short_id" |> to_string
  in

  let* assigned_a_email, assigned_a_app =
    Test_utils.create_scoped_user ~prefix:"lkp_assigned_a"
      ~role_str:"project_user"
  in
  let* _, unassigned_app =
    Test_utils.create_scoped_user ~prefix:"lkp_unassigned"
      ~role_str:"project_user"
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/users" proj_a_short)
      ~body:(Printf.sprintf {|{"email": %S}|} assigned_a_email)
      ~expected_status:204 "Assign user to Project A"
  in

  let count_matches app ?(scope = "all") query =
    let* res =
      Test_utils.assert_json_get ~handler:app
        ~path:
          (Printf.sprintf "/api/v1/lookup?q=%s&scope=%s" (Uri.pct_encode query)
             scope)
        ~expected_status:200
        (Printf.sprintf "Lookup %s (scope=%s)" query scope)
    in
    let* body = Dream.body res in
    let matches = Yojson.Safe.from_string body |> member "matches" |> to_list in
    Lwt.return (List.length matches)
  in

  (* Assigned Project A user sees Project A resources *)
  let* c_pa = count_matches assigned_a_app proj_a_short in
  let* c_sa = count_matches assigned_a_app sample_a in
  let* c_pla = count_matches assigned_a_app plate_a in
  Alcotest.(check int) "Assigned user finds Project A" 1 c_pa;
  Alcotest.(check int) "Assigned user finds Sample A" 1 c_sa;
  Alcotest.(check int) "Assigned user finds Plate A" 1 c_pla;

  (* Assigned Project A user does NOT see Project B resources *)
  let* c_pb = count_matches assigned_a_app proj_b_short in
  let* c_pb_scoped =
    count_matches assigned_a_app ~scope:"project" proj_b_short
  in
  let* c_sb = count_matches assigned_a_app sample_b in
  let* c_plb = count_matches assigned_a_app plate_b in
  Alcotest.(check int) "Assigned A user cannot see Project B" 0 c_pb;
  Alcotest.(check int)
    "Assigned A user cannot see Project B (scoped)" 0 c_pb_scoped;
  Alcotest.(check int) "Assigned A user cannot see Sample B" 0 c_sb;
  Alcotest.(check int) "Assigned A user cannot see Plate B" 0 c_plb;

  (* Unassigned user cannot see Project A resources, but can see global product *)
  let* c_unassigned_pa = count_matches unassigned_app proj_a_short in
  let* c_unassigned_prod = count_matches unassigned_app prod_short in
  Alcotest.(check int) "Unassigned user cannot see Project A" 0 c_unassigned_pa;
  Alcotest.(check int)
    "Unassigned user can still look up global product" 1 c_unassigned_prod;
  Lwt.return ()

let suite =
  [
    ( "Lookup API",
      [
        Alcotest_lwt.test_case "Lookup Sample by Short ID and Unknown ID" `Quick
          test_lookup_sample_short_id_and_unknown;
        Alcotest_lwt.test_case
          "Lookup Project, Plate, Product, Def, Strain, Community" `Quick
          test_lookup_all_resource_types;
        Alcotest_lwt.test_case "Lookup Scope Filtering and Validation" `Quick
          test_lookup_scope_and_validation;
        Alcotest_lwt.test_case "Lookup Respects Project Scope Permissions"
          `Quick test_lookup_project_scope_isolation;
      ] );
  ]
