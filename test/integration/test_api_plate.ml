(* Test Definitions for API endpoint /api/plates *)

let handler = Test_utils.admin_app

let test_create_malformed_json _switch () =
  let body = {| {"name": "Broken JSON", |} in
  let req = Test_utils.json_post ~path:"/api/v1/plates" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for bad JSON" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_create_missing_fields _switch () =
  let body = {| {"name": "Test Plate"} |} in
  let req = Test_utils.json_post ~path:"/api/v1/plates" ~body in
  let response = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing fields" 400
    (Dream.status response |> Dream.status_to_int);
  Lwt.return ()

let test_plate_lifecycle _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let plate_name = Printf.sprintf "Test Plate LC %f" time in

  (* 1. Create Plate *)
  let create_body =
    Printf.sprintf
      {| {
    "name": "%s",
    "project_id": 1,
    "plate_format": "96-well"
  } |}
      plate_name
  in
  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:create_body ~expected_status:201 "Create plate successful"
  in
  let* create_body_str = Dream.body create_res in
  let json = Yojson.Safe.from_string create_body_str in
  let plate_id = Yojson.Safe.Util.(member "id" json |> to_int) in

  (* 2. Fetch Plate *)
  let path = Printf.sprintf "/api/v1/plates/%d" plate_id in
  let* _ =
    Test_utils.assert_json_get ~handler ~path ~expected_status:200
      "Fetch plate successful"
  in

  (* 3. Fetch All Plates *)
  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/plates"
      ~expected_status:200 "Fetch all plates"
  in
  let* _ =
    Test_utils.assert_json_get ~handler ~path:"/api/v1/projects/1/plates"
      ~expected_status:200 "Fetch project plates"
  in

  (* 4. Update Plate - Not yet implemented in routes, skipping *)

  (* 5. Export Plates *)
  let export_path = Printf.sprintf "/api/v1/plates/%d/export" plate_id in
  let* export_res =
    Test_utils.assert_json_get ~handler ~path:export_path ~expected_status:200
      "Export successful"
  in
  let headers = Dream.all_headers export_res in
  let content_type_headers =
    List.filter_map
      (fun (k, v) ->
        if String.lowercase_ascii k = "content-type" then Some v else None)
      headers
  in
  Alcotest.(check bool)
    "Has CSV Content-Type" true
    (List.exists (fun h -> h = "text/csv") content_type_headers);

  (* 6. Archive Plate - Not yet implemented, skipping *)
  Lwt.return ()

(* Testcase executes inside Alcotest_lwt's outer Lwt_main.run, whereas
   Test_utils.assert_json_* calls Dream.test (which invokes its own nested
   Lwt_main.run). Direct Storage promises must be driven via Lwt_main.run so
   the outer Alcotest_lwt loop never yields across test cases. *)
let test_concurrent_plate_short_ids _switch () =
  let open Lwt.Syntax in
  let unwrap msg = function
    | Ok v -> v
    | Error _ -> Alcotest.fail ("Unexpected storage error in " ^ msg)
  in
  let create_plate proj_id category idx =
    let name = Printf.sprintf "%s Plate %d" category idx in
    let* res =
      Exlab_storage.Plate.add ~category ~name ~project_id:proj_id
        ~product_id:None ~plate_format:Exlab_core.Types.Well_96 ()
    in
    match res with
    | Ok p -> Lwt.return p.short_id
    | Error _ -> Alcotest.fail "Concurrent Plate.add failed (race collision)"
  in

  let proj, created_exp, created_src =
    Lwt_main.run
      (let* proj_res =
         Exlab_storage.Project.add ~name:"Concurrent Plate Project"
           ~description:None ~status:Exlab_core.Types.Active ~contact_name:None
           ~owner:None ()
       in
       let proj = unwrap "Project.add" proj_res in
       let* created_exp =
         Lwt_list.map_p
           (create_plate proj.id "Experimental")
           (List.init 8 (fun i -> i + 1))
       in
       let* created_src =
         Lwt_list.map_p
           (create_plate proj.id "Source")
           (List.init 8 (fun i -> i + 1))
       in
       Lwt.return (proj, created_exp, created_src))
  in

  let exp_ids = List.sort String.compare created_exp in
  let expected_exp_ids =
    List.init 8 (fun i -> Printf.sprintf "P-%d-%04d" proj.id (i + 1))
  in
  Alcotest.(check (list string))
    "Concurrent Experimental plates get distinct consecutive short IDs"
    expected_exp_ids exp_ids;

  let src_ids = List.sort String.compare created_src in
  let expected_src_ids =
    List.init 8 (fun i -> Printf.sprintf "S-%d-%04d" proj.id (i + 1))
  in
  Alcotest.(check (list string))
    "Concurrent Source plates get distinct consecutive short IDs"
    expected_src_ids src_ids;
  Lwt.return ()

let test_archive_plate_soft_delete _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        {|{"name": "Archive Target Plate", "project_id": 1, "plate_format": "96-well"}|}
      ~expected_status:201 "Create plate for archive test"
  in
  let* create_str = Dream.body create_res in
  let create_json = Yojson.Safe.from_string create_str in
  let short_id = create_json |> member "short_id" |> to_string in
  Alcotest.(check string)
    "Newly created plate defaults to Active status" "Active"
    (create_json |> member "status" |> to_string);

  let plate_path = Printf.sprintf "/api/v1/plates/%s" short_id in
  let* _ =
    Test_utils.assert_json_delete ~handler ~path:plate_path ~expected_status:204
      "DELETE plate soft-archives with 204"
  in
  let* get_res =
    Test_utils.assert_json_get ~handler ~path:plate_path ~expected_status:200
      "GET archived plate still resolves by short_id"
  in
  let* get_str = Dream.body get_res in
  let archived_status =
    Yojson.Safe.from_string get_str
    |> member "plate" |> member "status" |> to_string
  in
  Alcotest.(check string)
    "Archived plate has status = Archived" "Archived" archived_status;

  let* _ =
    Test_utils.assert_json_delete ~handler
      ~path:"/api/v1/plates/PLT-NONEXISTENT-999" ~expected_status:404
      "DELETE unknown plate returns 404"
  in
  Lwt.return ()

let test_archived_plate_downstream_isolation _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Archived Plate Isolation Project", "status": "Active"}|}
      ~expected_status:201 "Create project"
  in
  let* proj_str = Dream.body proj_res in
  let proj_json = Yojson.Safe.from_string proj_str in
  let proj_id = proj_json |> member "id" |> to_int in
  let proj_short = proj_json |> member "short_id" |> to_string in

  (* Create strain + source + experimental sample *)
  let strain_tag =
    Printf.sprintf "IsoStrain_%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.int 9999)
  in
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Escherichia", "species": "coli", "strain_name": %S}|}
           strain_tag)
      ~expected_status:201 "Create strain"
  in
  let* strain_str = Dream.body strain_res in
  let strain_id = Yojson.Safe.from_string strain_str |> member "id" |> to_int in
  let* src_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/samples" proj_short)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": []}|}
           strain_id)
      ~expected_status:201 "Create source sample"
  in
  let* src_str = Dream.body src_res in
  let src_id = Yojson.Safe.from_string src_str |> member "id" |> to_int in
  let* exp_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/samples" proj_short)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Experimental", "parent_sample_id": %d, "result_definition_ids": []}|}
           src_id)
      ~expected_status:201 "Create experimental sample"
  in
  let* exp_str = Dream.body exp_res in
  let exp_json = Yojson.Safe.from_string exp_str in
  let exp_id = exp_json |> member "id" |> to_int in
  let exp_short = exp_json |> member "short_id" |> to_string in

  (* Create Plate Alpha #1, assign sample to A1, and attach a plate-level result *)
  let* p1_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Plate Alpha", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
      ~expected_status:201 "Create Plate Alpha #1"
  in
  let* p1_str = Dream.body p1_res in
  let p1_json = Yojson.Safe.from_string p1_str in
  let p1_id = p1_json |> member "id" |> to_int in
  let p1_short = p1_json |> member "short_id" |> to_string in

  let* _ =
    Test_utils.assert_json_patch ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%d/wells/A1" p1_id)
      ~body:(Printf.sprintf {|{"sample_id": %d}|} exp_id)
      ~expected_status:200 "Assign sample to A1 on Plate Alpha #1"
  in

  let def_tag =
    Printf.sprintf "PLT_ISO_%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.int 9999)
  in
  let* def_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/result-definitions"
      ~body:
        (Printf.sprintf
           {|{"name": %S, "short_id": %S, "data_type": "String", "is_required": false}|}
           def_tag def_tag)
      ~expected_status:201 "Create result definition"
  in
  let* def_str = Dream.body def_res in
  let def_id = Yojson.Safe.from_string def_str |> member "id" |> to_int in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%d/results" p1_id)
      ~body:
        (Printf.sprintf
           {|{"result_definition_id": %d, "value": {"type": "String", "value": "BadRun"}}|}
           def_id)
      ~expected_status:201 "Attach plate result to Plate Alpha #1"
  in

  (* Archive Plate Alpha #1 *)
  let* _ =
    Test_utils.assert_json_delete ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%s" p1_short)
      ~expected_status:204 "Archive Plate Alpha #1"
  in

  (* 1. Sample locations must exclude archived Plate Alpha #1 *)
  let* loc_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/samples/%d/locations" exp_id)
      ~expected_status:200 "GET sample locations"
  in
  let* loc_str = Dream.body loc_res in
  let locs = Yojson.Safe.from_string loc_str |> member "data" |> to_list in
  Alcotest.(check int)
    "Archived plate excluded from sample locations" 0 (List.length locs);

  (* 2. Project dashboard must exclude archived plate well & plate-level data *)
  let* dash_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/dashboard" proj_short)
      ~expected_status:200 "GET project dashboard"
  in
  let* dash_str = Dream.body dash_res in
  Alcotest.(check bool)
    "Archived plate short_id does not appear in project dashboard" false
    (Re.execp (Re.compile (Re.str p1_short)) dash_str);
  Alcotest.(check bool)
    "Archived plate result value does not appear in project dashboard" false
    (Re.execp (Re.compile (Re.str "BadRun")) dash_str);

  (* 3. Creating replacement 'Plate Alpha' #2 and PATCHing by plate_name succeeds without ambiguity *)
  let* p2_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Plate Alpha", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
      ~expected_status:201 "Create replacement Plate Alpha #2"
  in
  let* p2_str = Dream.body p2_res in
  let p2_id = Yojson.Safe.from_string p2_str |> member "id" |> to_int in

  let patch_csv =
    Printf.sprintf "plate_name,well,sample_short_id\nPlate Alpha,B2,%s\n"
      exp_short
  in
  let patch_req =
    Test_utils.csv_patch
      ~path:(Printf.sprintf "/api/v1/projects/%s/plates/bulk-csv" proj_short)
      ~body:patch_csv
  in
  let patch_res = Dream.test handler patch_req in
  Alcotest.(check int)
    "Bulk layout PATCH by name resolves active Plate Alpha #2 without ambiguity"
    200
    (Dream.status patch_res |> Dream.status_to_int);

  let* loc_res2 =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/samples/%d/locations" exp_id)
      ~expected_status:200 "GET sample locations after replacement"
  in
  let* loc_str2 = Dream.body loc_res2 in
  let locs2 = Yojson.Safe.from_string loc_str2 |> member "data" |> to_list in
  Alcotest.(check int) "Active plate location shown" 1 (List.length locs2);
  Alcotest.(check int)
    "Location points to replacement Plate Alpha #2" p2_id
    (List.hd locs2 |> member "plate_id" |> to_int);
  Alcotest.(check string)
    "Location well_coordinate is B2" "B2"
    (List.hd locs2 |> member "well_coordinate" |> to_string);
  Lwt.return ()

let test_update_plate_rename_and_restore _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let* create_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        {|{"name": "Initial Plate Name", "project_id": 1, "plate_format": "96-well"}|}
      ~expected_status:201 "Create plate for update test"
  in
  let* create_str = Dream.body create_res in
  let short_id =
    Yojson.Safe.from_string create_str |> member "short_id" |> to_string
  in
  let plate_path = Printf.sprintf "/api/v1/plates/%s" short_id in

  let* _ =
    Test_utils.assert_json_delete ~handler ~path:plate_path ~expected_status:204
      "Archive plate"
  in
  let* put_res =
    Test_utils.assert_json_put ~handler ~path:plate_path
      ~body:{|{"name": "Restored Plate Name", "status": "Active"}|}
      ~expected_status:200 "PUT plate renames and restores status to Active"
  in
  let* put_str = Dream.body put_res in
  let put_json = Yojson.Safe.from_string put_str in
  Alcotest.(check string)
    "Updated name matches" "Restored Plate Name"
    (put_json |> member "name" |> to_string);
  Alcotest.(check string)
    "Updated status is Active" "Active"
    (put_json |> member "status" |> to_string);

  let* _ =
    Test_utils.assert_json_put ~handler ~path:plate_path
      ~body:{|{"name": "   ", "status": "Active"}|} ~expected_status:400
      "PUT rejects empty plate name"
  in
  Lwt.return ()

let test_permanent_delete_plate_cascades_and_reclaims_ids _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Reclaim Plate IDs Project", "status": "Active"}|}
      ~expected_status:201 "Create project"
  in
  let* proj_str = Dream.body proj_res in
  let proj_json = Yojson.Safe.from_string proj_str in
  let proj_id = proj_json |> member "id" |> to_int in
  let proj_short = proj_json |> member "short_id" |> to_string in

  (* Create strain + source sample + experimental sample *)
  let strain_tag =
    Printf.sprintf "ReclaimStrain_%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.int 9999)
  in
  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        (Printf.sprintf
           {|{"genus": "Bacillus", "species": "subtilis", "strain_name": %S}|}
           strain_tag)
      ~expected_status:201 "Create strain"
  in
  let* strain_str = Dream.body strain_res in
  let strain_id = Yojson.Safe.from_string strain_str |> member "id" |> to_int in
  let* src_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/samples" proj_short)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d, "result_definition_ids": []}|}
           strain_id)
      ~expected_status:201 "Create source sample"
  in
  let* src_str = Dream.body src_res in
  let src_id = Yojson.Safe.from_string src_str |> member "id" |> to_int in
  let* exp_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/samples" proj_short)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Experimental", "parent_sample_id": %d, "result_definition_ids": []}|}
           src_id)
      ~expected_status:201 "Create experimental sample"
  in
  let* exp_str = Dream.body exp_res in
  let exp_id = Yojson.Safe.from_string exp_str |> member "id" |> to_int in

  (* Create Source plate S-<proj>-0001 *)
  let* s1_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Source 1", "project_id": %d, "plate_format": "96-well", "category": "Source"}|}
           proj_id)
      ~expected_status:201 "Create Source plate 1"
  in
  let* s1_str = Dream.body s1_res in
  Alcotest.(check string)
    "Source plate is S-<proj>-0001"
    (Printf.sprintf "S-%d-0001" proj_id)
    (Yojson.Safe.from_string s1_str |> member "short_id" |> to_string);

  (* Create Experimental plates P-<proj>-0001 and P-<proj>-0002 *)
  let create_exp_plate label =
    let* res =
      Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
        ~body:
          (Printf.sprintf
             {|{"name": %S, "project_id": %d, "plate_format": "96-well"}|} label
             proj_id)
        ~expected_status:201 ("Create " ^ label)
    in
    let* s = Dream.body res in
    Lwt.return (Yojson.Safe.from_string s)
  in
  let* p1_json = create_exp_plate "Exp 1" in
  let p1_short = p1_json |> member "short_id" |> to_string in
  let* p2_json = create_exp_plate "Exp 2" in
  let p2_id = p2_json |> member "id" |> to_int in
  let p2_short = p2_json |> member "short_id" |> to_string in
  Alcotest.(check string)
    "Second plate is P-<proj>-0002"
    (Printf.sprintf "P-%d-0002" proj_id)
    p2_short;

  (* Assign sample + plate-level result value to P-<proj>-0002 *)
  let* _ =
    Test_utils.assert_json_patch ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%d/wells/A1" p2_id)
      ~body:(Printf.sprintf {|{"sample_id": %d}|} exp_id)
      ~expected_status:200 "Assign sample to P2 A1"
  in
  let def_tag =
    Printf.sprintf "PLT_DEL_%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.int 9999)
  in
  let* def_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/result-definitions"
      ~body:
        (Printf.sprintf
           {|{"name": %S, "short_id": %S, "data_type": "String", "is_required": false}|}
           def_tag def_tag)
      ~expected_status:201 "Create result definition"
  in
  let* def_str = Dream.body def_res in
  let def_id = Yojson.Safe.from_string def_str |> member "id" |> to_int in
  let* rv_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%d/results" p2_id)
      ~body:
        (Printf.sprintf
           {|{"result_definition_id": %d, "value": {"type": "String", "value": "JunkValue"}}|}
           def_id)
      ~expected_status:201 "Attach plate data to P2"
  in
  let* rv_str = Dream.body rv_res in
  let rv_id = Yojson.Safe.from_string rv_str |> member "id" |> to_int in

  (* Permanently delete trailing junk plate P-<proj>-0002 *)
  let* _ =
    Test_utils.assert_json_delete ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%s?permanent=true" p2_short)
      ~expected_status:204 "Permanent DELETE P2 returns 204"
  in
  let* _ =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%s" p2_short)
      ~expected_status:404 "Permanently deleted P2 returns 404"
  in
  let* proj_res_export =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/dashboard" proj_short)
      ~expected_status:200 "Dashboard after permanent delete"
  in
  let* dash_str = Dream.body proj_res_export in
  Alcotest.(check bool)
    "Deleted plate's result_value was cascaded" false
    (Re.execp (Re.compile (Re.str "JunkValue")) dash_str);
  let* _ =
    Test_utils.assert_json_put ~handler
      ~path:(Printf.sprintf "/api/v1/results/%d" rv_id)
      ~body:{|{"value": {"type": "String", "value": "Nope"}}|}
      ~expected_status:404 "Plate result_value row was cascaded and deleted"
  in
  let* _ =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/samples/%d" exp_id)
      ~expected_status:200 "Assigned sample itself remains intact"
  in

  (* Next Experimental plate reclaims P-<proj>-0002 *)
  let* p2_reclaimed = create_exp_plate "Exp 2 Reclaimed" in
  let p2_reclaimed_short = p2_reclaimed |> member "short_id" |> to_string in
  Alcotest.(check string)
    "Next plate reclaims P-<proj>-0002"
    (Printf.sprintf "P-%d-0002" proj_id)
    p2_reclaimed_short;

  (* Permanently delete BOTH P-<proj>-0001 then P-<proj>-0002 -> resets to P-<proj>-0001 *)
  let* _ =
    Test_utils.assert_json_delete ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%s?permanent=true" p1_short)
      ~expected_status:204 "Permanent DELETE P1"
  in
  let* _ =
    Test_utils.assert_json_delete ~handler
      ~path:
        (Printf.sprintf "/api/v1/plates/%s?permanent=true" p2_reclaimed_short)
      ~expected_status:204 "Permanent DELETE P2 Reclaimed"
  in
  let* p_fresh = create_exp_plate "Fresh Exp 1" in
  Alcotest.(check string)
    "After deleting all experimental plates, numbering restarts at \
     P-<proj>-0001"
    (Printf.sprintf "P-%d-0001" proj_id)
    (p_fresh |> member "short_id" |> to_string);

  (* Source plate counter was completely unaffected -> next Source plate is S-<proj>-0002 *)
  let* s2_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Source 2", "project_id": %d, "plate_format": "96-well", "category": "Source"}|}
           proj_id)
      ~expected_status:201 "Create Source plate 2"
  in
  let* s2_str = Dream.body s2_res in
  Alcotest.(check string)
    "Source plate counter unaffected (S-<proj>-0002)"
    (Printf.sprintf "S-%d-0002" proj_id)
    (Yojson.Safe.from_string s2_str |> member "short_id" |> to_string);
  Lwt.return ()

let test_plate_archive_and_delete_project_scope_permissions _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let create_user ~prefix ~role_str =
    Test_utils.create_scoped_user ~prefix:("plt_" ^ prefix) ~role_str
  in
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Plate Scope Permissions Project", "status": "Active"}|}
      ~expected_status:201 "Create project"
  in
  let* proj_str = Dream.body proj_res in
  let proj_json = Yojson.Safe.from_string proj_str in
  let proj_id = proj_json |> member "id" |> to_int in
  let proj_short = proj_json |> member "short_id" |> to_string in

  let mk_plate name =
    let* res =
      Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
        ~body:
          (Printf.sprintf
             {|{"name": %S, "project_id": %d, "plate_format": "96-well"}|} name
             proj_id)
        ~expected_status:201 ("Create " ^ name)
    in
    let* s = Dream.body res in
    Lwt.return (Yojson.Safe.from_string s |> member "short_id" |> to_string)
  in
  let* p1 = mk_plate "Perm P1" in
  let* p2 = mk_plate "Perm P2" in
  let* p3 = mk_plate "Perm P3" in
  let* p4 = mk_plate "Perm P4" in

  let* _u_email, unassigned_user_app =
    create_user ~prefix:"unassigned_pu" ~role_str:"project_user"
  in
  let* _upm_email, unassigned_pm_app =
    create_user ~prefix:"unassigned_pm" ~role_str:"project_manager"
  in
  let* assigned_u_email, assigned_user_app =
    create_user ~prefix:"assigned_pu" ~role_str:"project_user"
  in
  let* assigned_pm_email, assigned_pm_app =
    create_user ~prefix:"assigned_pm" ~role_str:"project_manager"
  in
  let* _lm_email, lab_mgr_app =
    create_user ~prefix:"lab_mgr" ~role_str:"lab_manager"
  in

  let assign email =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/users" proj_short)
      ~body:(Printf.sprintf {|{"email": %S}|} email)
      ~expected_status:204 ("Assign " ^ email)
  in
  let* _ = assign assigned_u_email in
  let* _ = assign assigned_pm_email in

  (* Unassigned project_user and unassigned project_manager get 403 *)
  let* _ =
    Test_utils.assert_json_delete ~handler:unassigned_user_app
      ~path:(Printf.sprintf "/api/v1/plates/%s" p1)
      ~expected_status:403 "Unassigned project_user cannot archive plate"
  in
  let* _ =
    Test_utils.assert_json_delete ~handler:unassigned_user_app
      ~path:(Printf.sprintf "/api/v1/plates/%s?permanent=true" p1)
      ~expected_status:403
      "Unassigned project_user cannot permanently delete plate"
  in
  let* _ =
    Test_utils.assert_json_put ~handler:unassigned_user_app
      ~path:(Printf.sprintf "/api/v1/plates/%s" p1)
      ~body:{|{"name": "Hack"}|} ~expected_status:403
      "Unassigned project_user cannot PUT plate"
  in
  let* _ =
    Test_utils.assert_json_delete ~handler:unassigned_pm_app
      ~path:(Printf.sprintf "/api/v1/plates/%s?permanent=true" p1)
      ~expected_status:403
      "Unassigned project_manager cannot permanently delete plate"
  in

  (* Assigned project_user, assigned project_manager, and global lab_manager succeed *)
  let* _ =
    Test_utils.assert_json_delete ~handler:assigned_user_app
      ~path:(Printf.sprintf "/api/v1/plates/%s" p1)
      ~expected_status:204 "Assigned project_user can archive plate"
  in
  let* _ =
    Test_utils.assert_json_delete ~handler:assigned_user_app
      ~path:(Printf.sprintf "/api/v1/plates/%s?permanent=true" p2)
      ~expected_status:204 "Assigned project_user can permanently delete plate"
  in
  let* _ =
    Test_utils.assert_json_delete ~handler:assigned_pm_app
      ~path:(Printf.sprintf "/api/v1/plates/%s?permanent=true" p3)
      ~expected_status:204
      "Assigned project_manager can permanently delete plate"
  in
  let* _ =
    Test_utils.assert_json_delete ~handler:lab_mgr_app
      ~path:(Printf.sprintf "/api/v1/plates/%s?permanent=true" p4)
      ~expected_status:204
      "Lab_manager can permanently delete plate on any project"
  in
  Lwt.return ()

let test_unassign_bulk_invalid_well_returns_400 _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Unassign Bulk Validation Proj", "status": "Active"}|}
      ~expected_status:201 "Create project"
  in
  let* proj_str = Dream.body proj_res in
  let proj_id = Yojson.Safe.from_string proj_str |> member "id" |> to_int in
  let* plate_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Unassign Plate", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
      ~expected_status:201 "Create plate"
  in
  let* plate_str = Dream.body plate_res in
  let plate_short =
    Yojson.Safe.from_string plate_str |> member "short_id" |> to_string
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%s/unassign-bulk" plate_short)
      ~body:{|{"wells": ["A1", "Z99"]}|} ~expected_status:400
      "Out-of-bounds well in unassign-bulk returns 400 Bad Request"
  in
  Lwt.return ()

let test_export_plate_project_scope_permissions _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Export Plate Scope Proj", "status": "Active"}|}
      ~expected_status:201 "Create project"
  in
  let* proj_str = Dream.body proj_res in
  let proj_json = Yojson.Safe.from_string proj_str in
  let proj_id = proj_json |> member "id" |> to_int in
  let proj_short = proj_json |> member "short_id" |> to_string in

  let* plate_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Export Scope Plate", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
      ~expected_status:201 "Create plate"
  in
  let* plate_str = Dream.body plate_res in
  let plate_short =
    Yojson.Safe.from_string plate_str |> member "short_id" |> to_string
  in

  let* _, unassigned_user_app =
    Test_utils.create_scoped_user ~prefix:"exp_unassigned"
      ~role_str:"project_user"
  in
  let* _, unassigned_pm_app =
    Test_utils.create_scoped_user ~prefix:"exp_unassigned_pm"
      ~role_str:"project_manager"
  in
  let* assigned_email, assigned_user_app =
    Test_utils.create_scoped_user ~prefix:"exp_assigned"
      ~role_str:"project_user"
  in
  let* _, lab_mgr_app =
    Test_utils.create_scoped_user ~prefix:"exp_lab_mgr" ~role_str:"lab_manager"
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/users" proj_short)
      ~body:(Printf.sprintf {|{"email": %S}|} assigned_email)
      ~expected_status:204 "Assign user to project"
  in

  let export_path = Printf.sprintf "/api/v1/plates/%s/export" plate_short in
  let* _ =
    Test_utils.assert_json_get ~handler:unassigned_user_app ~path:export_path
      ~expected_status:403 "Unassigned project_user cannot export plate CSV"
  in
  let* _ =
    Test_utils.assert_json_get ~handler:unassigned_pm_app ~path:export_path
      ~expected_status:403 "Unassigned project_manager cannot export plate CSV"
  in
  let* _ =
    Test_utils.assert_json_get ~handler:assigned_user_app ~path:export_path
      ~expected_status:200 "Assigned project_user can export plate CSV"
  in
  let* _ =
    Test_utils.assert_json_get ~handler:lab_mgr_app ~path:export_path
      ~expected_status:200 "Lab_manager can export plate CSV"
  in
  Lwt.return ()

let suite =
  [
    ( "Plate API",
      [
        Alcotest_lwt.test_case "Plate Lifecycle" `Quick test_plate_lifecycle;
        Alcotest_lwt.test_case "Reject Malformed JSON" `Quick
          test_create_malformed_json;
        Alcotest_lwt.test_case "Reject Missing Fields" `Quick
          test_create_missing_fields;
        Alcotest_lwt.test_case "Concurrent Plate Short IDs" `Quick
          test_concurrent_plate_short_ids;
        Alcotest_lwt.test_case "Archive Plate Soft Delete" `Quick
          test_archive_plate_soft_delete;
        Alcotest_lwt.test_case "Archived Plate Downstream Isolation" `Quick
          test_archived_plate_downstream_isolation;
        Alcotest_lwt.test_case "Update Plate Rename and Restore" `Quick
          test_update_plate_rename_and_restore;
        Alcotest_lwt.test_case
          "Permanent Delete Plate Cascades and Reclaims IDs" `Quick
          test_permanent_delete_plate_cascades_and_reclaims_ids;
        Alcotest_lwt.test_case
          "Plate Archive and Delete Project Scope Permissions" `Quick
          test_plate_archive_and_delete_project_scope_permissions;
        Alcotest_lwt.test_case "Unassign Bulk Rejects Out of Bounds Well" `Quick
          test_unassign_bulk_invalid_well_returns_400;
        Alcotest_lwt.test_case "Export Plate Project Scope Permissions" `Quick
          test_export_plate_project_scope_permissions;
      ] );
  ]
