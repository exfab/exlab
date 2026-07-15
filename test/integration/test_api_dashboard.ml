open Exlab_server

let handler = Test_utils.admin_app

open Exlab_server

let handler = Test_utils.admin_app

let test_plate_dashboard_isolation _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Dashboard Proj %f" time in

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
           {|{"genus": "Test", "species": "strain", "strain_name": "S-%f", "links": []}|}
           time)
      ~expected_status:201 "Create Strain"
  in
  let* strain_body = Dream.body strain_res in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string strain_body) |> to_int)
  in

  (* 3. Create Source Sample *)
  let* src_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d}|}
           strain_id)
      ~expected_status:201 "Create Source"
  in
  let* src_body = Dream.body src_res in
  let src_id =
    Yojson.Safe.Util.(member "id" (Yojson.Safe.from_string src_body) |> to_int)
  in

  (* 4. Create 2 Experimental Samples *)
  let exp_body =
    Printf.sprintf
      {|{"sample_type": "Solid Cell Culture", "category": "Experimental", "parent_sample_id": %d}|}
      src_id
  in
  let* exp1_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:exp_body ~expected_status:201 "Create Exp 1"
  in
  let* exp2_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:exp_body ~expected_status:201 "Create Exp 2"
  in

  let* exp1_body_str = Dream.body exp1_res in
  let exp1_short =
    Yojson.Safe.Util.(
      member "short_id" (Yojson.Safe.from_string exp1_body_str) |> to_string)
  in
  let exp1_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string exp1_body_str) |> to_int)
  in

  let* exp2_body_str = Dream.body exp2_res in
  let exp2_short =
    Yojson.Safe.Util.(
      member "short_id" (Yojson.Safe.from_string exp2_body_str) |> to_string)
  in
  let exp2_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string exp2_body_str) |> to_int)
  in

  (* 5. Create 2 Plates via Bulk API and assign samples *)
  let csv_body =
    Printf.sprintf
      "plate_name,well,sample_short_id\nPlate 1,A1,%s\nPlate 2,A1,%s" exp1_short
      exp2_short
  in
  let* plates_res =
    Test_utils.assert_csv_post ~handler
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%d/plates/bulk-csv?plate_format=96-well" proj_id)
      ~body:csv_body ~expected_status:201 "Create Plates"
  in
  let* plates_body = Dream.body plates_res in
  let plates_json = Yojson.Safe.from_string plates_body in
  let plates_list = Yojson.Safe.Util.(member "plates" plates_json |> to_list) in

  let plate1_id =
    List.find
      (fun p -> Yojson.Safe.Util.(member "name" p |> to_string) = "Plate 1")
      plates_list
    |> fun p -> Yojson.Safe.Util.(member "id" p |> to_int)
  in
  let plate2_id =
    List.find
      (fun p -> Yojson.Safe.Util.(member "name" p |> to_string) = "Plate 2")
      plates_list
    |> fun p -> Yojson.Safe.Util.(member "id" p |> to_int)
  in

  (* 6. Create Result Definition *)
  let* def_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/result-definitions"
      ~body:
        (Printf.sprintf
           {|{"short_id": "test_def_%f", "name": "Test Def %f", "data_type": "Float"}|}
           time time)
      ~expected_status:201 "Create Def"
  in
  let* def_body = Dream.body def_res in
  let def_id =
    Yojson.Safe.Util.(member "id" (Yojson.Safe.from_string def_body) |> to_int)
  in

  (* 7. Assign Results to Samples *)
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/samples/%d/results" exp1_id)
      ~body:
        (Printf.sprintf
           {|{"result_definition_id": %d, "value": {"type": "Float", "value": 10.5}}|}
           def_id)
      ~expected_status:201 "Add Result 1"
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/samples/%d/results" exp2_id)
      ~body:
        (Printf.sprintf
           {|{"result_definition_id": %d, "value": {"type": "Float", "value": 20.5}}|}
           def_id)
      ~expected_status:201 "Add Result 2"
  in

  (* 8. Assign Results directly to Plates *)
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%d/results" plate1_id)
      ~body:
        (Printf.sprintf
           {|{"result_definition_id": %d, "value": {"type": "Float", "value": 100.5}}|}
           def_id)
      ~expected_status:201 "Add Plate Result 1"
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%d/results" plate2_id)
      ~body:
        (Printf.sprintf
           {|{"result_definition_id": %d, "value": {"type": "Float", "value": 200.5}}|}
           def_id)
      ~expected_status:201 "Add Plate Result 2"
  in

  (* 9. Fetch Plate 1 Dashboard *)
  let* dashboard_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%d/dashboard" plate1_id)
      ~expected_status:200 "Get Plate 1 Dashboard"
  in
  let* dashboard_body = Dream.body dashboard_res in
  Printf.printf "DASHBOARD JSON: %s\n" dashboard_body;
  let dashboard_json = Yojson.Safe.from_string dashboard_body in
  let data = Yojson.Safe.Util.(member "data" dashboard_json |> to_list) in

  (* 10. Verify Data *)
  (* Data should contain Sample 1 and Plate 1, but NOT Sample 2 or Plate 2 *)
  Alcotest.(check int)
    "Should have exactly 2 rows (1 sample, 1 plate)" 2 (List.length data);

  let get_short_id_opt item =
    Yojson.Safe.Util.(member "short_id" item |> to_string_option)
  in

  let has_exp1 =
    List.exists (fun item -> get_short_id_opt item = Some exp1_short) data
  in
  Alcotest.(check bool) "Contains Exp 1" true has_exp1;

  let has_exp2 =
    List.exists (fun item -> get_short_id_opt item = Some exp2_short) data
  in
  Alcotest.(check bool) "Does NOT contain Exp 2" false has_exp2;

  Lwt.return ()

let test_project_dashboard () =
  let req = Test_utils.json_get ~path:"/api/v1/projects/999/dashboard" in
  let response = Dream.test handler req in
  let status_code = Dream.status response |> Dream.status_to_int in
  Alcotest.(check int) "Should return 404 for missing project" 404 status_code;
  Lwt.return ()

let test_project_export_csv () =
  let req =
    Test_utils.json_get ~path:"/api/v1/projects/999/results/export?shape=long"
  in
  let response = Dream.test handler req in
  let status_code = Dream.status response |> Dream.status_to_int in
  Alcotest.(check int)
    "Should return 404 for missing project export" 404 status_code;
  Lwt.return ()

let test_sample_children_dashboard () =
  let req =
    Test_utils.json_get ~path:"/api/v1/samples/999999/children/dashboard"
  in
  let response = Dream.test handler req in
  let status_code = Dream.status response |> Dream.status_to_int in
  let body = Dream.body response |> Lwt_main.run in
  Printf.printf "GOT BODY: %s\n" body;
  Alcotest.(check int) "Should return 404 for missing sample" 404 status_code;
  Lwt.return ()

let suite =
  [
    ( "Dashboard API",
      [
        Alcotest_lwt.test_case "Project dashboard returns 404" `Quick
          (fun _ () -> test_project_dashboard ());
        Alcotest_lwt.test_case "Project results export returns 404" `Quick
          (fun _ () -> test_project_export_csv ());
        Alcotest_lwt.test_case "Sample children dashboard returns 404" `Quick
          (fun _ () -> test_sample_children_dashboard ());
        Alcotest_lwt.test_case "Plate dashboard isolates plate data correctly"
          `Quick test_plate_dashboard_isolation;
      ] );
  ]
