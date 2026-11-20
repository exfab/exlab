open Exlab_server

let handler = Test_utils.admin_app

let test_transfer_map_basic _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "TM Proj %f" time in

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
  let _src_id =
    Yojson.Safe.Util.(member "id" (Yojson.Safe.from_string src_body) |> to_int)
  in
  let src_short_id =
    Yojson.Safe.Util.(
      member "short_id" (Yojson.Safe.from_string src_body) |> to_string)
  in

  (* 4. Create Source Plate via Bulk API *)
  let csv_src_body =
    Printf.sprintf "plate_name,well,sample_short_id\nS-Plate 1,A1,%s"
      src_short_id
  in
  let* src_plate_res =
    Test_utils.assert_csv_post ~handler
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%d/plates/bulk-csv?plate_format=96-well" proj_id)
      ~body:csv_src_body ~expected_status:201 "Create Source Plate"
  in
  let* src_plate_body = Dream.body src_plate_res in
  let src_plate_json = Yojson.Safe.from_string src_plate_body in
  let src_plate_id =
    Yojson.Safe.Util.(
      member "plates" src_plate_json
      |> to_list |> List.hd |> member "id" |> to_int)
  in

  (* 5. Plan Multiple Plates (This creates the Experimental samples) *)
  let plan_payload =
    Printf.sprintf
      {|{"source_sample_short_ids": ["%s"], "replicates": 2, "num_plates": 1, "plate_name_prefix": "P-Exp", "plate_format": "96-well", "reserved_wells": [], "strategy": "simple"}|}
      src_short_id
  in
  let* plan_res =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%d/plates/plan" proj_id)
      ~body:plan_payload ~expected_status:201 "Plan Plates"
  in
  let* plan_body = Dream.body plan_res in
  let plan_json = Yojson.Safe.from_string plan_body in
  let dest_plate_id =
    Yojson.Safe.Util.(
      member "plates" plan_json |> to_list |> List.hd |> member "id" |> to_int)
  in

  (* 6. Generate Transfer Map *)
  let map_payload =
    Printf.sprintf {|{"source_plate_id": %d, "destination_plate_ids": [%d]}|}
      src_plate_id dest_plate_id
  in
  let* map_res =
    Test_utils.assert_json_post ~handler
      ~path:
        (Printf.sprintf "/api/v1/projects/%d/plates/transfer-map?format=csv"
           proj_id)
      ~body:map_payload ~expected_status:200 "Generate Map"
  in
  let* map_body = Dream.body map_res in
  Printf.printf "TRANSFER MAP OUTPUT:\n%s\n" map_body;

  Alcotest.(check bool)
    "CSV contains data rows" true
    (String.length map_body > 100);

  Lwt.return ()

let test_transfer_map_rejects_cross_project_plates _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let create_project name =
    let* res =
      Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
        ~body:(Printf.sprintf {|{"name": %S}|} name)
        ~expected_status:201 ("Create " ^ name)
    in
    let* body = Dream.body res in
    let j = Yojson.Safe.from_string body in
    Lwt.return (j |> member "id" |> to_int, j |> member "short_id" |> to_string)
  in
  let* proj_a_id, proj_a_short = create_project "TM Scope Proj A" in
  let* proj_b_id, _proj_b_short = create_project "TM Scope Proj B" in

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
    Lwt.return (Yojson.Safe.from_string body |> member "id" |> to_int)
  in
  let* plate_a1 = create_plate proj_a_id "TM Plate A1" in
  let* plate_a2 = create_plate proj_a_id "TM Plate A2" in
  let* plate_b1 = create_plate proj_b_id "TM Plate B1" in

  let* assigned_a_email, assigned_a_app =
    Test_utils.create_scoped_user ~prefix:"tm_assigned_a"
      ~role_str:"project_user"
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/users" proj_a_short)
      ~body:(Printf.sprintf {|{"email": %S}|} assigned_a_email)
      ~expected_status:204 "Assign user to Project A"
  in

  let tm_path_a =
    Printf.sprintf "/api/v1/projects/%d/plates/transfer-map?format=csv"
      proj_a_id
  in

  (* Project A user trying to read Project B source or destination plate gets 403 *)
  let* _ =
    Test_utils.assert_json_post ~handler:assigned_a_app ~path:tm_path_a
      ~body:
        (Printf.sprintf
           {|{"source_plate_id": %d, "destination_plate_ids": [%d]}|} plate_b1
           plate_a2)
      ~expected_status:403
      "Project A user cannot use unauthorized Project B source plate"
  in
  let* _ =
    Test_utils.assert_json_post ~handler:assigned_a_app ~path:tm_path_a
      ~body:
        (Printf.sprintf
           {|{"source_plate_id": %d, "destination_plate_ids": [%d]}|} plate_a1
           plate_b1)
      ~expected_status:403
      "Project A user cannot use unauthorized Project B destination plate"
  in

  (* Admin trying to mix plates from Project B on Project A's endpoint gets 400 *)
  let* _ =
    Test_utils.assert_json_post ~handler ~path:tm_path_a
      ~body:
        (Printf.sprintf
           {|{"source_plate_id": %d, "destination_plate_ids": [%d]}|} plate_b1
           plate_a2)
      ~expected_status:400
      "Admin cannot use cross-project source plate on transfer map"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:tm_path_a
      ~body:
        (Printf.sprintf
           {|{"source_plate_id": %d, "destination_plate_ids": [%d]}|} plate_a1
           plate_b1)
      ~expected_status:400
      "Admin cannot use cross-project destination plate on transfer map"
  in

  (* Same-project plates succeed for assigned Project A user *)
  let* _ =
    Test_utils.assert_json_post ~handler:assigned_a_app ~path:tm_path_a
      ~body:
        (Printf.sprintf
           {|{"source_plate_id": %d, "destination_plate_ids": [%d]}|} plate_a1
           plate_a2)
      ~expected_status:200
      "Assigned Project A user can generate transfer map for Project A plates"
  in
  Lwt.return ()

let suite =
  [
    ( "Transfer Map API",
      [
        Alcotest_lwt.test_case "Basic Transfer Map" `Quick
          test_transfer_map_basic;
        Alcotest_lwt.test_case "Reject Cross-Project Plates in Transfer Map"
          `Quick test_transfer_map_rejects_cross_project_plates;
      ] );
  ]
