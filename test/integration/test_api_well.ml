(* Test Definitions for API endpoint /api/wells *)

let handler = Test_utils.admin_app

let test_get_well_success _switch () =
  let open Lwt.Syntax in
  (* First create a plate which will automatically generate wells *)
  let time = Unix.gettimeofday () in
  let plate_name = Printf.sprintf "Test Plate %f" time in
  let create_plate_body =
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
      ~body:create_plate_body ~expected_status:201 "Create plate successful"
  in
  let* create_body = Dream.body create_res in
  let json = Yojson.Safe.from_string create_body in
  let plate_id = Yojson.Safe.Util.(member "id" json |> to_int) in

  (* Fetch the plate to get its wells *)
  let plate_path = Printf.sprintf "/api/v1/plates/%d" plate_id in
  let* get_plate_res =
    Test_utils.assert_json_get ~handler ~path:plate_path ~expected_status:200
      "Fetch plate successful"
  in
  let* get_plate_body = Dream.body get_plate_res in
  let plate_json = Yojson.Safe.from_string get_plate_body in

  (* Extract the first well ID from the plate response *)
  let wells = Yojson.Safe.Util.(member "wells" plate_json |> to_list) in
  let first_well = List.hd wells in
  let well_obj = Yojson.Safe.Util.(member "well" first_well) in
  let _well_id = Yojson.Safe.Util.(member "id" well_obj |> to_int) in

  (* Patch well to assign it to a sample *)
  let patch_path = Printf.sprintf "/api/v1/plates/%d/wells/A1" plate_id in
  let patch_body = {| { "sample_id": null } |} in
  let* _ =
    Test_utils.assert_json_patch ~handler ~path:patch_path ~body:patch_body
      ~expected_status:200 "Patch well successful"
  in
  Lwt.return ()

let test_patch_well_invalid_json _switch () =
  let open Lwt.Syntax in
  let patch_path = "/api/v1/plates/1/wells/A1" in
  let patch_body = {| { "sample_id": "not-an-int" } |} in
  let* _ =
    Test_utils.assert_json_patch ~handler ~path:patch_path ~body:patch_body
      ~expected_status:400 "Patch with invalid JSON fails"
  in
  Lwt.return ()

let test_patch_well_project_scope_and_validation _switch () =
  let open Lwt.Syntax in
  let open Yojson.Safe.Util in
  let create_project name =
    let* res =
      Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
        ~body:(Printf.sprintf {|{"name": %S, "status": "Active"}|} name)
        ~expected_status:201 ("Create " ^ name)
    in
    let* s = Dream.body res in
    let j = Yojson.Safe.from_string s in
    Lwt.return (j |> member "id" |> to_int, j |> member "short_id" |> to_string)
  in
  let* proj_a_id, proj_a_short = create_project "Well Scope Project A" in
  let* proj_b_id, _proj_b_short = create_project "Well Scope Project B" in

  let* strain_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/strains"
      ~body:
        {|{"genus": "Escherichia", "species": "coli", "strain_name": "WellScopeK12", "links": []}|}
      ~expected_status:201 "Create strain"
  in
  let* strain_str = Dream.body strain_res in
  let strain_id = Yojson.Safe.from_string strain_str |> member "id" |> to_int in

  let create_source_sample proj_id =
    let* res =
      Test_utils.assert_json_post ~handler
        ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
        ~body:
          (Printf.sprintf
             {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d}|}
             strain_id)
        ~expected_status:201 "Create source sample"
    in
    let* s = Dream.body res in
    Lwt.return (Yojson.Safe.from_string s |> member "id" |> to_int)
  in
  let* sample_a_id = create_source_sample proj_a_id in
  let* sample_b_id = create_source_sample proj_b_id in

  let* plate_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Well Scope Plate A", "project_id": %d, "plate_format": "96-well"}|}
           proj_a_id)
      ~expected_status:201 "Create plate in Project A"
  in
  let* plate_str = Dream.body plate_res in
  let plate_json = Yojson.Safe.from_string plate_str in
  let plate_id = plate_json |> member "id" |> to_int in
  let plate_short = plate_json |> member "short_id" |> to_string in

  let* _, unassigned_user_app =
    Test_utils.create_scoped_user ~prefix:"well_unassigned"
      ~role_str:"project_user"
  in
  let* _, unassigned_pm_app =
    Test_utils.create_scoped_user ~prefix:"well_unassigned_pm"
      ~role_str:"project_manager"
  in
  let* assigned_a_email, assigned_a_app =
    Test_utils.create_scoped_user ~prefix:"well_assigned_a"
      ~role_str:"project_user"
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/users" proj_a_short)
      ~body:(Printf.sprintf {|{"email": %S}|} assigned_a_email)
      ~expected_status:204 "Assign user to Project A"
  in

  (* Unassigned users cannot patch well (by ID or short_id) *)
  let* _ =
    Test_utils.assert_json_patch ~handler:unassigned_user_app
      ~path:(Printf.sprintf "/api/v1/plates/%d/wells/A1" plate_id)
      ~body:{|{"sample_id": null}|} ~expected_status:403
      "Unassigned project_user cannot PATCH well"
  in
  let* _ =
    Test_utils.assert_json_patch ~handler:unassigned_pm_app
      ~path:(Printf.sprintf "/api/v1/plates/%s/wells/A1" plate_short)
      ~body:{|{"sample_id": null}|} ~expected_status:403
      "Unassigned project_manager cannot PATCH well"
  in

  (* Assigned Project A user can assign same-project sample using plate short_id *)
  let* _ =
    Test_utils.assert_json_patch ~handler:assigned_a_app
      ~path:(Printf.sprintf "/api/v1/plates/%s/wells/a1" plate_short)
      ~body:(Printf.sprintf {|{"sample_id": %d}|} sample_a_id)
      ~expected_status:200
      "Assigned project_user can assign same-project sample via short_id"
  in

  (* Cross-project sample assignment is rejected *)
  let* _ =
    Test_utils.assert_json_patch ~handler:assigned_a_app
      ~path:(Printf.sprintf "/api/v1/plates/%s/wells/A2" plate_short)
      ~body:(Printf.sprintf {|{"sample_id": %d}|} sample_b_id)
      ~expected_status:403
      "Project A user cannot assign unauthorized Project B sample"
  in
  let* _ =
    Test_utils.assert_json_patch ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%s/wells/A2" plate_short)
      ~body:(Printf.sprintf {|{"sample_id": %d}|} sample_b_id)
      ~expected_status:400 "Admin cannot assign cross-project sample to a plate"
  in

  (* Out-of-bounds well coordinate is rejected *)
  let* _ =
    Test_utils.assert_json_patch ~handler:assigned_a_app
      ~path:(Printf.sprintf "/api/v1/plates/%s/wells/Z99" plate_short)
      ~body:{|{"sample_id": null}|} ~expected_status:400
      "Out-of-bounds well coordinate returns 400"
  in
  Lwt.return ()

let suite =
  [
    ( "Well API",
      [
        Alcotest_lwt.test_case "Patch well successful" `Quick
          test_get_well_success;
        Alcotest_lwt.test_case "Reject Patch with Invalid JSON" `Quick
          test_patch_well_invalid_json;
        Alcotest_lwt.test_case "Well Patch Project Scope and Validation" `Quick
          test_patch_well_project_scope_and_validation;
      ] );
  ]
