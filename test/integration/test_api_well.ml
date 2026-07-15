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

let suite =
  [
    ( "Well API",
      [
        Alcotest_lwt.test_case "Patch well successful" `Quick
          test_get_well_success;
        Alcotest_lwt.test_case "Reject Patch with Invalid JSON" `Quick
          test_patch_well_invalid_json;
      ] );
  ]
