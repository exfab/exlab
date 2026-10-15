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
      ] );
  ]
