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

let suite =
  [
    ( "Transfer Map API",
      [
        Alcotest_lwt.test_case "Basic Transfer Map" `Quick
          test_transfer_map_basic;
      ] );
  ]
