let handler = Test_utils.admin_app

let test_generate_run_missing_fields _switch () =
  let req =
    Test_utils.json_post ~path:"/api/v1/projects/1/plates/plan"
      ~body:{|{"source_sample_short_ids": ["S-1"]}|}
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing fields" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_generate_run_missing_source _switch () =
  let req =
    Test_utils.json_post ~path:"/api/v1/projects/1/plates/plan"
      ~body:
        {|{"source_sample_short_ids": ["FAKE-1"], "replicates": 3, "num_plates": 2, "plate_name_prefix": "Test", "plate_format": "96-well", "reserved_wells": [], "strategy": "round_robin"}|}
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing source" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_generate_run_success _switch () =
  (* Setup Project *)
  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:{|{"name": "Run Gen Proj 1", "status": "Active"}|}
  in
  let res_proj = Dream.test handler req_proj in
  let body_proj = Dream.body res_proj |> Lwt_main.run in
  let proj_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_proj)
    |> Yojson.Safe.Util.to_int
  in

  (* Setup Source Samples *)
  let req_s1 =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": 1}|}
  in
  let _res_s1 = Dream.test handler req_s1 in
  let body_s1 = Dream.body _res_s1 |> Lwt_main.run in
  Dream.log "body_s1: %s" body_s1;
  let json_s1 = Yojson.Safe.from_string body_s1 in
  let short_s1 =
    match Yojson.Safe.Util.member "short_id" json_s1 with
    | `String s -> s
    | _ ->
        Yojson.Safe.Util.member "id" json_s1
        |> Yojson.Safe.Util.to_int |> string_of_int
  in

  let req_s2 =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": 1}|}
  in
  let _res_s2 = Dream.test handler req_s2 in
  let body_s2 = Dream.body _res_s2 |> Lwt_main.run in
  let json_s2 = Yojson.Safe.from_string body_s2 in
  let short_s2 =
    match Yojson.Safe.Util.member "short_id" json_s2 with
    | `String s -> s
    | _ ->
        Yojson.Safe.Util.member "id" json_s2
        |> Yojson.Safe.Util.to_int |> string_of_int
  in

  (* Generate Run: 2 sources, 3 replicates = 6 samples. Format 24-well. Reserve 19 wells.
     Usable wells per plate = 24 - 19 = 5.
     We need 6 samples, so it should generate 2 plates. *)
  let reserved_wells =
    [
      "A1";
      "A2";
      "A3";
      "A4";
      "A5";
      "A6";
      "B1";
      "B2";
      "B3";
      "B4";
      "B5";
      "B6";
      "C1";
      "C2";
      "C3";
      "C4";
      "C5";
      "C6";
      "D1";
    ]
  in
  let reserved_json =
    `List (List.map (fun s -> `String s) reserved_wells)
    |> Yojson.Safe.to_string
  in
  let payload =
    Printf.sprintf
      {|{"source_sample_short_ids": ["%s", "%s"], "replicates": 3, "num_plates": 2, "plate_name_prefix": "Run Alpha", "plate_format": "24-well", "reserved_wells": %s, "strategy": "round_robin"}|}
      short_s1 short_s2 reserved_json
  in

  let req_run =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%d/plates/plan" proj_id)
      ~body:payload
  in
  let res_run = Dream.test handler req_run in
  let body_run = Dream.body res_run |> Lwt_main.run in
  Dream.log "Response: %s" body_run;
  Alcotest.(check int)
    "Should return 201 Created" 201
    (Dream.status res_run |> Dream.status_to_int);

  let json_run = Yojson.Safe.from_string body_run in
  let plates =
    Yojson.Safe.Util.member "plates" json_run |> Yojson.Safe.Util.to_list
  in
  Alcotest.(check int) "Should create exactly 2 plates" 2 (List.length plates);

  Lwt.return ()

let suite =
  [
    ( "Multi-Plate Planner API",
      [
        Alcotest_lwt.test_case "Missing Fields" `Quick
          test_generate_run_missing_fields;
        Alcotest_lwt.test_case "Missing Source" `Quick
          test_generate_run_missing_source;
        Alcotest_lwt.test_case "Success Generate Run" `Quick
          test_generate_run_success;
      ] );
  ]
