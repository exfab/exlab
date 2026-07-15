let handler = Test_utils.admin_app

let string_includes ~substring s =
  let len = String.length substring in
  let max_idx = String.length s - len in
  let rec check idx =
    if idx > max_idx then false
    else if String.sub s idx len = substring then true
    else check (idx + 1)
  in
  check 0

let test_autofill_strategy_failure _switch () =
  (* Setup Project *)
  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:{|{"name": "AutoFill Proj 1", "status": "Active"}|}
  in
  let res_proj = Dream.test handler req_proj in
  let body_proj = Dream.body res_proj |> Lwt_main.run in
  let proj_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_proj)
    |> Yojson.Safe.Util.to_int
  in

  (* Setup Plate *)
  let req_plate =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "AutoFill Plate 1", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
  in
  let res_plate = Dream.test handler req_plate in
  let body_plate = Dream.body res_plate |> Lwt_main.run in
  let plate_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_plate)
    |> Yojson.Safe.Util.to_int
  in

  let req =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/plates/%d/auto-fill" plate_id)
      ~body:
        {|{"wells": ["A1"], "sample_short_ids": ["S-1"], "strategy": "magic"}|}
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for unknown strategy" 400
    (Dream.status res |> Dream.status_to_int);
  let body = Dream.body res |> Lwt_main.run in
  Alcotest.(check bool)
    "Should mention strategy in error" true
    (string_includes ~substring:"strategy" body);
  Lwt.return ()

let test_autofill_more_wells_than_samples _switch () =
  (* Setup Project *)
  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:{|{"name": "AutoFill Proj 1", "status": "Active"}|}
  in
  let res_proj = Dream.test handler req_proj in
  let body_proj = Dream.body res_proj |> Lwt_main.run in
  let proj_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_proj)
    |> Yojson.Safe.Util.to_int
  in

  (* Setup Sample *)
  let req_s1 =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": 1, "result_definition_ids": []}|}
  in
  let _res_s1 = Dream.test handler req_s1 in
  let body_s1 = Dream.body _res_s1 |> Lwt_main.run in
  let json_s1 = Yojson.Safe.from_string body_s1 in
  let short_s1 =
    match Yojson.Safe.Util.member "short_id" json_s1 with
    | `String s -> s
    | _ ->
        Yojson.Safe.Util.member "id" json_s1
        |> Yojson.Safe.Util.to_int |> string_of_int
  in

  (* Setup Plate *)
  let req_plate =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "AutoFill Plate 1", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
  in
  let res_plate = Dream.test handler req_plate in
  let body_plate = Dream.body res_plate |> Lwt_main.run in
  let plate_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_plate)
    |> Yojson.Safe.Util.to_int
  in

  (* Auto Fill 3 Wells with 1 Sample *)
  let req_fill =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/plates/%d/auto-fill" plate_id)
      ~body:
        (Printf.sprintf
           {|{"wells": ["A1", "A2", "A3"], "sample_short_ids": ["%s"]}|}
           short_s1)
  in
  let res_fill = Dream.test handler req_fill in
  Alcotest.(check int)
    "Should return 200 OK" 200
    (Dream.status res_fill |> Dream.status_to_int);

  (* Verify Plate Layout *)
  let req_get =
    Test_utils.json_get ~path:(Printf.sprintf "/api/v1/plates/%d" plate_id)
  in
  let res_get = Dream.test handler req_get in
  let body_get = Dream.body res_get |> Lwt_main.run in
  let json_get = Yojson.Safe.from_string body_get in
  let wells = Yojson.Safe.Util.(member "wells" json_get |> to_list) in

  let assigned_wells =
    List.filter
      (fun w ->
        Yojson.Safe.Util.(
          member "well" w |> member "sample_id" |> to_option to_int)
        <> None)
      wells
  in
  Alcotest.(check int)
    "Should only assign 1 well" 1
    (List.length assigned_wells);

  Lwt.return ()

let test_autofill_more_samples_than_wells _switch () =
  (* Setup Project *)
  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:{|{"name": "AutoFill Proj 2", "status": "Active"}|}
  in
  let res_proj = Dream.test handler req_proj in
  let body_proj = Dream.body res_proj |> Lwt_main.run in
  let proj_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_proj)
    |> Yojson.Safe.Util.to_int
  in

  (* Setup Samples *)
  let req_s1 =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": 1, "result_definition_ids": []}|}
  in
  let _res_s1 = Dream.test handler req_s1 in
  let body_s1 = Dream.body _res_s1 |> Lwt_main.run in
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
        {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": 1, "result_definition_ids": []}|}
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

  (* Setup Plate *)
  let req_plate =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "AutoFill Plate 2", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
  in
  let res_plate = Dream.test handler req_plate in
  let body_plate = Dream.body res_plate |> Lwt_main.run in
  let plate_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_plate)
    |> Yojson.Safe.Util.to_int
  in

  (* Auto Fill 1 Well with 2 Samples *)
  let req_fill =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/plates/%d/auto-fill" plate_id)
      ~body:
        (Printf.sprintf {|{"wells": ["A1"], "sample_short_ids": ["%s", "%s"]}|}
           short_s1 short_s2)
  in
  let res_fill = Dream.test handler req_fill in
  Alcotest.(check int)
    "Should return 200 OK" 200
    (Dream.status res_fill |> Dream.status_to_int);

  (* Verify Plate Layout *)
  let req_get =
    Test_utils.json_get ~path:(Printf.sprintf "/api/v1/plates/%d" plate_id)
  in
  let res_get = Dream.test handler req_get in
  let body_get = Dream.body res_get |> Lwt_main.run in
  let json_get = Yojson.Safe.from_string body_get in
  let wells = Yojson.Safe.Util.(member "wells" json_get |> to_list) in

  let assigned_wells =
    List.filter
      (fun w ->
        Yojson.Safe.Util.(
          member "well" w |> member "sample_id" |> to_option to_int)
        <> None)
      wells
  in
  Alcotest.(check int)
    "Should only assign 1 well" 1
    (List.length assigned_wells);

  Lwt.return ()

let test_autofill_out_of_bounds _switch () =
  (* Setup Project *)
  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:{|{"name": "AutoFill Proj 3", "status": "Active"}|}
  in
  let res_proj = Dream.test handler req_proj in
  let body_proj = Dream.body res_proj |> Lwt_main.run in
  let proj_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_proj)
    |> Yojson.Safe.Util.to_int
  in

  (* Setup Sample *)
  let req_s1 =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": 1, "result_definition_ids": []}|}
  in
  let _res_s1 = Dream.test handler req_s1 in
  let body_s1 = Dream.body _res_s1 |> Lwt_main.run in
  let json_s1 = Yojson.Safe.from_string body_s1 in
  let short_s1 =
    match Yojson.Safe.Util.member "short_id" json_s1 with
    | `String s -> s
    | _ ->
        Yojson.Safe.Util.member "id" json_s1
        |> Yojson.Safe.Util.to_int |> string_of_int
  in

  (* Setup Plate (24-well) *)
  let req_plate =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "AutoFill Plate 3", "project_id": %d, "plate_format": "24-well"}|}
           proj_id)
  in
  let res_plate = Dream.test handler req_plate in
  let body_plate = Dream.body res_plate |> Lwt_main.run in
  let plate_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_plate)
    |> Yojson.Safe.Util.to_int
  in

  (* Try to fill H12 on a 24 well plate *)
  let req_fill =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/plates/%d/auto-fill" plate_id)
      ~body:
        (Printf.sprintf {|{"wells": ["H12"], "sample_short_ids": ["%s"]}|}
           short_s1)
  in
  let res_fill = Dream.test handler req_fill in
  Alcotest.(check int)
    "Should return 400 Bad Request" 400
    (Dream.status res_fill |> Dream.status_to_int);
  let body_fill = Dream.body res_fill |> Lwt_main.run in
  Alcotest.(check bool)
    "Should mention bounds error" true
    (string_includes ~substring:"bounds" body_fill
    || string_includes ~substring:"Invalid" body_fill);

  Lwt.return ()

let suite =
  [
    ( "Plate Auto-Fill API",
      [
        Alcotest_lwt.test_case "Fails Unknown Strategy" `Quick
          test_autofill_strategy_failure;
        Alcotest_lwt.test_case "Option B - More Wells than Samples" `Quick
          test_autofill_more_wells_than_samples;
        Alcotest_lwt.test_case "Option B - More Samples than Wells" `Quick
          test_autofill_more_samples_than_wells;
        Alcotest_lwt.test_case "Fails Out of Bounds Well" `Quick
          test_autofill_out_of_bounds;
      ] );
  ]
