let handler = Test_utils.admin_app

let create_test_result_definition handler data_type =
  let json =
    Printf.sprintf
      {|{
        "short_id": "RD-%s",
        "name": "Test %s",
        "description": "A test definition for %s",
        "data_type": "%s"
      }|}
      data_type data_type data_type data_type
  in
  let req =
    Test_utils.json_post ~path:"/api/v1/result-definitions" ~body:json
  in
  let res = Dream.test handler req in
  let body = Dream.body res |> Lwt_main.run in
  let json_res = Yojson.Safe.from_string body in
  let id = Yojson.Safe.Util.member "id" json_res |> Yojson.Safe.Util.to_int in
  Lwt.return id

let test_update_result_value () =
  let rand_suffix =
    string_of_int (int_of_float (Unix.gettimeofday () *. 1000.0))
  in
  (* Create a project and plate *)
  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:
        (Printf.sprintf {|{"name": "Test Project %s", "status": "Active"}|}
           rand_suffix)
  in
  let res_proj = Dream.test handler req_proj in
  let body_proj = Dream.body res_proj |> Lwt_main.run in
  let proj_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_proj)
    |> Yojson.Safe.Util.to_int
  in

  let req_plate =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Test Plate %s", "project_id": %d, "plate_format": "96-well"}|}
           rand_suffix proj_id)
  in
  let res_plate = Dream.test handler req_plate in
  let body_plate = Dream.body res_plate |> Lwt_main.run in
  let plate_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_plate)
    |> Yojson.Safe.Util.to_int
  in

  (* Create Result Definition *)
  let req_def =
    Test_utils.json_post ~path:"/api/v1/result-definitions"
      ~body:
        (Printf.sprintf
           {|{"short_id": "TS-1-%s", "name": "TS Float %s", "data_type": "FloatSeries"}|}
           rand_suffix rand_suffix)
  in
  let res_def = Dream.test handler req_def in
  let body_def = Dream.body res_def |> Lwt_main.run in
  let def_id =
    Yojson.Safe.Util.member "id" (Yojson.Safe.from_string body_def)
    |> Yojson.Safe.Util.to_int
  in

  (* Post Initial Result *)
  let req_res =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/plates/%d/results" plate_id)
      ~body:
        (Printf.sprintf
           {|{"result_definition_id": %d, "value": {"type": "FloatSeries", "value": [{"time": 1.0, "value": 42.0}]}}|}
           def_id)
  in
  let res_res = Dream.test handler req_res in
  Alcotest.(check int)
    "Create status" 201
    (Dream.status res_res |> Dream.status_to_int);
  let body_res = Dream.body res_res |> Lwt_main.run in
  let result_uid =
    Yojson.Safe.Util.member "uid" (Yojson.Safe.from_string body_res)
    |> Yojson.Safe.Util.to_string
  in

  (* Put Updated Result *)
  let req_put =
    Dream.request ~method_:`PUT
      ~target:(Printf.sprintf "/api/v1/results/%s" result_uid)
      ""
  in
  Dream.add_header req_put "Content-Type" "application/json";
  Dream.set_body req_put
    {|{"value": {"type": "FloatSeries", "value": [{"time": 1.0, "value": 42.0}, {"time": 2.0, "value": 84.0}]}}|};
  let res_put = Dream.test handler req_put in
  Alcotest.(check int)
    "Update status" 200
    (Dream.status res_put |> Dream.status_to_int);
  let body_put = Dream.body res_put |> Lwt_main.run in
  let json_put = Yojson.Safe.from_string body_put in
  let value_list =
    Yojson.Safe.Util.(member "value" json_put |> member "value" |> to_list)
  in
  Alcotest.(check int) "Updated points count" 2 (List.length value_list);
  Lwt.return_unit

let suite =
  [
    ( "Result Value Update API",
      [
        Alcotest_lwt.test_case "Update TimeSeries via PUT" `Quick (fun _ () ->
            test_update_result_value ());
      ] );
  ]
