open Exlab_server

let handler = Test_utils.admin_app

let test_community_get_nested_projects _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Nested Proj Community %f" time in
  let comm_name = Printf.sprintf "Nested Community %f" time in

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

  (* 2. Create Community *)
  let* comm_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/communities"
      ~body:(Printf.sprintf {|{"name": "%s"}|} comm_name)
      ~expected_status:201 "Create Community"
  in
  let* comm_body = Dream.body comm_res in
  let comm_id =
    Yojson.Safe.Util.(member "id" (Yojson.Safe.from_string comm_body) |> to_int)
  in

  (* 3. Create Source Sample *)
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%d/samples" proj_id)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Source", "community_id": %d}|}
           comm_id)
      ~expected_status:201 "Create Source Sample for Community"
  in

  (* 4. Get Community and verify nested project data *)
  let path = Printf.sprintf "/api/v1/communities/%d" comm_id in
  let* get_res =
    Test_utils.assert_json_get ~handler ~path ~expected_status:200
      "Get Community by ID"
  in
  let* get_body = Dream.body get_res in
  let json = Yojson.Safe.from_string get_body in

  let source_samples =
    Yojson.Safe.Util.(member "source_samples" json |> to_list)
  in
  Alcotest.(check int)
    "Should have exactly 1 source sample" 1
    (List.length source_samples);

  let sample_entry = List.hd source_samples in
  let returned_proj_id =
    Yojson.Safe.Util.(member "project" sample_entry |> member "id" |> to_int)
  in
  let returned_proj_name =
    Yojson.Safe.Util.(
      member "project" sample_entry |> member "name" |> to_string)
  in

  Alcotest.(check int) "Returned project ID matches" proj_id returned_proj_id;
  Alcotest.(check string)
    "Returned project name matches" proj_name returned_proj_name;

  Lwt.return ()

let suite =
  [
    ( "Community API",
      [
        Alcotest_lwt.test_case "Get Community returns nested projects" `Quick
          test_community_get_nested_projects;
      ] );
  ]
