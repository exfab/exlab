let handler = Test_utils.admin_app

let test_bulk_plate_csv_missing_header _switch () =
  let req =
    Test_utils.csv_post ~path:"/api/v1/projects/1/plates/bulk-csv"
      ~body:"bad_header\nPlate 1"
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing plate_name header" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_plate_csv_success_multiple_plates _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Bulk Plate Project %f" time in
  (* Create Project *)
  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
  in
  let res_proj = Dream.test handler req_proj in
  let* proj_body_str = Dream.body res_proj in
  let proj_json = Yojson.Safe.from_string proj_body_str in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_id_str = string_of_int proj_id in

  (* Create Samples - Needs proper topology and type per core requirements *)
  let req_strain =
    Test_utils.json_post ~path:"/api/v1/strains"
      ~body:
        {|{"genus": "Escherichia", "species": "coli", "strain_name": "DH5a", "links": []}|}
  in
  let res_strain = Dream.test handler req_strain in
  let* body_strain = Dream.body res_strain in
  let strain_id =
    Yojson.Safe.Util.(
      member "id" (Yojson.Safe.from_string body_strain) |> to_int)
  in

  let req_s1 =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%s/samples" proj_id_str)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Liquid Cell Culture", "category": "Source", "strain_id": %d}|}
           strain_id)
  in
  let res_s1 = Dream.test handler req_s1 in
  let* body_s1 = Dream.body res_s1 in
  let s1_json = Yojson.Safe.from_string body_s1 in
  let s1_short = Yojson.Safe.Util.(member "short_id" s1_json |> to_string) in

  let req_s2 =
    Test_utils.json_post
      ~path:(Printf.sprintf "/api/v1/projects/%s/samples" proj_id_str)
      ~body:
        (Printf.sprintf
           {|{"sample_type": "Solid Cell Culture", "category": "Source", "strain_id": %d}|}
           strain_id)
  in
  let res_s2 = Dream.test handler req_s2 in
  let* body_s2 = Dream.body res_s2 in
  let s2_json = Yojson.Safe.from_string body_s2 in
  let s2_short = Yojson.Safe.Util.(member "short_id" s2_json |> to_string) in

  let req =
    Test_utils.csv_post
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well"
           proj_id_str)
      ~body:
        (Printf.sprintf
           "plate_name,well,sample_short_id\n\
            Plate 1,A1,%s\n\
            Plate 1,A2,%s\n\
            Plate 2,,"
           s1_short s2_short)
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 201 Created for bulk plates" 201
    (Dream.status res |> Dream.status_to_int);

  (* Plate 2 is duplicated in the CSV but rows are grouped by name during
     creation, so exactly 2 plates are created: Plate 1 and Plate 2. *)
  let req_list =
    Test_utils.json_get
      ~path:(Printf.sprintf "/api/v1/projects/%s/plates" proj_id_str)
  in
  let res_list = Dream.test handler req_list in
  let* list_body = Dream.body res_list in
  let list_json = Yojson.Safe.from_string list_body in
  let count = Yojson.Safe.Util.(member "count" list_json |> to_int) in
  Alcotest.(check int) "Should have exactly 2 plates created" 2 count;

  let plates = Yojson.Safe.Util.(member "data" list_json |> to_list) in
  let plate1_id =
    List.find
      (fun p -> Yojson.Safe.Util.(member "name" p |> to_string) = "Plate 1")
      plates
    |> fun p -> Yojson.Safe.Util.(member "id" p |> to_int)
  in
  let plate2_id =
    List.find
      (fun p -> Yojson.Safe.Util.(member "name" p |> to_string) = "Plate 2")
      plates
    |> fun p -> Yojson.Safe.Util.(member "id" p |> to_int)
  in

  let req_wells1 =
    Test_utils.json_get ~path:(Printf.sprintf "/api/v1/plates/%d" plate1_id)
  in
  let res_wells1 = Dream.test handler req_wells1 in
  let* body_wells1 = Dream.body res_wells1 in
  let wells1 =
    Yojson.Safe.Util.(
      member "wells" (Yojson.Safe.from_string body_wells1) |> to_list)
  in
  let plate1_samples =
    List.filter
      (fun w ->
        Yojson.Safe.Util.(member "well" w |> member "sample_id") <> `Null)
      wells1
  in
  Alcotest.(check int)
    "Should have exactly 2 samples assigned to Plate 1" 2
    (List.length plate1_samples);

  let req_wells2 =
    Test_utils.json_get ~path:(Printf.sprintf "/api/v1/plates/%d" plate2_id)
  in
  let res_wells2 = Dream.test handler req_wells2 in
  let* body_wells2 = Dream.body res_wells2 in
  let wells2 =
    Yojson.Safe.Util.(
      member "wells" (Yojson.Safe.from_string body_wells2) |> to_list)
  in
  let plate2_samples =
    List.filter
      (fun w ->
        Yojson.Safe.Util.(member "well" w |> member "sample_id") <> `Null)
      wells2
  in
  Alcotest.(check int)
    "Should have exactly 0 samples assigned to Plate 2" 0
    (List.length plate2_samples);

  Lwt.return ()

let test_bulk_plate_csv_plate_type_prefix _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Bulk Plate Prefix Project %f" time in

  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
  in
  let res_proj = Dream.test handler req_proj in
  let* proj_body_str = Dream.body res_proj in
  let proj_json = Yojson.Safe.from_string proj_body_str in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_id_str = string_of_int proj_id in

  let req =
    Test_utils.csv_post
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well&plate_type=Source"
           proj_id_str)
      ~body:"plate_name,well,sample_short_id\nSource Plate 1,,"
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 201 Created for bulk plates" 201
    (Dream.status res |> Dream.status_to_int);

  let* res_body_str = Dream.body res in
  let res_json = Yojson.Safe.from_string res_body_str in
  let plates = Yojson.Safe.Util.(member "plates" res_json |> to_list) in

  let plate_short_id =
    List.hd plates |> fun p ->
    Yojson.Safe.Util.(member "short_id" p |> to_string)
  in

  Alcotest.(check string)
    "Source plate should have S- prefix" "S"
    (String.sub plate_short_id 0 1);

  Lwt.return ()

let test_bulk_update_layouts_missing_header _switch () =
  let req =
    Test_utils.csv_patch ~path:"/api/v1/projects/1/plates/bulk-csv"
      ~body:"well,sample_short_id\nA1,S1"
  in
  let res = Dream.test handler req in
  Alcotest.(check int)
    "Should return 400 for missing plate_name header" 400
    (Dream.status res |> Dream.status_to_int);
  Lwt.return ()

let test_bulk_update_layouts_invalid_format _switch () =
  let time = Unix.gettimeofday () in
  let req =
    Test_utils.csv_patch ~path:"/api/v1/projects/1/plates/bulk-csv"
      ~body:
        (Printf.sprintf "plate_name,well,sample_short_id\nPlate-404-%f,H12,S1"
           time)
  in
  let res = Dream.test handler req in
  let status = Dream.status res |> Dream.status_to_int in
  let is_400_or_404 = status = 400 || status = 404 in
  Alcotest.(check bool)
    "Should return 400 (out of bounds well) or 404 (missing plate)" true
    is_400_or_404;
  Lwt.return ()

let test_bulk_update_layouts_missing_sample _switch () =
  let time = Unix.gettimeofday () in
  let req =
    Test_utils.csv_patch ~path:"/api/v1/projects/1/plates/bulk-csv"
      ~body:
        (Printf.sprintf
           "plate_name,well,sample_short_id\nPlate-404-%f,A1,MISSING-SAMPLE-999"
           time)
  in
  let res = Dream.test handler req in
  let status = Dream.status res |> Dream.status_to_int in
  let is_400_or_404 = status = 400 || status = 404 in
  Alcotest.(check bool)
    "Should return 400 (missing sample) or 404 (missing plate)" true
    is_400_or_404;
  Lwt.return ()

let test_bulk_plate_csv_rollback_on_invalid_sample _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Bulk Plate Rollback Project %f" time in

  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
  in
  let res_proj = Dream.test handler req_proj in
  let* proj_body_str = Dream.body res_proj in
  let proj_json = Yojson.Safe.from_string proj_body_str in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_id_str = string_of_int proj_id in

  let req =
    Test_utils.csv_post
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well"
           proj_id_str)
      ~body:
        "plate_name,well,sample_short_id\n\
         Rollback Plate 1,A1,NONEXISTENT-SAMPLE\n\
         Rollback Plate 2,,"
  in
  let res = Dream.test handler req in
  let status = Dream.status res |> Dream.status_to_int in
  Alcotest.(check bool)
    "Should return 400 or 404 for invalid sample" true
    (status = 400 || status = 404);

  (* Check that NO plates were created because of the rollback *)
  let req_list =
    Test_utils.json_get
      ~path:(Printf.sprintf "/api/v1/projects/%s/plates" proj_id_str)
  in
  let res_list = Dream.test handler req_list in
  let* list_body = Dream.body res_list in
  let list_json = Yojson.Safe.from_string list_body in
  let count = Yojson.Safe.Util.(member "count" list_json |> to_int) in
  Alcotest.(check int)
    "Should have exactly 0 plates created due to rollback" 0 count;

  Lwt.return ()

let suite =
  [
    ( "Bulk Plate CSV API",
      [
        Alcotest_lwt.test_case "POST: Missing Header" `Quick
          test_bulk_plate_csv_missing_header;
        Alcotest_lwt.test_case "POST: Success Multiple Plates" `Quick
          test_bulk_plate_csv_success_multiple_plates;
        Alcotest_lwt.test_case "POST: Plate Type Prefix" `Quick
          test_bulk_plate_csv_plate_type_prefix;
        Alcotest_lwt.test_case "POST: Rollback on Invalid Sample" `Quick
          test_bulk_plate_csv_rollback_on_invalid_sample;
        Alcotest_lwt.test_case "PATCH: Missing Header" `Quick
          test_bulk_update_layouts_missing_header;
        Alcotest_lwt.test_case "PATCH: Invalid Format/Bounds" `Quick
          test_bulk_update_layouts_invalid_format;
        Alcotest_lwt.test_case "PATCH: Missing Sample" `Quick
          test_bulk_update_layouts_missing_sample;
      ] );
  ]
