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

let test_bulk_update_layouts_by_short_id _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Bulk Update ShortID Project %f" time in

  (* 1. Create Project *)
  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
  in
  let res_proj = Dream.test handler req_proj in
  let* proj_body_str = Dream.body res_proj in
  let proj_json = Yojson.Safe.from_string proj_body_str in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_id_str = string_of_int proj_id in

  (* 2. Create Strain & Samples *)
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
  let s1_id = Yojson.Safe.Util.(member "id" s1_json |> to_int) in
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
  let s2_id = Yojson.Safe.Util.(member "id" s2_json |> to_int) in
  let s2_short = Yojson.Safe.Util.(member "short_id" s2_json |> to_string) in

  (* 3. Create Blank Plate with a distinct human name *)
  let req_create_plate =
    Test_utils.csv_post
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well"
           proj_id_str)
      ~body:"plate_name\nPlate_Custom_Name_123"
  in
  let res_create_plate = Dream.test handler req_create_plate in
  Alcotest.(check int)
    "Plate creation returns 201" 201
    (Dream.status res_create_plate |> Dream.status_to_int);

  let* create_body_str = Dream.body res_create_plate in
  let create_json = Yojson.Safe.from_string create_body_str in
  let created_plate =
    Yojson.Safe.Util.(member "plates" create_json |> to_list |> List.hd)
  in
  let plate_short_id =
    Yojson.Safe.Util.(member "short_id" created_plate |> to_string)
  in

  (* 4. PATCH using plate_short_id header and plate_short_id value *)
  let patch_body =
    Printf.sprintf "plate_short_id,well,sample_short_id\n%s,A1,%s\n%s,B2,%s"
      plate_short_id s1_short plate_short_id s2_short
  in
  let req_patch =
    Test_utils.csv_patch
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well"
           proj_id_str)
      ~body:patch_body
  in
  let res_patch = Dream.test handler req_patch in
  Alcotest.(check int)
    "PATCH returns 200" 200
    (Dream.status res_patch |> Dream.status_to_int);

  (* 5. Verify Plate Wells *)
  let req_get =
    Test_utils.json_get
      ~path:(Printf.sprintf "/api/v1/plates/%s" plate_short_id)
  in
  let res_get = Dream.test handler req_get in
  let* get_body_str = Dream.body res_get in
  let get_json = Yojson.Safe.from_string get_body_str in
  let wells = Yojson.Safe.Util.(member "wells" get_json |> to_list) in

  let find_well coord =
    List.find_opt
      (fun w_obj ->
        let w = Yojson.Safe.Util.member "well" w_obj in
        Yojson.Safe.Util.(member "coordinate" w |> to_string) = coord)
      wells
  in

  let well_a1_opt = find_well "A1" in
  Alcotest.(check bool) "A1 well exists" true (Option.is_some well_a1_opt);
  let well_a1 = Option.get well_a1_opt |> Yojson.Safe.Util.member "well" in
  let sample_a1 =
    Yojson.Safe.Util.(member "sample_id" well_a1 |> to_int_option)
  in
  Alcotest.(check (option int)) "A1 has sample s1" (Some s1_id) sample_a1;

  let well_b2_opt = find_well "B2" in
  Alcotest.(check bool) "B2 well exists" true (Option.is_some well_b2_opt);
  let well_b2 = Option.get well_b2_opt |> Yojson.Safe.Util.member "well" in
  let sample_b2 =
    Yojson.Safe.Util.(member "sample_id" well_b2 |> to_int_option)
  in
  Alcotest.(check (option int)) "B2 has sample s2" (Some s2_id) sample_b2;

  Lwt.return ()

let test_bulk_update_layouts_case_insensitive_name _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Bulk Case Project %f" time in

  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
  in
  let res_proj = Dream.test handler req_proj in
  let* proj_body_str = Dream.body res_proj in
  let proj_json = Yojson.Safe.from_string proj_body_str in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_id_str = string_of_int proj_id in

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
  let s1_id = Yojson.Safe.Util.(member "id" s1_json |> to_int) in
  let s1_short = Yojson.Safe.Util.(member "short_id" s1_json |> to_string) in

  let req_create_plate =
    Test_utils.csv_post
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well"
           proj_id_str)
      ~body:"plate_name\nPlate_Upper_Case"
  in
  let res_create_plate = Dream.test handler req_create_plate in
  Alcotest.(check int)
    "Plate creation returns 201" 201
    (Dream.status res_create_plate |> Dream.status_to_int);

  (* Update using lowercase "plate_upper_case" *)
  let patch_body =
    Printf.sprintf "plate_name,well,sample_short_id\nplate_upper_case,C3,%s"
      s1_short
  in
  let req_patch =
    Test_utils.csv_patch
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well"
           proj_id_str)
      ~body:patch_body
  in
  let res_patch = Dream.test handler req_patch in
  Alcotest.(check int)
    "PATCH returns 200 for case-insensitive name" 200
    (Dream.status res_patch |> Dream.status_to_int);

  let* create_body_str = Dream.body res_create_plate in
  let create_json = Yojson.Safe.from_string create_body_str in
  let created_plate =
    Yojson.Safe.Util.(member "plates" create_json |> to_list |> List.hd)
  in
  let plate_short_id =
    Yojson.Safe.Util.(member "short_id" created_plate |> to_string)
  in

  let req_get =
    Test_utils.json_get
      ~path:(Printf.sprintf "/api/v1/plates/%s" plate_short_id)
  in
  let res_get = Dream.test handler req_get in
  let* get_body_str = Dream.body res_get in
  let get_json = Yojson.Safe.from_string get_body_str in
  let wells = Yojson.Safe.Util.(member "wells" get_json |> to_list) in
  let well_c3 =
    List.find
      (fun w_obj ->
        let w = Yojson.Safe.Util.member "well" w_obj in
        Yojson.Safe.Util.(member "coordinate" w |> to_string) = "C3")
      wells
    |> Yojson.Safe.Util.member "well"
  in
  let sample_c3 =
    Yojson.Safe.Util.(member "sample_id" well_c3 |> to_int_option)
  in
  Alcotest.(check (option int)) "C3 has sample s1" (Some s1_id) sample_c3;

  Lwt.return ()

let test_bulk_update_layouts_ambiguous_name _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Bulk Ambiguous Project %f" time in

  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
  in
  let res_proj = Dream.test handler req_proj in
  let* proj_body_str = Dream.body res_proj in
  let proj_json = Yojson.Safe.from_string proj_body_str in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_id_str = string_of_int proj_id in

  (* Create two plates with the exact same name via single POSTs *)
  let req_p1 =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Duplicate Plate", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
  in
  let res_p1 = Dream.test handler req_p1 in
  Alcotest.(check int)
    "Create Plate 1 returns 201" 201
    (Dream.status res_p1 |> Dream.status_to_int);

  let req_p2 =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Duplicate Plate", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
  in
  let res_p2 = Dream.test handler req_p2 in
  Alcotest.(check int)
    "Create Plate 2 returns 201" 201
    (Dream.status res_p2 |> Dream.status_to_int);

  (* Try to PATCH by name "Duplicate Plate" *)
  let patch_body =
    "plate_name,well,sample_short_id\nDuplicate Plate,A1,DUMMY-SAMPLE"
  in
  let req_patch =
    Test_utils.csv_patch
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well"
           proj_id_str)
      ~body:patch_body
  in
  let res_patch = Dream.test handler req_patch in
  Alcotest.(check int)
    "PATCH returns 400 Bad Request for ambiguous duplicate plate name" 400
    (Dream.status res_patch |> Dream.status_to_int);

  Lwt.return ()

let test_bulk_update_layouts_atomic_rollback _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Bulk Atomic Project %f" time in

  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
  in
  let res_proj = Dream.test handler req_proj in
  let* proj_body_str = Dream.body res_proj in
  let proj_json = Yojson.Safe.from_string proj_body_str in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_id_str = string_of_int proj_id in

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

  (* Create valid plate 1 *)
  let req_p1 =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Valid Plate 1", "project_id": %d, "plate_format": "96-well"}|}
           proj_id)
  in
  let res_p1 = Dream.test handler req_p1 in
  let* p1_body_str = Dream.body res_p1 in
  let p1_json = Yojson.Safe.from_string p1_body_str in
  let p1_short = Yojson.Safe.Util.(member "short_id" p1_json |> to_string) in

  (* PATCH includes Valid Plate 1 and Nonexistent Plate 2 *)
  let patch_body =
    Printf.sprintf
      "plate_name,well,sample_short_id\n%s,A1,%s\nNonexistent_Plate,A1,%s"
      p1_short s1_short s1_short
  in
  let req_patch =
    Test_utils.csv_patch
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/plates/bulk-csv?plate_format=96-well"
           proj_id_str)
      ~body:patch_body
  in
  let res_patch = Dream.test handler req_patch in
  Alcotest.(check int)
    "PATCH returns 404 for missing plate" 404
    (Dream.status res_patch |> Dream.status_to_int);

  (* Verify Valid Plate 1 well A1 is STILL empty *)
  let req_get =
    Test_utils.json_get ~path:(Printf.sprintf "/api/v1/plates/%s" p1_short)
  in
  let res_get = Dream.test handler req_get in
  let* get_body_str = Dream.body res_get in
  let get_json = Yojson.Safe.from_string get_body_str in
  let wells = Yojson.Safe.Util.(member "wells" get_json |> to_list) in
  let well_a1 =
    List.find
      (fun w_obj ->
        let w = Yojson.Safe.Util.member "well" w_obj in
        Yojson.Safe.Util.(member "coordinate" w |> to_string) = "A1")
      wells
    |> Yojson.Safe.Util.member "well"
  in
  let sample_a1 =
    Yojson.Safe.Util.(member "sample_id" well_a1 |> to_int_option)
  in
  Alcotest.(check (option int))
    "A1 remains empty due to atomic rollback" None sample_a1;

  Lwt.return ()

let test_bulk_update_layouts_out_of_bounds_for_plate_format _switch () =
  let open Lwt.Syntax in
  let time = Unix.gettimeofday () in
  let proj_name = Printf.sprintf "Bulk Bounds Project %f" time in

  let req_proj =
    Test_utils.json_post ~path:"/api/v1/projects"
      ~body:(Printf.sprintf {|{"name": "%s"}|} proj_name)
  in
  let res_proj = Dream.test handler req_proj in
  let* proj_body_str = Dream.body res_proj in
  let proj_json = Yojson.Safe.from_string proj_body_str in
  let proj_id = Yojson.Safe.Util.(member "id" proj_json |> to_int) in
  let proj_id_str = string_of_int proj_id in

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

  (* Create a 24-well plate (rows A-D, columns 1-6) *)
  let req_p1 =
    Test_utils.json_post ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "24 Well Plate", "project_id": %d, "plate_format": "24-well"}|}
           proj_id)
  in
  let res_p1 = Dream.test handler req_p1 in
  let* p1_body_str = Dream.body res_p1 in
  let p1_json = Yojson.Safe.from_string p1_body_str in
  let p1_short = Yojson.Safe.Util.(member "short_id" p1_json |> to_string) in

  (* Try to update with coordinate H12 (invalid for 24-well plate) *)
  let patch_body =
    Printf.sprintf "plate_short_id,well,sample_short_id\n%s,H12,%s" p1_short
      s1_short
  in
  let req_patch =
    Test_utils.csv_patch
      ~path:(Printf.sprintf "/api/v1/projects/%s/plates/bulk-csv" proj_id_str)
      ~body:patch_body
  in
  let res_patch = Dream.test handler req_patch in
  Alcotest.(check int)
    "PATCH returns 400 Bad Request for out-of-bounds coordinate on 24-well \
     plate"
    400
    (Dream.status res_patch |> Dream.status_to_int);

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
        Alcotest_lwt.test_case "PATCH: Success by Short ID" `Quick
          test_bulk_update_layouts_by_short_id;
        Alcotest_lwt.test_case "PATCH: Case-insensitive Name" `Quick
          test_bulk_update_layouts_case_insensitive_name;
        Alcotest_lwt.test_case "PATCH: Ambiguous Duplicate Name" `Quick
          test_bulk_update_layouts_ambiguous_name;
        Alcotest_lwt.test_case "PATCH: Atomic Rollback" `Quick
          test_bulk_update_layouts_atomic_rollback;
        Alcotest_lwt.test_case "PATCH: Reject Out of Bounds for Plate Format"
          `Quick test_bulk_update_layouts_out_of_bounds_for_plate_format;
      ] );
  ]
