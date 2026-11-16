(* Integration tests for vendor-neutral Analysis Runs, Plate Media, and Storage Connections API *)

open Lwt.Syntax

let handler = Test_utils.admin_app

let create_test_project name =
  let body = Printf.sprintf {|{"name": %S, "status": "Active"}|} name in
  let* res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects" ~body
      ~expected_status:201 "Create project for analysis run test"
  in
  let* body_str = Dream.body res in
  let json = Yojson.Safe.from_string body_str in
  let short_id = Yojson.Safe.Util.(json |> member "short_id" |> to_string) in
  Lwt.return short_id

let test_register_first_run_and_get_canonical _switch () =
  let* project_short_id = create_test_project "Analysis Run Tracer Project" in
  let canonical_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs/canonical"
      project_short_id
  in
  let runs_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs" project_short_id
  in

  let* _ =
    Test_utils.assert_json_get ~handler ~path:canonical_path
      ~expected_status:404 "Canonical returns 404 when no runs exist"
  in

  let post_body =
    {|{
      "tool_name": "PhenoTypic",
      "tool_version": "0.18.1",
      "storage_base_uri": "gs://exfab-data/UCR-029/deliverables",
      "dataset_checksum": "md5:GGtzyFMj+5yU4wGXM4i+KA==",
      "pipeline_name": "UCR_029_v14",
      "pipeline_config": {"version": "0.18.1", "nrows": 8, "ncols": 12},
      "artifacts": {
        "MeasureShape_parquet": "gs://exfab-data/UCR-029/deliverables/measurements_by_feature/MeasureShape.parquet"
      },
      "summary_metrics": {"plates_count": 111, "samples_count": 3841},
      "notes": "Initial pass"
    }|}
  in

  let* post_res =
    Test_utils.assert_json_post ~handler ~path:runs_path ~body:post_body
      ~expected_status:201 "Register first analysis run"
  in
  let* post_str = Dream.body post_res in
  let post_json = Yojson.Safe.from_string post_str in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "First run has version_number = 1" 1
    (post_json |> member "version_number" |> to_int);
  Alcotest.(check string)
    "tool_name is PhenoTypic" "PhenoTypic"
    (post_json |> member "tool_name" |> to_string);
  Alcotest.(check bool)
    "First run is canonical" true
    (post_json |> member "is_canonical" |> to_bool);

  let* canonical_res =
    Test_utils.assert_json_get ~handler ~path:canonical_path
      ~expected_status:200 "GET /canonical returns 200"
  in
  let* canonical_str = Dream.body canonical_res in
  let canonical_json = Yojson.Safe.from_string canonical_str in
  Alcotest.(check string)
    "Canonical dataset_checksum matches" "md5:GGtzyFMj+5yU4wGXM4i+KA=="
    (canonical_json |> member "dataset_checksum" |> to_string);
  Lwt.return ()

let test_versioning_and_promotion _switch () =
  let* project_short_id = create_test_project "Analysis Versioning Project" in
  let runs_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs" project_short_id
  in
  let canonical_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs/canonical"
      project_short_id
  in
  let v1_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs/1" project_short_id
  in
  let promote_v1_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs/1/canonical"
      project_short_id
  in

  let* _ =
    Test_utils.assert_json_post ~handler ~path:runs_path
      ~body:
        {|{"tool_name": "Nextflow", "storage_base_uri": "s3://lab/v1", "dataset_checksum": "sha256:v1"}|}
      ~expected_status:201 "Register v1"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:runs_path
      ~body:
        {|{"tool_name": "Nextflow", "storage_base_uri": "s3://lab/v2", "dataset_checksum": "sha256:v2"}|}
      ~expected_status:201 "Register v2"
  in

  let* v1_res =
    Test_utils.assert_json_get ~handler ~path:v1_path ~expected_status:200
      "GET /analysis-runs/1"
  in
  let* v1_str = Dream.body v1_res in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "v1 is superseded" false
    (Yojson.Safe.from_string v1_str |> member "is_canonical" |> to_bool);

  let* _ =
    Test_utils.assert_json_put ~handler ~path:promote_v1_path ~body:"{}"
      ~expected_status:200 "Promote v1 back to canonical"
  in
  let* can_res =
    Test_utils.assert_json_get ~handler ~path:canonical_path
      ~expected_status:200 "GET /canonical after rollback"
  in
  let* can_str = Dream.body can_res in
  Alcotest.(check int)
    "Canonical version is 1" 1
    (Yojson.Safe.from_string can_str |> member "version_number" |> to_int);
  Lwt.return ()

let test_cross_project_tool_filtering _switch () =
  let* proj_a = create_test_project "Multi-Tool Project A" in
  let* proj_b = create_test_project "Multi-Tool Project B" in

  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/analysis-runs" proj_a)
      ~body:
        {|{"tool_name": "CellProfiler", "storage_base_uri": "https://data.org/cp", "dataset_checksum": "cp_1"}|}
      ~expected_status:201 "Register CellProfiler run"
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/analysis-runs" proj_b)
      ~body:
        {|{"tool_name": "PhenoTypic", "storage_base_uri": "https://data.org/pt", "dataset_checksum": "pt_1"}|}
      ~expected_status:201 "Register PhenoTypic run"
  in

  let* res =
    Test_utils.assert_json_get ~handler
      ~path:"/api/v1/analysis-runs?canonical_only=true&tool=CellProfiler"
      ~expected_status:200 "Filter cross-project runs by tool=CellProfiler"
  in
  let* body_str = Dream.body res in
  let open Yojson.Safe.Util in
  let items = Yojson.Safe.from_string body_str |> member "data" |> to_list in
  let checksums =
    List.map (fun i -> i |> member "dataset_checksum" |> to_string) items
  in
  Alcotest.(check bool)
    "Includes CellProfiler run" true
    (List.mem "cp_1" checksums);
  Alcotest.(check bool)
    "Excludes PhenoTypic run" false
    (List.mem "pt_1" checksums);
  Lwt.return ()

let test_s3_storage_connections_and_plate_media _switch () =
  (* 1. Configure S3/MinIO via Admin Storage Connections API *)
  let s3_cfg_body =
    {|{
      "s3": {
        "endpoint_url": "https://minio.biofoundry.org",
        "region": "us-west-2",
        "access_key_id": "MINIO_TEST_ACCESS",
        "secret_access_key": "MINIO_SUPER_SECRET_KEY"
      }
    }|}
  in
  let* put_res =
    Test_utils.assert_json_put ~handler
      ~path:"/api/v1/admin/storage-connections" ~body:s3_cfg_body
      ~expected_status:200 "Admin configures S3/MinIO connection"
  in
  let* put_str = Dream.body put_res in
  Alcotest.(check bool)
    "PUT response masks S3 secret_access_key" false
    (Re.execp (Re.compile (Re.str "MINIO_SUPER_SECRET_KEY")) put_str);

  (* 2. Verify generic /api/v1/settings/storage_connections is blocked (403) *)
  let* _ =
    Test_utils.assert_json_get ~handler
      ~path:"/api/v1/settings/storage_connections" ~expected_status:403
      "Generic settings API refuses to leak storage_connections"
  in

  (* 3. Create project + plate + S3-backed analysis run *)
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "S3 Media Project", "status": "Active"}|}
      ~expected_status:201 "Create project"
  in
  let* proj_str = Dream.body proj_res in
  let open Yojson.Safe.Util in
  let proj_json = Yojson.Safe.from_string proj_str in
  let project_id = proj_json |> member "id" |> to_int in
  let project_short_id = proj_json |> member "short_id" |> to_string in

  let* plate_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Media Plate", "project_id": %d, "plate_format": "96-well"}|}
           project_id)
      ~expected_status:201 "Create plate"
  in
  let* plate_str = Dream.body plate_res in
  let plate_short_id =
    Yojson.Safe.from_string plate_str |> member "short_id" |> to_string
  in

  let run_body =
    Printf.sprintf
      {|{
        "tool_name": "CellProfiler",
        "storage_base_uri": "s3://lab-bucket/exp1",
        "dataset_checksum": "etag:abc123",
        "artifacts": {
          "features_parquet": "s3://lab-bucket/exp1/features.parquet"
        },
        "plate_media": {
          %S: [
            {"time": 2, "uri": "s3://lab-bucket/exp1/media/t2.png", "media_type": "image"},
            {"time": 1, "uri": "s3://lab-bucket/exp1/media/t1.png", "media_type": "image"}
          ]
        }
      }|}
      plate_short_id
  in
  let* _ =
    Test_utils.assert_json_post ~handler
      ~path:
        (Printf.sprintf "/api/v1/projects/%s/analysis-runs" project_short_id)
      ~body:run_body ~expected_status:201 "Register S3-backed run"
  in

  (* 4. Sign S3 artifact URL via /canonical/signed-url *)
  let* signed_res =
    Test_utils.assert_json_get ~handler
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/analysis-runs/canonical/signed-url?artifact=features_parquet"
           project_short_id)
      ~expected_status:200 "Resolve S3 SigV4 URL for artifact"
  in
  let* signed_str = Dream.body signed_res in
  let signed_url =
    Yojson.Safe.from_string signed_str |> member "signed_url" |> to_string
  in
  Alcotest.(check bool)
    "Signed URL points to MinIO endpoint with AWS4-HMAC-SHA256" true
    (String.starts_with
       ~prefix:
         "https://minio.biofoundry.org/lab-bucket/exp1/features.parquet?X-Amz-Algorithm=AWS4-HMAC-SHA256"
       signed_url);

  (* 5. Fetch plate media with signed URLs *)
  let* media_res =
    Test_utils.assert_json_get ~handler
      ~path:
        (Printf.sprintf "/api/v1/plates/%s/media?include_signed_urls=true"
           plate_short_id)
      ~expected_status:200 "GET plate media with S3 signed URLs"
  in
  let* media_str = Dream.body media_res in
  let media_items =
    Yojson.Safe.from_string media_str |> member "media" |> to_list
  in
  Alcotest.(check (list int))
    "Plate media sorted ascending [1; 2]" [ 1; 2 ]
    (List.map (fun m -> m |> member "time" |> to_int) media_items);
  let first_signed = List.hd media_items |> member "signed_url" |> to_string in
  Alcotest.(check bool)
    "Plate media item signed with MinIO SigV4" true
    (String.starts_with
       ~prefix:"https://minio.biofoundry.org/lab-bucket/exp1/media/t1.png?"
       first_signed);

  (* 6. Verify project dashboard remains unpolluted *)
  let* dash_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/projects/%s/dashboard" project_short_id)
      ~expected_status:200 "GET project dashboard"
  in
  let* dash_str = Dream.body dash_res in
  let cols = Yojson.Safe.from_string dash_str |> member "columns" |> to_list in
  Alcotest.(check int)
    "Dashboard still has only 6 input columns" 6 (List.length cols);

  (* 7. Verify raw PostgreSQL row is encrypted with AES-256-GCM (no plaintext secret!) *)
  let raw_setting_res =
    Lwt_main.run (Exlab_storage.Setting.get_by_key "storage_connections")
  in
  (match raw_setting_res with
  | Ok (Some setting) ->
      let raw_db_json = Yojson.Safe.to_string setting.value in
      Alcotest.(check bool)
        "Postgres row is encrypted with aes-256-gcm-v1" true
        (Re.execp (Re.compile (Re.str "aes-256-gcm-v1")) raw_db_json);
      Alcotest.(check bool)
        "Postgres row does NOT store MINIO_SUPER_SECRET_KEY in plaintext" false
        (Re.execp (Re.compile (Re.str "MINIO_SUPER_SECRET_KEY")) raw_db_json)
  | _ -> Alcotest.fail "Expected storage_connections row in system_settings");

  (* Clean up test S3 setting *)
  let* _ =
    Test_utils.assert_json_put ~handler
      ~path:"/api/v1/admin/storage-connections" ~body:{|{"clear_s3": true}|}
      ~expected_status:200 "Clean up test S3 connection"
  in
  Lwt.return ()

let test_independent_versioning_and_canonical_per_tool _switch () =
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Multi-Tool Canonical Project", "status": "Active"}|}
      ~expected_status:201 "Create project for multi-tool test"
  in
  let* proj_str = Dream.body proj_res in
  let open Yojson.Safe.Util in
  let proj_json = Yojson.Safe.from_string proj_str in
  let project_id = proj_json |> member "id" |> to_int in
  let project_short_id = proj_json |> member "short_id" |> to_string in

  let* plate_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/plates"
      ~body:
        (Printf.sprintf
           {|{"name": "Multi-Tool Plate", "project_id": %d, "plate_format": "96-well"}|}
           project_id)
      ~expected_status:201 "Create plate"
  in
  let* plate_str = Dream.body plate_res in
  let plate_short_id =
    Yojson.Safe.from_string plate_str |> member "short_id" |> to_string
  in

  let runs_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs" project_short_id
  in

  (* 1. Register PhenoTypic v1 (with plate_media) and PhenoTypic v2 *)
  let* pt_v1_res =
    Test_utils.assert_json_post ~handler ~path:runs_path
      ~body:
        (Printf.sprintf
           {|{
             "tool_name": "PhenoTypic",
             "storage_base_uri": "https://example.org/pt/v1",
             "dataset_checksum": "sha256:pt_v1",
             "plate_media": {%S: [{"time": 1, "uri": "https://example.org/pt/v1/p1.png"}]}
           }|}
           plate_short_id)
      ~expected_status:201 "Register PhenoTypic v1"
  in
  let* pt_v1_str = Dream.body pt_v1_res in
  Alcotest.(check int)
    "PhenoTypic first run is v1" 1
    (Yojson.Safe.from_string pt_v1_str |> member "version_number" |> to_int);

  let* pt_v2_res =
    Test_utils.assert_json_post ~handler ~path:runs_path
      ~body:
        (Printf.sprintf
           {|{
             "tool_name": "PhenoTypic",
             "storage_base_uri": "https://example.org/pt/v2",
             "dataset_checksum": "sha256:pt_v2",
             "plate_media": {%S: [{"time": 1, "uri": "https://example.org/pt/v2/p1.png"}]}
           }|}
           plate_short_id)
      ~expected_status:201 "Register PhenoTypic v2"
  in
  let* pt_v2_str = Dream.body pt_v2_res in
  Alcotest.(check int)
    "PhenoTypic second run is v2" 2
    (Yojson.Safe.from_string pt_v2_str |> member "version_number" |> to_int);

  (* 2. Register Nextflow v1 on the SAME project *)
  let* nf_v1_res =
    Test_utils.assert_json_post ~handler ~path:runs_path
      ~body:
        {|{
          "tool_name": "Nextflow",
          "storage_base_uri": "https://example.org/nf/v1",
          "dataset_checksum": "sha256:nf_v1"
        }|}
      ~expected_status:201 "Register Nextflow v1 on same project"
  in
  let* nf_v1_str = Dream.body nf_v1_res in
  let nf_v1_json = Yojson.Safe.from_string nf_v1_str in
  Alcotest.(check int)
    "Nextflow starts its own version sequence at v1" 1
    (nf_v1_json |> member "version_number" |> to_int);
  Alcotest.(check bool)
    "Nextflow v1 is canonical for Nextflow" true
    (nf_v1_json |> member "is_canonical" |> to_bool);

  (* 3. Verify BOTH PhenoTypic v2 AND Nextflow v1 remain canonical simultaneously *)
  let* pt_can_res =
    Test_utils.assert_json_get ~handler
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/analysis-runs/canonical?tool=PhenoTypic"
           project_short_id)
      ~expected_status:200 "GET canonical PhenoTypic run"
  in
  let* pt_can_str = Dream.body pt_can_res in
  let pt_can_json = Yojson.Safe.from_string pt_can_str in
  Alcotest.(check int)
    "PhenoTypic canonical is still v2" 2
    (pt_can_json |> member "version_number" |> to_int);
  Alcotest.(check bool)
    "PhenoTypic v2 still has is_canonical = true" true
    (pt_can_json |> member "is_canonical" |> to_bool);

  let* nf_can_res =
    Test_utils.assert_json_get ~handler
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/analysis-runs/canonical?tool=Nextflow"
           project_short_id)
      ~expected_status:200 "GET canonical Nextflow run"
  in
  let* nf_can_str = Dream.body nf_can_res in
  let nf_can_json = Yojson.Safe.from_string nf_can_str in
  Alcotest.(check string)
    "Nextflow canonical checksum matches nf_v1" "sha256:nf_v1"
    (nf_can_json |> member "dataset_checksum" |> to_string);

  (* 4. Promoting PhenoTypic v1 back to canonical does NOT demote Nextflow v1 *)
  let* _ =
    Test_utils.assert_json_put ~handler
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/analysis-runs/1/canonical?tool=PhenoTypic"
           project_short_id)
      ~body:"{}" ~expected_status:200 "Promote PhenoTypic v1 to canonical"
  in
  let* nf_after_res =
    Test_utils.assert_json_get ~handler
      ~path:
        (Printf.sprintf
           "/api/v1/projects/%s/analysis-runs/canonical?tool=Nextflow"
           project_short_id)
      ~expected_status:200
      "Nextflow v1 still canonical after PhenoTypic rollback"
  in
  let* nf_after_str = Dream.body nf_after_res in
  Alcotest.(check bool)
    "Nextflow v1 remains canonical" true
    (Yojson.Safe.from_string nf_after_str |> member "is_canonical" |> to_bool);

  (* 5. Plate media still resolves PhenoTypic's media even though Nextflow was uploaded later *)
  let* media_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "/api/v1/plates/%s/media" plate_short_id)
      ~expected_status:200 "GET plate media resolves canonical run with media"
  in
  let* media_str = Dream.body media_res in
  let media_items =
    Yojson.Safe.from_string media_str |> member "media" |> to_list
  in
  Alcotest.(check int)
    "Plate media has 1 item from promoted PhenoTypic v1" 1
    (List.length media_items);
  Alcotest.(check string)
    "Plate media URI reflects promoted PhenoTypic v1"
    "https://example.org/pt/v1/p1.png"
    (List.hd media_items |> member "uri" |> to_string);
  Lwt.return ()

let suite =
  [
    ( "Analysis Runs API",
      [
        Alcotest_lwt.test_case "Register first run and fetch canonical" `Quick
          test_register_first_run_and_get_canonical;
        Alcotest_lwt.test_case "Versioning and promotion" `Quick
          test_versioning_and_promotion;
        Alcotest_lwt.test_case "Cross-project tool filtering" `Quick
          test_cross_project_tool_filtering;
        Alcotest_lwt.test_case "S3 storage connections and plate media" `Quick
          test_s3_storage_connections_and_plate_media;
        Alcotest_lwt.test_case
          "Independent versioning and canonical status per tool" `Quick
          test_independent_versioning_and_canonical_per_tool;
      ] );
  ]
