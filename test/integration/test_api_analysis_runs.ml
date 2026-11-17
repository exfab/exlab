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

let test_sync_blocks_cloud_metadata_ssrf _switch () =
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "SSRF Guard Project", "status": "Active"}|}
      ~expected_status:201 "Create project for SSRF guard test"
  in
  let* proj_str = Dream.body proj_res in
  let open Yojson.Safe.Util in
  let project_short_id =
    Yojson.Safe.from_string proj_str |> member "short_id" |> to_string
  in
  let sync_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs/sync" project_short_id
  in
  let check_blocked_endpoint endpoint_url =
    let* _ =
      Test_utils.assert_json_put ~handler
        ~path:"/api/v1/admin/storage-connections"
        ~body:
          (Printf.sprintf
             {|{"s3": {"endpoint_url": %S, "region": "us-east-1", "access_key_id": "AKIA", "secret_access_key": "SECRET"}}|}
             endpoint_url)
        ~expected_status:200 "Configure S3 endpoint"
    in
    let* res =
      Test_utils.assert_json_post ~handler ~path:sync_path
        ~body:{|{"storage_base_uri": "s3://lab-bucket/deliverables"}|}
        ~expected_status:400
        ("Sync rejects disallowed endpoint " ^ endpoint_url)
    in
    let* body_str = Dream.body res in
    let err_msg =
      Yojson.Safe.from_string body_str |> member "error" |> to_string
    in
    Alcotest.(check bool)
      ("Error mentions blocked host for " ^ endpoint_url)
      true
      (Re.execp (Re.compile (Re.no_case (Re.str "blocked"))) err_msg);
    Lwt.return ()
  in
  let* () = check_blocked_endpoint "http://0.0.0.0:1" in
  let* () = check_blocked_endpoint "http://metadata.google.internal" in
  let* () = check_blocked_endpoint "http://169.254.169.254" in
  let* _ =
    Test_utils.assert_json_put ~handler
      ~path:"/api/v1/admin/storage-connections" ~body:{|{"clear_s3": true}|}
      ~expected_status:200 "Clean up test S3 connection"
  in
  Lwt.return ()

let test_http_client_timeout_on_stalling_server _switch () =
  let res =
    Lwt_main.run
      (let listen_fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
       Lwt_unix.setsockopt listen_fd Unix.SO_REUSEADDR true;
       let* () =
         Lwt_unix.bind listen_fd
           (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 0))
       in
       Lwt_unix.listen listen_fd 4;
       let port =
         match Lwt_unix.getsockname listen_fd with
         | Unix.ADDR_INET (_, p) -> p
         | _ -> Alcotest.fail "Expected INET socket"
       in
       let accepted_fd_ref = ref None in
       let server_bg =
         let* client_fd, _ = Lwt_unix.accept listen_fd in
         accepted_fd_ref := Some client_fd;
         (* Deliberately stall without sending any HTTP bytes *)
         Lwt_unix.sleep 5.0
       in
       Lwt.async (fun () ->
           Lwt.catch (fun () -> server_bg) (fun _ -> Lwt.return ()));
       let url = Printf.sprintf "http://127.0.0.1:%d/stall" port in
       let* r =
         Lwt.pick
           [
             Exlab_server.Http_client.get_string ~timeout:0.12 url;
             (let* () = Lwt_unix.sleep 0.6 in
              Lwt.return (Error "TEST_HARNESS_HUNG"));
           ]
       in
       let* () =
         Lwt.catch (fun () -> Lwt_unix.close listen_fd) (fun _ -> Lwt.return ())
       in
       let* () =
         match !accepted_fd_ref with
         | Some cfd ->
             Lwt.catch (fun () -> Lwt_unix.close cfd) (fun _ -> Lwt.return ())
         | None -> Lwt.return ()
       in
       Lwt.return r)
  in
  match res with
  | Ok _ -> Alcotest.fail "Expected timeout error on stalling server"
  | Error err_msg ->
      Alcotest.(check bool)
        ("Error indicates request timed out (got: " ^ err_msg ^ ")")
        true
        (Re.execp (Re.compile (Re.no_case (Re.str "timed out"))) err_msg);
      Lwt.return ()

let untrusted_self_signed_cert_pem =
  {|-----BEGIN CERTIFICATE-----
MIIDJTCCAg2gAwIBAgIUYbBjDtKw5c2tq0ume1QN/DTudY0wDQYJKoZIhvcNAQEL
BQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI2MTExNjIyMzExNloXDTM2MTEx
MzIyMzExNlowFDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEF
AAOCAQ8AMIIBCgKCAQEA2pIeBYhYaat+4gfGK/vRAXt554s5NaARslJ+3ZMCzXfA
KXrXzKIyKyoUcH0ZtrmZ5mPDnLbmPdSz2gV/0zujygoeaX9nv22Fh3f038I0gtxl
zOIZOs4PfKfM6UFiCvD9W4sfFx0uJrv9tEmVZZqOzA5UezMTGUTtGLt5SzqWXvyJ
5yNbeMVLi7/RgtsmjofhR73ev1KIkOHxK+5pIu22CHyfyLM7gXCoVUwtv3Q/+dCf
LeWJZHjQEC6jboi1mLbCFS2A59N7we+62cm9GkZlFg7j1ZjfgXHYowCAID0KxCE+
XYYuqqL4seNB3HSzAvHNt6rSGN4WVUMFDG/llKUsfwIDAQABo28wbTAdBgNVHQ4E
FgQUHSKoud0hbKNLqGdqA3HXinSwV7owHwYDVR0jBBgwFoAUHSKoud0hbKNLqGdq
A3HXinSwV7owDwYDVR0TAQH/BAUwAwEB/zAaBgNVHREEEzARhwR/AAABgglsb2Nh
bGhvc3QwDQYJKoZIhvcNAQELBQADggEBAFJH/d0n6AwLH7lm62uIgHzCLjTVggva
kGjflt+GvbFcXXVnBo86pT8U87BV+lqCgXCLrZOw6iFfU0rRP0kjgoOhkTkjC9ZD
sRHaVvhtG+Oy0HZluL5PLUeJveWCcxGPRStVGNZojXSyCddWTGiTBzgdAacVONnt
CfcYhlMHbqcdbCT/+JGrd4Vh5Ip5b/TgPqJqwcuw5jbTudSNAK3zCR8xQtw/xnnm
l5XjpK2aDZNX4rkFFcqNRt6qMAq4Vi6QjBRn2KfYyJOeu/vtyjsmu+xVgU1Dqj8c
fOTPfpd4R525mPvWu8d7bDPfkMEFDCDY1pBU+JEpz9mYlRn9RtQPMw4=
-----END CERTIFICATE-----
|}

let untrusted_self_signed_key_pem =
  {|-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDakh4FiFhpq37i
B8Yr+9EBe3nnizk1oBGyUn7dkwLNd8ApetfMojIrKhRwfRm2uZnmY8OctuY91LPa
BX/TO6PKCh5pf2e/bYWHd/TfwjSC3GXM4hk6zg98p8zpQWIK8P1bix8XHS4mu/20
SZVlmo7MDlR7MxMZRO0Yu3lLOpZe/InnI1t4xUuLv9GC2yaOh+FHvd6/UoiQ4fEr
7mki7bYIfJ/IszuBcKhVTC2/dD/50J8t5YlkeNAQLqNuiLWYtsIVLYDn03vB77rZ
yb0aRmUWDuPVmN+BcdijAIAgPQrEIT5dhi6qovix40HcdLMC8c23qtIY3hZVQwUM
b+WUpSx/AgMBAAECggEABK1eQdTJAOVZkVBZ3+sDKc7bdZmQRL6auL3wlv7IqxHN
p60MhcFHdmyJRE2CGJd8HLfr4aibYKXyn7m4VaN8pmzwzsxEy0EyTJxD+beQjgnN
KAUlZ81PP/+cdILYG1vmPnVJprubOWOSimJadXKV9aBgvl2CEVpUis/qFXbfU112
LlqC/bc5sE3OXKm7v0OZpohLTynXHeTCWug++5aEv1tlcrIVRA9eSETPxNOxMADl
4K+LFEBpWcNfMKsF+jRAjfPtmCH/H4gDqdHpWgvQP8WiAQAt5vcV0GCsqxAcJq6b
4yyW9f+F9aEqGi/iLezM4dYcshaOeuM6FjJ2H0RXEQKBgQDwWxhRTvJH8BBT6zVg
SSeww83KMuaR23EP+3MjQ7KFeiesA/OVYvV5XPnp9dvQ09jPB745evq0GLjc3NcK
q050vVItOvZtx8xAfb1PNtsVetmb6OnSH2oNq7WCXsAgJheh8GPt5/gaTnhWHlxx
Y3CC9AbkSxAqH7TfF33HzUjUjwKBgQDozAffpRQKQmmMK7f6Wy485EfArd9Lxz/B
X2sdKM6EaDcygkxmdzOVZ5ScdVR6YBRGRHnHJechTrjhN/9faWUxv5Kvhjcj1Y6M
/ZnV/ITIP+Fx3bf1rl70DnDrtTzIFQ4EDRqPxy9mf6oUdsEycLYsK4Agg3JE50Gu
UNiG3J2BEQKBgFtrWpl6fJ6uCZfRq7CewpDyOLDCoX4yyBM7IbT1B87LApHjwt88
D4AExC2fzObl3N4J5Ol6BCdCC/Y9rTEqrSGr7Li1Z4uj5eVVrg6WH8hQvLoXYfAA
8NaYS+Z1YYAY3P5EL84KYAypcg3XaHPcTawRYxmsD07mVmZQShAX3a6zAoGBAISW
YA2iaY2GUbMB22D9uZH4ul0lKj5wlwE9pTYQXb7P1FAyIe1iCsEXI28Q7DJesm1q
5qjkD4XK3kV3rTzcPERfRd4uZQ1FvlDSx2E21zFuIKrHitSkoXQDHZFBwgJ662an
R++HlJmeMHJDSLm/tCHoDKt38gcIZ0IuzYPR9urhAoGBAIiplZvafjWe8q6K4VMx
p5XjFacUUP0V+HhM0CBqgBG1Q4/TBYSmPDzFlEYifRm7I7511F6E544j0Ha0z0BM
3cMtgBmBbG6FYlgkWw5STap65KwOQ3Uz26mufLZa6jaukLz3u0lY3Qf9WlN04u68
tvNZYH8eNPYP3w2ynw2KZfKx
-----END PRIVATE KEY-----
|}

let write_temp_pem prefix contents =
  let path = Filename.temp_file prefix ".pem" in
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents);
  path

let test_http_client_rejects_untrusted_tls_cert _switch () =
  let cert_file =
    write_temp_pem "exlab_test_cert_" untrusted_self_signed_cert_pem
  in
  let key_file =
    write_temp_pem "exlab_test_key_" untrusted_self_signed_key_pem
  in
  let res =
    Lwt_main.run
      (let server_ctx =
         Ssl.create_context (Ssl.SSLv23 [@alert "-deprecated"])
           Ssl.Server_context
       in
       Ssl.use_certificate server_ctx cert_file key_file;
       let listen_fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
       Lwt_unix.setsockopt listen_fd Unix.SO_REUSEADDR true;
       let* () =
         Lwt_unix.bind listen_fd
           (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 0))
       in
       Lwt_unix.listen listen_fd 4;
       let port =
         match Lwt_unix.getsockname listen_fd with
         | Unix.ADDR_INET (_, p) -> p
         | _ -> Alcotest.fail "Expected INET socket"
       in
       let server_bg =
         let* client_fd, _ = Lwt_unix.accept listen_fd in
         Lwt.finalize
           (fun () ->
             let* ssl_sock = Lwt_ssl.ssl_accept client_fd server_ctx in
             let resp =
               Bytes.of_string
                 "HTTP/1.1 200 OK\r\n\
                  Content-Length: 2\r\n\
                  Connection: close\r\n\
                  \r\n\
                  OK"
             in
             let* _ = Lwt_ssl.write ssl_sock resp 0 (Bytes.length resp) in
             Lwt_ssl.ssl_shutdown ssl_sock)
           (fun () ->
             Lwt.catch
               (fun () -> Lwt_unix.close client_fd)
               (fun _ -> Lwt.return ()))
       in
       Lwt.async (fun () ->
           Lwt.catch (fun () -> server_bg) (fun _ -> Lwt.return ()));
       let url = Printf.sprintf "https://127.0.0.1:%d/untrusted" port in
       let* r = Exlab_server.Http_client.get_string ~timeout:2.0 url in
       let* () =
         Lwt.catch (fun () -> Lwt_unix.close listen_fd) (fun _ -> Lwt.return ())
       in
       Lwt.return r)
  in
  (try Sys.remove cert_file with _ -> ());
  (try Sys.remove key_file with _ -> ());
  match res with
  | Ok (status, body) ->
      Alcotest.fail
        (Printf.sprintf
           "Expected TLS verification failure on self-signed cert, got HTTP %d \
            (%s)"
           status body)
  | Error _ -> Lwt.return ()

let test_delete_non_canonical_run _switch () =
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Delete Run Project", "status": "Active"}|}
      ~expected_status:201 "Create project for delete test"
  in
  let* proj_str = Dream.body proj_res in
  let open Yojson.Safe.Util in
  let project_short_id =
    Yojson.Safe.from_string proj_str |> member "short_id" |> to_string
  in
  let runs_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs" project_short_id
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:runs_path
      ~body:
        {|{"tool_name": "PhenoTypic", "storage_base_uri": "https://example.org/del/v1", "dataset_checksum": "sha256:del1"}|}
      ~expected_status:201 "Register v1"
  in
  let* _ =
    Test_utils.assert_json_post ~handler ~path:runs_path
      ~body:
        {|{"tool_name": "PhenoTypic", "storage_base_uri": "https://example.org/del/v2", "dataset_checksum": "sha256:del2"}|}
      ~expected_status:201 "Register v2"
  in
  let v1_path = Printf.sprintf "%s/1" runs_path in
  let* _ =
    Test_utils.assert_json_delete ~handler ~path:v1_path ~expected_status:204
      "DELETE non-canonical v1 returns 204"
  in
  let* _ =
    Test_utils.assert_json_get ~handler ~path:v1_path ~expected_status:404
      "GET deleted v1 returns 404"
  in
  let* _ =
    Test_utils.assert_json_delete ~handler ~path:v1_path ~expected_status:404
      "DELETE already-deleted v1 returns 404"
  in
  let* list_res =
    Test_utils.assert_json_get ~handler ~path:runs_path ~expected_status:200
      "GET remaining runs"
  in
  let* list_str = Dream.body list_res in
  let remaining =
    Yojson.Safe.from_string list_str |> member "data" |> to_list
  in
  Alcotest.(check int) "1 run remains" 1 (List.length remaining);
  Alcotest.(check int)
    "Remaining run preserves version_number = 2" 2
    (List.hd remaining |> member "version_number" |> to_int);
  Alcotest.(check bool)
    "Remaining v2 is still canonical" true
    (List.hd remaining |> member "is_canonical" |> to_bool);
  Lwt.return ()

let test_delete_canonical_run_promotes_fallback_per_tool _switch () =
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Canonical Fallback Project", "status": "Active"}|}
      ~expected_status:201 "Create project for canonical fallback test"
  in
  let* proj_str = Dream.body proj_res in
  let open Yojson.Safe.Util in
  let project_short_id =
    Yojson.Safe.from_string proj_str |> member "short_id" |> to_string
  in
  let runs_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs" project_short_id
  in
  let register tool tag =
    Test_utils.assert_json_post ~handler ~path:runs_path
      ~body:
        (Printf.sprintf
           {|{"tool_name": %S, "storage_base_uri": "https://example.org/%s", "dataset_checksum": "sha256:%s"}|}
           tool tag tag)
      ~expected_status:201 ("Register " ^ tag)
  in
  let* _ = register "PhenoTypic" "pt_v1" in
  let* _ = register "PhenoTypic" "pt_v2" in
  let* _ = register "PhenoTypic" "pt_v3" in
  let* _ = register "Nextflow" "nf_v1" in

  (* Delete canonical PhenoTypic v3 -> PhenoTypic v2 must automatically become canonical *)
  let* _ =
    Test_utils.assert_json_delete ~handler
      ~path:(Printf.sprintf "%s/3?tool=PhenoTypic" runs_path)
      ~expected_status:204 "DELETE canonical PhenoTypic v3"
  in
  let* pt_can_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "%s/canonical?tool=PhenoTypic" runs_path)
      ~expected_status:200 "PhenoTypic automatically falls back to v2"
  in
  let* pt_can_str = Dream.body pt_can_res in
  let pt_can_json = Yojson.Safe.from_string pt_can_str in
  Alcotest.(check int)
    "Fallback canonical is PhenoTypic v2" 2
    (pt_can_json |> member "version_number" |> to_int);
  Alcotest.(check bool)
    "PhenoTypic v2 has is_canonical = true" true
    (pt_can_json |> member "is_canonical" |> to_bool);

  (* Nextflow v1 is completely unaffected *)
  let* _ =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "%s/canonical?tool=Nextflow" runs_path)
      ~expected_status:200 "Nextflow v1 remains canonical"
  in

  (* Delete PhenoTypic v2 -> falls back to v1; delete v1 -> 404 for PhenoTypic, 200 for Nextflow *)
  let* _ =
    Test_utils.assert_json_delete ~handler
      ~path:(Printf.sprintf "%s/2?tool=PhenoTypic" runs_path)
      ~expected_status:204 "DELETE PhenoTypic v2"
  in
  let* pt_v1_can_res =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "%s/canonical?tool=PhenoTypic" runs_path)
      ~expected_status:200 "PhenoTypic falls back to v1"
  in
  let* pt_v1_can_str = Dream.body pt_v1_can_res in
  Alcotest.(check int)
    "Fallback canonical is PhenoTypic v1" 1
    (Yojson.Safe.from_string pt_v1_can_str |> member "version_number" |> to_int);

  let* _ =
    Test_utils.assert_json_delete ~handler
      ~path:(Printf.sprintf "%s/1?tool=PhenoTypic" runs_path)
      ~expected_status:204 "DELETE final PhenoTypic v1"
  in
  let* _ =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "%s/canonical?tool=PhenoTypic" runs_path)
      ~expected_status:404 "No canonical PhenoTypic run left"
  in
  let* _ =
    Test_utils.assert_json_get ~handler
      ~path:(Printf.sprintf "%s/canonical?tool=Nextflow" runs_path)
      ~expected_status:200 "Nextflow v1 STILL canonical"
  in
  Lwt.return ()

let make_session_app ~user_id ~role_str =
  let mw next_handler request =
    let* () = Dream.set_session_field request "user_role" role_str in
    let* () =
      Dream.set_session_field request "user_id" (string_of_int user_id)
    in
    next_handler request
  in
  Dream.memory_sessions @@ mw
  @@ Dream.router
       [ Dream.scope "/" [ Exlab_server.Auth.auth_required ] Test_utils.routes ]

let test_delete_run_permissions_managers_and_creator _switch () =
  let open Yojson.Safe.Util in
  let suffix =
    Printf.sprintf "%d_%d"
      (int_of_float (Unix.gettimeofday () *. 1000.0))
      (Random.int 10000)
  in
  let create_and_assign_user ~project_short_id ~prefix ~role_str =
    let email = Printf.sprintf "%s_%s@exlab.com" prefix suffix in
    let* create_res =
      Test_utils.assert_json_post ~handler ~path:"/api/v1/users"
        ~body:
          (Printf.sprintf
             {|{"email": %S, "password": "password123", "role": %S}|} email
             role_str)
        ~expected_status:201 ("Create user " ^ prefix)
    in
    let* create_str = Dream.body create_res in
    let uid = Yojson.Safe.from_string create_str |> member "id" |> to_int in
    let* _ =
      Test_utils.assert_json_post ~handler
        ~path:(Printf.sprintf "/api/v1/projects/%s/users" project_short_id)
        ~body:(Printf.sprintf {|{"email": %S}|} email)
        ~expected_status:204
        ("Assign " ^ prefix ^ " to project")
    in
    Lwt.return (make_session_app ~user_id:uid ~role_str)
  in
  let* proj_res =
    Test_utils.assert_json_post ~handler ~path:"/api/v1/projects"
      ~body:{|{"name": "Delete Permissions Project", "status": "Active"}|}
      ~expected_status:201 "Create project for delete permission test"
  in
  let* proj_str = Dream.body proj_res in
  let project_short_id =
    Yojson.Safe.from_string proj_str |> member "short_id" |> to_string
  in
  let* creator_app =
    create_and_assign_user ~project_short_id ~prefix:"creator"
      ~role_str:"project_user"
  in
  let* bystander_app =
    create_and_assign_user ~project_short_id ~prefix:"bystander"
      ~role_str:"project_user"
  in
  let* manager_app =
    create_and_assign_user ~project_short_id ~prefix:"manager"
      ~role_str:"project_manager"
  in
  let runs_path =
    Printf.sprintf "/api/v1/projects/%s/analysis-runs" project_short_id
  in

  (* Creator (project_user) registers v1 and v2 *)
  let* _ =
    Test_utils.assert_json_post ~handler:creator_app ~path:runs_path
      ~body:
        {|{"tool_name": "PhenoTypic", "storage_base_uri": "https://example.org/perm/v1", "dataset_checksum": "sha256:p1"}|}
      ~expected_status:201 "Creator registers v1"
  in
  let* _ =
    Test_utils.assert_json_post ~handler:creator_app ~path:runs_path
      ~body:
        {|{"tool_name": "PhenoTypic", "storage_base_uri": "https://example.org/perm/v2", "dataset_checksum": "sha256:p2"}|}
      ~expected_status:201 "Creator registers v2"
  in

  (* Another project_user who did NOT create v1 gets 403 Forbidden *)
  let* _ =
    Test_utils.assert_json_delete ~handler:bystander_app
      ~path:(Printf.sprintf "%s/1" runs_path)
      ~expected_status:403
      "Non-creator project_user cannot DELETE another user's run"
  in

  (* Creator project_user CAN delete their own run v1 -> 204 *)
  let* _ =
    Test_utils.assert_json_delete ~handler:creator_app
      ~path:(Printf.sprintf "%s/1" runs_path)
      ~expected_status:204 "Creator project_user can DELETE their own run"
  in

  (* Project_manager on the project CAN delete v2 even though creator registered it -> 204 *)
  let* _ =
    Test_utils.assert_json_delete ~handler:manager_app
      ~path:(Printf.sprintf "%s/2" runs_path)
      ~expected_status:204
      "Project_manager can DELETE any run in assigned project"
  in
  Lwt.return ()

let test_encryption_key_decoupled_from_dream_secret _switch () =
  let open Yojson.Safe.Util in
  let prev_enc =
    Option.value ~default:"" (Sys.getenv_opt "EXLAB_ENCRYPTION_KEY")
  in
  let prev_dream = Option.value ~default:"" (Sys.getenv_opt "DREAM_SECRET") in
  Lwt.finalize
    (fun () ->
      (* 1. Deployed server with DREAM_SECRET set but EXLAB_ENCRYPTION_KEY missing must NOT piggyback on DREAM_SECRET *)
      Unix.putenv "EXLAB_ENCRYPTION_KEY" "";
      Unix.putenv "DREAM_SECRET" "cookie-only-secret-v1";
      let* put_missing_res =
        Test_utils.assert_json_put ~handler
          ~path:"/api/v1/admin/storage-connections"
          ~body:
            {|{"s3": {"endpoint_url": "https://s3.example.org", "region": "us-west-2", "access_key_id": "AKIA1", "secret_access_key": "SEC1"}}|}
          ~expected_status:400
          "PUT fails when DREAM_SECRET is set without EXLAB_ENCRYPTION_KEY"
      in
      let* err_str = Dream.body put_missing_res in
      let err_msg =
        Yojson.Safe.from_string err_str |> member "error" |> to_string
      in
      Alcotest.(check bool)
        "Error names EXLAB_ENCRYPTION_KEY" true
        (Re.execp (Re.compile (Re.str "EXLAB_ENCRYPTION_KEY")) err_msg);

      (* 2. With EXLAB_ENCRYPTION_KEY set, rotating DREAM_SECRET never invalidates stored credentials *)
      Unix.putenv "EXLAB_ENCRYPTION_KEY" "dedicated-db-master-key-v1";
      let* _ =
        Test_utils.assert_json_put ~handler
          ~path:"/api/v1/admin/storage-connections"
          ~body:
            {|{"s3": {"endpoint_url": "https://s3.example.org", "region": "us-west-2", "access_key_id": "AKIA1", "secret_access_key": "SEC1"}}|}
          ~expected_status:200
          "PUT succeeds with dedicated EXLAB_ENCRYPTION_KEY"
      in
      Unix.putenv "DREAM_SECRET" "rotated-cookie-secret-v2";
      let* get_after_rotate_res =
        Test_utils.assert_json_get ~handler
          ~path:"/api/v1/admin/storage-connections" ~expected_status:200
          "GET storage connections after DREAM_SECRET rotation"
      in
      let* get_str = Dream.body get_after_rotate_res in
      let configured =
        Yojson.Safe.from_string get_str
        |> member "s3" |> member "configured" |> to_bool
      in
      Alcotest.(check bool)
        "S3 connection still decrypts after DREAM_SECRET rotation" true
        configured;

      let* _ =
        Test_utils.assert_json_put ~handler
          ~path:"/api/v1/admin/storage-connections" ~body:{|{"clear_s3": true}|}
          ~expected_status:200 "Clear test S3 connection"
      in
      Lwt.return ())
    (fun () ->
      Unix.putenv "EXLAB_ENCRYPTION_KEY" prev_enc;
      Unix.putenv "DREAM_SECRET" prev_dream;
      Lwt.return ())

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
        Alcotest_lwt.test_case "Sync blocks cloud metadata SSRF endpoints"
          `Quick test_sync_blocks_cloud_metadata_ssrf;
        Alcotest_lwt.test_case "Http_client times out cleanly on stalled socket"
          `Quick test_http_client_timeout_on_stalling_server;
        Alcotest_lwt.test_case
          "Http_client rejects untrusted self-signed TLS certificate" `Quick
          test_http_client_rejects_untrusted_tls_cert;
        Alcotest_lwt.test_case "Delete non-canonical run and 404 on missing"
          `Quick test_delete_non_canonical_run;
        Alcotest_lwt.test_case
          "Delete canonical run promotes previous version per tool" `Quick
          test_delete_canonical_run_promotes_fallback_per_tool;
        Alcotest_lwt.test_case
          "Delete run permissions enforce managers or run creator" `Quick
          test_delete_run_permissions_managers_and_creator;
        Alcotest_lwt.test_case
          "EXLAB_ENCRYPTION_KEY is decoupled from DREAM_SECRET" `Quick
          test_encryption_key_decoupled_from_dream_secret;
      ] );
  ]
