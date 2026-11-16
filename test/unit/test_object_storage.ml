open Exlab_core

let test_sa_json =
  {|{
  "type": "service_account",
  "project_id": "exfab-test",
  "client_email": "exlab-bucket-reader@exfab-test.iam.gserviceaccount.com",
  "private_key": "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDYKOvJEwysRtSk\nl/dXmr3r87ZwQJEhQ/PSukUVr6O9X3w38Nx4eCYU+y/dqE46Jj4CeyTtA7ju38im\nhO3xzvcJCJQ/VOJ3zSIUDyzlWOuWrP3VnZs1XP63dTN79IGEqJwMmlqjinjxJjkW\ng1WS2NFAyJ1Gr/M4lHOglDTusSj+440sDrdMUS00X/yeoBqTEDqhnDbiTZwyyQ7f\nEecPjEa0xc3JYam63knUAlY8i73YvLs343xAStJaqwpVSIoD/bv+hQC6Q3/zpt5f\ndgestvFP/0Yd9kv1g9mMlEQ2C9+k7DFHRZg5GytGGyUYINDgg3DdGK53U7XmePso\nOzYJY3CXAgMBAAECggEAFXOO93s5FIoW8jQ7XjZ24JXLP73FaVK2cLqX8bxmq68J\n9pc9mer6hILraVWzJbKQqgUq6sAmBMN9xLpUzsMMrjKewhMCqOol/Qc6G/leFkVN\nD3tYYTK9Lts/sVTYQaAyx4FniB4aChYSYxL3iN77E5dh6RdVZzXV37q1iHaMxfad\nfSCbM3rLLdaeHL9bLK500FkaZ0A4RjcWmxhUFk4KfsaDVgXU1QaaBA6mNzn9XVFL\nQWjQgebO4CiKpLALqSn/tQ7KjaZMdcPv8a2DWkeIapb+2qXnnH1k9awiDm88f9Kw\n2dqZW3yEC/eERsjPs7f6GtXfsDSfomOcy4+vW7p98QKBgQD74XZTR21VgnpWlMwS\nNjKq0mKmSXMvtQfCh6YVrDQmI6j0qvYcMaHyRC5Q1x+CG6YS6zvJ3xhBilj9EqD5\nE285Tm+LrNwe5OcYkv901X0WZveg+s2W4Dws8ybpgBnfpNo3RGM2QiooiySUla83\ndOh1MFt4e5LukfsfMT5Gn9y82QKBgQDbsehqaHWWoQDRYn/XXSvZJdjnC3st9cMf\nrrZIbJLMs85BHV3USk2UriJS/Zw7MvdhulZc/e5OQS1r/KJ3iQqLQjCx7WqiSwMH\nijMZRtIWFuzOfSo1aPhr7vbS1/dA4bD6lIs8Ut0YqMuWeQhp7++eFItbxHYZqITZ\nuONUZ9ry7wKBgQC03RGM/SYKF7XnA9OBZD9/n7BMfUNjx1b6HVNqDownssV4YmZ0\nmkMSZljnnYE3eMkInrcA3txY+5Gorz69e3ILMUIxhn9NA8rZP3yFlWLLukYn5s/H\nfd5X3IVdd6mciJBUzRt8c4g4Mqmis9TsOltNAts7qWg5PMOYg1CXf+UOCQKBgEpj\nqE33hvF1HODJWH5lBwblINfC9HewgtyEfSZy1RZJP+qedfn/GtvgRrERPMhyzn8h\n6EJenUWUHjw1dBHDrnVO+UwWFwhTG/h+nCTP7S0boB2fFh61p4Bsjtj/1bH7ecCv\ncaIAcOr7pMwTuXydiOZfEH0HPfIcNLfAOgGHlQSzAoGAZLCajOf3Y4Z0fCSk+EvE\nRrFZ8ur9efpRU2lqImjQ9nrzKj7oEykiwFRCKV8LRsVmb8ReCpvmfhag35k2H/Q4\nhG52EqC2vF9AWx8FixEv4pvjRePSSYEH5ViagmVuH4ODjM5qNvDC0ZeeYwBHqAJS\n6c0XL+hERyIMIyrw9ZoLjEQ=\n-----END PRIVATE KEY-----\n"
}|}

let test_https_passthrough () =
  let res =
    Object_storage.resolve_download_url ~config:Object_storage.empty_config
      ~uri:"https://example.org/datasets/run1/measurements.parquet"
      ~now:1787997600.0 ~expires_in:900
  in
  Alcotest.(check (result string string))
    "HTTPS URLs pass straight through without cloud config"
    (Ok "https://example.org/datasets/run1/measurements.parquet") res

let test_gcs_signing () =
  match Object_storage.gcs_credentials_of_json_string test_sa_json with
  | Error e -> Alcotest.fail e
  | Ok gcs -> (
      let config = { Object_storage.empty_config with gcs = Some gcs } in
      let res =
        Object_storage.resolve_download_url ~config
          ~uri:"gs://exfab-data/UCR-029/MeasureShape.parquet" ~now:1787997600.0
          ~expires_in:900
      in
      match res with
      | Error e -> Alcotest.fail e
      | Ok url ->
          Alcotest.(check bool)
            "GCS URL uses GOOG4-RSA-SHA256" true
            (Re.execp
               (Re.compile (Re.str "X-Goog-Algorithm=GOOG4-RSA-SHA256"))
               url))

let test_s3_minio_sigv4_signing () =
  let s3 : Object_storage.s3_credentials =
    {
      endpoint_url = "https://minio.biofoundry.org";
      region = "us-east-1";
      access_key_id = "AKIAIOSFODNN7EXAMPLE";
      secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY";
    }
  in
  let config = { Object_storage.empty_config with s3 = Some s3 } in
  let res =
    Object_storage.resolve_download_url ~config
      ~uri:"s3://lims-bucket/proj-1/measurements.parquet" ~now:1787997600.0
      ~expires_in:900
  in
  match res with
  | Error e -> Alcotest.fail ("S3 signing failed: " ^ e)
  | Ok url ->
      Alcotest.(check bool)
        "S3/MinIO URL targets custom endpoint + bucket/object" true
        (String.starts_with
           ~prefix:
             "https://minio.biofoundry.org/lims-bucket/proj-1/measurements.parquet?"
           url);
      Alcotest.(check bool)
        "Contains AWS4-HMAC-SHA256 algorithm" true
        (Re.execp (Re.compile (Re.str "X-Amz-Algorithm=AWS4-HMAC-SHA256")) url);
      Alcotest.(check bool)
        "Contains 64-hex-char HMAC-SHA256 signature" true
        (Re.execp (Re.Perl.compile_pat "X-Amz-Signature=[0-9a-f]{64}$") url)

let test_mask_secrets_in_summary () =
  let raw_json =
    Yojson.Safe.from_string
      (Printf.sprintf
         {|{
           "gcs_service_account_json": %s,
           "s3": {
             "endpoint_url": "https://minio.local",
             "region": "us-east-1",
             "access_key_id": "MINIOKEY123",
             "secret_access_key": "SUPER_SECRET_DO_NOT_LEAK"
           }
         }|}
         (Yojson.Safe.to_string (`String test_sa_json)))
  in
  let summary_json =
    Object_storage.masked_summary_of_stored_json ~env_gcs_email:None
      (Some raw_json)
  in
  let serialized = Yojson.Safe.to_string summary_json in
  Alcotest.(check bool)
    "Summary never contains RSA PRIVATE KEY" false
    (Re.execp (Re.compile (Re.str "PRIVATE KEY")) serialized);
  Alcotest.(check bool)
    "Summary never contains S3 secret_access_key" false
    (Re.execp (Re.compile (Re.str "SUPER_SECRET_DO_NOT_LEAK")) serialized);
  Alcotest.(check bool)
    "Summary exposes non-sensitive client_email" true
    (Re.execp
       (Re.compile
          (Re.str "exlab-bucket-reader@exfab-test.iam.gserviceaccount.com"))
       serialized)

let test_aes_gcm_encryption_at_rest () =
  let raw_json =
    Yojson.Safe.from_string
      (Printf.sprintf
         {|{
           "gcs_service_account_json": %s,
           "s3": {
             "endpoint_url": "https://minio.local",
             "region": "us-east-1",
             "access_key_id": "MINIOKEY123",
             "secret_access_key": "SUPER_SECRET_DO_NOT_LEAK"
           }
         }|}
         (Yojson.Safe.to_string (`String test_sa_json)))
  in
  let master_secret = "prod-dream-secret-key-material" in
  let nonce = "0123456789ab" in
  let sealed = Object_storage.seal_stored_json ~master_secret ~nonce raw_json in
  let db_str = Yojson.Safe.to_string sealed in
  Alcotest.(check bool)
    "Database JSON uses aes-256-gcm-v1 envelope" true
    (Re.execp (Re.compile (Re.str "aes-256-gcm-v1")) db_str);
  Alcotest.(check bool)
    "Database JSON does NOT contain RSA PRIVATE KEY" false
    (Re.execp (Re.compile (Re.str "PRIVATE KEY")) db_str);
  Alcotest.(check bool)
    "Database JSON does NOT contain S3 secret_access_key" false
    (Re.execp (Re.compile (Re.str "SUPER_SECRET_DO_NOT_LEAK")) db_str);

  let opened = Object_storage.open_stored_json ~master_secret (Some sealed) in
  Alcotest.(check bool)
    "Decrypts successfully with matching master_secret" true
    (Option.is_some opened);
  let wrong_key =
    Object_storage.open_stored_json ~master_secret:"wrong-secret" (Some sealed)
  in
  Alcotest.(check bool)
    "Refuses to decrypt with wrong master_secret (GCM auth tag mismatch)" true
    (Option.is_none wrong_key)

let test_parse_list_bucket_xml () =
  let sample_xml =
    {|<?xml version='1.0' encoding='UTF-8'?>
      <ListBucketResult xmlns='http://doc.s3.amazonaws.com/2006-03-01'>
        <Name>exfab-data</Name>
        <Prefix>data/UCR-033/deliverables/</Prefix>
        <IsTruncated>true</IsTruncated>
        <NextContinuationToken>tok_page_2</NextContinuationToken>
        <Contents>
          <Key>data/UCR-033/deliverables/.pipeline.lock</Key>
          <ETag>"d41d8cd98f00b204e9800998ecf8427e"</ETag>
          <Size>0</Size>
        </Contents>
        <Contents>
          <Key>data/UCR-033/deliverables/measurements.parquet</Key>
          <ETag>"ab12cd34ef56"</ETag>
          <Size>92897974</Size>
        </Contents>
      </ListBucketResult>|}
  in
  let page = Object_storage.parse_list_bucket_xml sample_xml in
  Alcotest.(check (option string))
    "Extracts NextContinuationToken when IsTruncated=true" (Some "tok_page_2")
    page.next_continuation_token;
  Alcotest.(check int)
    "Extracts 2 Contents entries" 2 (List.length page.objects);
  let second = List.nth page.objects 1 in
  Alcotest.(check string)
    "Parses object key" "data/UCR-033/deliverables/measurements.parquet"
    second.key;
  Alcotest.(check string) "Strips quotes from ETag" "ab12cd34ef56" second.etag;
  Alcotest.(check int) "Parses object size" 92897974 second.size

let suite =
  [
    ( "Object Storage",
      [
        Alcotest.test_case "HTTPS passthrough" `Quick test_https_passthrough;
        Alcotest.test_case "GCS V4 signing" `Quick test_gcs_signing;
        Alcotest.test_case "S3 / MinIO SigV4 signing" `Quick
          test_s3_minio_sigv4_signing;
        Alcotest.test_case "Mask secrets in summary" `Quick
          test_mask_secrets_in_summary;
        Alcotest.test_case "AES-256-GCM encryption at rest" `Quick
          test_aes_gcm_encryption_at_rest;
        Alcotest.test_case "Parse ListBucketResult XML" `Quick
          test_parse_list_bucket_xml;
      ] );
  ]
