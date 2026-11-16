open Exlab_core

let media_point_testable =
  Alcotest.testable Analysis_run.pp_media_point Analysis_run.equal_media_point

let test_validate_create () =
  Alcotest.(check (result (pair string (pair string string)) string))
    "Trims and validates tool_name, storage_base_uri, and dataset_checksum"
    (Ok
       ( "PhenoTypic",
         ("gs://exfab/deliverables", "md5:GGtzyFMj+5yU4wGXM4i+KA==") ))
    (Analysis_run.validate_create ~tool_name:"  PhenoTypic  "
       ~storage_base_uri:"  gs://exfab/deliverables  "
       ~dataset_checksum:" md5:GGtzyFMj+5yU4wGXM4i+KA== ");
  Alcotest.(check bool)
    "Rejects empty storage_base_uri" true
    (Stdlib.Result.is_error
       (Analysis_run.validate_create ~tool_name:"PhenoTypic"
          ~storage_base_uri:"  " ~dataset_checksum:"md5:abc"));
  Alcotest.(check bool)
    "Rejects empty dataset_checksum" true
    (Stdlib.Result.is_error
       (Analysis_run.validate_create ~tool_name:"PhenoTypic"
          ~storage_base_uri:"s3://bucket/run" ~dataset_checksum:""))

let test_extract_plate_media () =
  let json =
    Yojson.Safe.from_string
      {|{
        "P-8-0209": [
          {"time": 3, "uri": "s3://lab/t3.png", "label": "72h", "media_type": "image"},
          {"time": 1, "uri": "s3://lab/t1.png"}
        ]
      }|}
  in
  let extracted =
    Analysis_run.extract_plate_media ~plate_short_id:"P-8-0209" (Some json)
  in
  Alcotest.(check (list media_point_testable))
    "Sorts media ascending by time and preserves optional label/media_type"
    [
      {
        time = 1;
        uri = "s3://lab/t1.png";
        label = None;
        media_type = None;
        signed_url = None;
      };
      {
        time = 3;
        uri = "s3://lab/t3.png";
        label = Some "72h";
        media_type = Some "image";
        signed_url = None;
      };
    ]
    extracted

let test_build_manifest_from_objects () =
  let objs : Object_storage.listed_object list =
    [
      {
        key = "data/UCR-033/deliverables/.pipeline-config.migration.lock";
        etag = "d41d8";
        size = 0;
      };
      {
        key = "data/UCR-033/deliverables/measurements.parquet";
        etag = "etag_meas";
        size = 50000;
      };
      {
        key =
          "data/UCR-033/deliverables/measurements_by_feature/MeasureShape.parquet";
        etag = "etag_shape";
        size = 10000;
      };
      {
        key = "data/UCR-033/deliverables/pipeline.json.pht-pipe";
        etag = "etag_pipe";
        size = 1200;
      };
      {
        key = "data/UCR-033/deliverables/overlays/ds1/img_02.png";
        etag = "etag_i2";
        size = 2000;
      };
      {
        key = "data/UCR-033/deliverables/overlays/ds1/img_01.png";
        etag = "etag_i1";
        size = 2000;
      };
    ]
  in
  let pipe_json =
    Yojson.Safe.from_string
      {|{"name": "UCR_033_GANODERMA_v1", "version": "0.18.1"}|}
  in
  let metadata_csv =
    "Metadata_ImageName,Metadata_Dataset,Metadata_PlateID,Metadata_SampleID\n\
     img_01,ds1,P-9-0001,UCR-033-0001-01\n\
     img_02,ds1,P-9-0001,UCR-033-0001-01\n"
  in
  let manifest =
    Analysis_run.build_manifest_from_objects
      ~folder_uri:"gs://exfab-data/data/UCR-033/deliverables"
      ~pipeline_config:pipe_json ~metadata_csv objs
  in
  Alcotest.(check string)
    "Auto-detects PhenoTypic from pipeline.json.pht-pipe" "PhenoTypic"
    manifest.tool_name;
  Alcotest.(check (option string))
    "Extracts pipeline_name from pipeline config" (Some "UCR_033_GANODERMA_v1")
    manifest.pipeline_name;
  Alcotest.(check (option string))
    "Extracts tool_version from pipeline config" (Some "0.18.1")
    manifest.tool_version;
  let open Yojson.Safe.Util in
  let artifact_keys = manifest.artifacts |> to_assoc |> List.map fst in
  Alcotest.(check bool)
    "Skips hidden .lock files" false
    (List.exists (fun k -> String.contains k '.') artifact_keys
    && List.exists
         (fun (_, v) -> Re.execp (Re.compile (Re.str ".lock")) (to_string v))
         (to_assoc manifest.artifacts));
  Alcotest.(check bool)
    "Excludes raw overlays/*.png from root artifacts tree" false
    (List.exists
       (fun (_, v) -> Re.execp (Re.compile (Re.str "img_01.png")) (to_string v))
       (to_assoc manifest.artifacts));
  let p_media =
    Analysis_run.extract_plate_media ~plate_short_id:"P-9-0001"
      manifest.plate_media
  in
  Alcotest.(check int)
    "Maps 2 overlay images onto plate P-9-0001" 2 (List.length p_media);
  Alcotest.(check string)
    "First media item is chronologically img_01.png"
    "gs://exfab-data/data/UCR-033/deliverables/overlays/ds1/img_01.png"
    (List.hd p_media).uri

let suite =
  [
    ( "Analysis Run Core",
      [
        Alcotest.test_case "Validate create" `Quick test_validate_create;
        Alcotest.test_case "Extract and sort plate media" `Quick
          test_extract_plate_media;
        Alcotest.test_case "Build manifest from bucket objects" `Quick
          test_build_manifest_from_objects;
      ] );
  ]
