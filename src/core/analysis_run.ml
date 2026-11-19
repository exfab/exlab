(** Pure domain logic for versioned Analysis Runs and Plate Media. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type media_point = {
  time : int;
  uri : string;
  label : string option; [@yojson.option]
  media_type : string option; [@yojson.option]
  signed_url : string option; [@yojson.option]
}
[@@deriving yojson, show, eq]

let validate_create ~tool_name ~storage_base_uri ~dataset_checksum =
  let tool =
    let t = String.trim tool_name in
    if t = "" then "Custom" else t
  in
  let base_uri = String.trim storage_base_uri in
  let checksum = String.trim dataset_checksum in
  if base_uri = "" then Error "storage_base_uri cannot be empty"
  else if checksum = "" then Error "dataset_checksum cannot be empty"
  else Ok (tool, (base_uri, checksum))

let extract_plate_media ~plate_short_id (plate_media_json : Types.json option) :
    media_point list =
  match plate_media_json with
  | Some (`Assoc fields) -> (
      match List.assoc_opt plate_short_id fields with
      | Some (`List items) ->
          let parsed =
            List.filter_map
              (fun item ->
                try Some (media_point_of_yojson item) with _ -> None)
              items
          in
          List.sort (fun a b -> Int.compare a.time b.time) parsed
      | _ -> [])
  | _ -> []

let sign_media_points ~(config : Object_storage.config) ~(now : float)
    ~(expires_in : int) (points : media_point list) : media_point list =
  List.map
    (fun (pt : media_point) ->
      match
        Object_storage.resolve_download_url ~config ~uri:pt.uri ~now ~expires_in
      with
      | Ok url -> { pt with signed_url = Some url }
      | Error _ -> pt)
    points

let extract_artifact_uri ~artifact_key (artifacts_json : Types.json) :
    string option =
  match artifacts_json with
  | `Assoc fields -> (
      match List.assoc_opt artifact_key fields with
      | Some (`String uri) when String.trim uri <> "" -> Some (String.trim uri)
      | _ -> None)
  | _ -> None

type discovered_manifest = {
  tool_name : string;
  tool_version : string option;
  storage_base_uri : string;
  dataset_checksum : string;
  pipeline_name : string option;
  pipeline_config : Types.json option;
  artifacts : Types.json;
  summary_metrics : Types.json option;
  plate_media : Types.json option;
}

let artifact_key_of_rel_path (rel_path : string) : string =
  let buf = Buffer.create (String.length rel_path) in
  String.iter
    (fun c ->
      if
        (c >= 'A' && c <= 'Z')
        || (c >= 'a' && c <= 'z')
        || (c >= '0' && c <= '9')
      then Buffer.add_char buf c
      else Buffer.add_char buf '_')
    rel_path;
  Buffer.contents buf

let has_hidden_segment (rel_path : string) : bool =
  String.split_on_char '/' rel_path
  |> List.exists (fun seg -> seg <> "" && seg.[0] = '.')

let is_overlay_rel_path (rel_path : string) : bool =
  String.starts_with ~prefix:"overlays/" rel_path
  || String.starts_with ~prefix:"plate_media/" rel_path

let parse_metadata_csv_plate_images (csv_str : string) :
    int * int * (string, string list) Hashtbl.t =
  let lines = String.split_on_char '\n' csv_str in
  match lines with
  | [] -> (0, 0, Hashtbl.create 0)
  | header_line :: data_lines ->
      let headers =
        String.split_on_char ',' (String.trim header_line)
        |> List.map String.trim
      in
      let find_col name =
        let rec aux i = function
          | [] -> None
          | h :: rest -> if h = name then Some i else aux (i + 1) rest
        in
        aux 0 headers
      in
      let img_col = find_col "Metadata_ImageName" in
      let ds_col = find_col "Metadata_Dataset" in
      let plate_col = find_col "Metadata_PlateID" in
      let sample_col = find_col "Metadata_SampleID" in
      let plate_imgs : (string, (string * string) list) Hashtbl.t =
        Hashtbl.create 32
      in
      let unique_samples = Hashtbl.create 256 in
      let row_count = ref 0 in
      List.iter
        (fun line ->
          let trimmed = String.trim line in
          if trimmed <> "" then (
            incr row_count;
            let cols = Array.of_list (String.split_on_char ',' trimmed) in
            let get_opt = function
              | Some idx when idx < Array.length cols ->
                  let v = String.trim cols.(idx) in
                  if v <> "" then Some v else None
              | _ -> None
            in
            (match get_opt sample_col with
            | Some sid -> Hashtbl.replace unique_samples sid true
            | None -> ());
            match (get_opt plate_col, get_opt ds_col, get_opt img_col) with
            | Some plate_sid, Some ds, Some img ->
                let prev =
                  Option.value ~default:[]
                    (Hashtbl.find_opt plate_imgs plate_sid)
                in
                Hashtbl.replace plate_imgs plate_sid ((img, ds) :: prev)
            | _ -> ()))
        data_lines;
      let resolved : (string, string list) Hashtbl.t =
        Hashtbl.create (Hashtbl.length plate_imgs)
      in
      Hashtbl.iter
        (fun plate_sid pairs ->
          let uniq_sorted =
            List.sort_uniq
              (fun (i1, d1) (i2, d2) ->
                let c = String.compare i1 i2 in
                if c <> 0 then c else String.compare d1 d2)
              pairs
          in
          let rel_paths =
            List.map
              (fun (img, ds) -> Printf.sprintf "overlays/%s/%s.png" ds img)
              uniq_sorted
          in
          Hashtbl.replace resolved plate_sid rel_paths)
        plate_imgs;
      (Hashtbl.length unique_samples, !row_count, resolved)

let build_manifest_from_objects ~(folder_uri : string)
    ?(tool_name : string option) ?(pipeline_config : Types.json option)
    ?(metadata_csv : string option)
    (objects : Object_storage.listed_object list) : discovered_manifest =
  let base_uri = String_utils.strip_trailing_slashes (String.trim folder_uri) in
  let scheme =
    if String.starts_with ~prefix:"gs://" base_uri then "gs"
    else if String.starts_with ~prefix:"s3://" base_uri then "s3"
    else "https"
  in
  let bucket, prefix_no_slash =
    match Object_storage.parse_bucket_uri ~scheme base_uri with
    | Ok pair -> pair
    | Error _ -> ("", "")
  in
  let prefix = prefix_no_slash ^ "/" in
  let prefix_len = String.length prefix in

  let overlay_set = Hashtbl.create 256 in
  let deliverable_entries = ref [] in
  let saw_phenotypic_marker = ref false in

  List.iter
    (fun (obj : Object_storage.listed_object) ->
      if
        String.starts_with ~prefix obj.key
        && not (String.ends_with ~suffix:"/" obj.key)
      then
        let rel =
          String.sub obj.key prefix_len (String.length obj.key - prefix_len)
        in
        if rel <> "" && not (has_hidden_segment rel) then (
          if rel = "pipeline.json.pht-pipe" || rel = "rembi.yaml" then
            saw_phenotypic_marker := true;
          if is_overlay_rel_path rel then Hashtbl.replace overlay_set rel obj
          else deliverable_entries := (rel, obj) :: !deliverable_entries))
    objects;

  let sorted_deliverables =
    List.sort (fun (r1, _) (r2, _) -> String.compare r1 r2) !deliverable_entries
  in

  let artifacts_fields =
    List.map
      (fun (rel, _obj) ->
        let full_uri = Printf.sprintf "%s://%s/%s%s" scheme bucket prefix rel in
        (artifact_key_of_rel_path rel, `String full_uri))
      sorted_deliverables
  in

  let manifest_hash_input =
    sorted_deliverables
    |> List.map (fun (rel, (o : Object_storage.listed_object)) ->
        Printf.sprintf "%s:%s" rel o.etag)
    |> String.concat "\n"
  in
  let dataset_checksum =
    "sha256:" ^ Digestif.SHA256.(to_hex (digest_string manifest_hash_input))
  in

  let cfg_name, cfg_ver =
    match pipeline_config with
    | Some (`Assoc fs) ->
        let n =
          match List.assoc_opt "name" fs with
          | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
          | _ -> None
        in
        let v =
          match List.assoc_opt "version" fs with
          | Some (`String s) when String.trim s <> "" -> Some (String.trim s)
          | _ -> None
        in
        (n, v)
    | _ -> (None, None)
  in

  let resolved_tool =
    match tool_name with
    | Some t when String.trim t <> "" -> String.trim t
    | _ -> if !saw_phenotypic_marker then "PhenoTypic" else "Custom"
  in

  let samples_count, rows_count, plate_media_opt, plates_count =
    match metadata_csv with
    | None -> (0, 0, None, 0)
    | Some csv_str ->
        let n_samples, n_rows, plate_rel_map =
          parse_metadata_csv_plate_images csv_str
        in
        let plate_fields = ref [] in
        Hashtbl.iter
          (fun plate_sid rel_paths ->
            let existing_rels =
              if Hashtbl.length overlay_set = 0 then rel_paths
              else List.filter (fun r -> Hashtbl.mem overlay_set r) rel_paths
            in
            if existing_rels <> [] then
              let pts =
                List.mapi
                  (fun idx rel ->
                    let uri =
                      Printf.sprintf "%s://%s/%s%s" scheme bucket prefix rel
                    in
                    `Assoc
                      [
                        ("time", `Int (idx + 1));
                        ("uri", `String uri);
                        ("media_type", `String "image");
                      ])
                  existing_rels
              in
              plate_fields := (plate_sid, `List pts) :: !plate_fields)
          plate_rel_map;
        let sorted_plates =
          List.sort (fun (p1, _) (p2, _) -> String.compare p1 p2) !plate_fields
        in
        let media_json =
          if sorted_plates = [] then None else Some (`Assoc sorted_plates)
        in
        (n_samples, n_rows, media_json, List.length sorted_plates)
  in

  let metrics_fields =
    List.filter_map Fun.id
      [
        Some ("artifacts_count", `Int (List.length artifacts_fields));
        (if plates_count > 0 then Some ("plates_count", `Int plates_count)
         else None);
        (if samples_count > 0 then Some ("samples_count", `Int samples_count)
         else None);
        (if Hashtbl.length overlay_set > 0 then
           Some ("media_files_count", `Int (Hashtbl.length overlay_set))
         else None);
        (if rows_count > 0 then Some ("observations_count", `Int rows_count)
         else None);
      ]
  in

  {
    tool_name = resolved_tool;
    tool_version = cfg_ver;
    storage_base_uri = base_uri;
    dataset_checksum;
    pipeline_name = cfg_name;
    pipeline_config;
    artifacts = `Assoc artifacts_fields;
    summary_metrics = Some (`Assoc metrics_fields);
    plate_media = plate_media_opt;
  }
