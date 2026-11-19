(** Pure domain logic for versioned Analysis Runs and Plate Media. *)

type media_point = {
  time : int;
  uri : string;
  label : string option; [@yojson.option]
  media_type : string option; [@yojson.option]
  signed_url : string option; [@yojson.option]
}
[@@deriving yojson, show, eq]

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

val validate_create :
  tool_name:string ->
  storage_base_uri:string ->
  dataset_checksum:string ->
  (string * (string * string), string) result

val extract_plate_media :
  plate_short_id:string -> Types.json option -> media_point list

val sign_media_points :
  config:Object_storage.config ->
  now:float ->
  expires_in:int ->
  media_point list ->
  media_point list

val extract_artifact_uri : artifact_key:string -> Types.json -> string option

val build_manifest_from_objects :
  folder_uri:string ->
  ?tool_name:string ->
  ?pipeline_config:Types.json ->
  ?metadata_csv:string ->
  Object_storage.listed_object list ->
  discovered_manifest
