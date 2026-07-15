(** Core type definitions and generic data structures for Exlab. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type json = Yojson.Safe.t

let yojson_of_json (json : Yojson.Safe.t) : Yojson.Safe.t = json
let json_of_yojson (json : Yojson.Safe.t) : Yojson.Safe.t = json
let equal_json j1 j2 = Yojson.Safe.equal j1 j2
let pp_json fmt json = Format.pp_print_string fmt (Yojson.Safe.to_string json)

type entity_status = Pending | Active | Completed | Archived
[@@deriving show, eq]

let entity_status_of_string = function
  | "Pending" -> Ok Pending
  | "Active" -> Ok Active
  | "Completed" -> Ok Completed
  | "Archived" -> Ok Archived
  | unknown -> Error ("Unknown entity status: " ^ unknown)

let string_of_entity_status = function
  | Pending -> "Pending"
  | Active -> "Active"
  | Completed -> "Completed"
  | Archived -> "Archived"

let yojson_of_entity_status status = `String (string_of_entity_status status)

let entity_status_of_yojson json =
  match json with
  | `String s -> (
      match entity_status_of_string s with
      | Ok status -> status
      | Error msg -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error msg json)
  | _ ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid format: expected a string" json

type project = {
  id : int;  (** The internal database ID. *)
  uid : string;  (** A globally unique identifier (UUID v4). *)
  short_id : string;
      (** A human-readable short identifier (e.g., PROJ-0001). *)
  name : string;  (** The name of the project. *)
  description : string option; [@yojson.option]
      (** An optional description of the project's goals. *)
  status : entity_status;  (** The current status of the project. *)
  contact_name : string option; [@yojson.option]
      (** Optional point of contact. *)
  owner : string option; [@yojson.option]
      (** Optional owner or lead scientist. *)
  metadata : json option;
      [@yojson.option]
      [@printer fun fmt _ -> Format.pp_print_string fmt "<metadata>"]
      (** Arbitrary key-value metadata for this project. *)
  created_at : float;  (** UNIX timestamp of creation. *)
  updated_at : float;  (** UNIX timestamp of the last update. *)
}
[@@deriving yojson, show, eq]
(** A project represents a top-level organizational unit for laboratory work. *)

(** The category of a sample, determining its role in the workflow. *)
type sample_category = Source | Experimental [@@deriving show, eq]

(** [sample_category_of_string s] converts a string to a [sample_category]. *)
let sample_category_of_string = function
  | "Source" -> Ok Source
  | "Experimental" -> Ok Experimental
  | unknown -> Error ("Unknown sample category: " ^ unknown)

(** [string_of_sample_category c] converts a [sample_category] to a string. *)
let string_of_sample_category = function
  | Source -> "Source"
  | Experimental -> "Experimental"

(** [yojson_of_sample_category] provides JSON serialization for
    [sample_category]. *)
let yojson_of_sample_category category =
  `String (string_of_sample_category category)

(** [sample_category_of_yojson] provides JSON deserialization for
    [sample_category]. *)
let sample_category_of_yojson json =
  match json with
  | `String s -> (
      match sample_category_of_string s with
      | Ok cat -> cat
      | Error msg -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error msg json)
  | _ ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid format: expected a string" json

type sample = {
  id : int;  (** Internal database ID. *)
  uid : string;  (** UUID v4. *)
  short_id : string;  (** Human-readable short identifier. *)
  project_id : int;  (** ID of the associated project. *)
  sample_type : string;
      (** Specific type of the sample (e.g., "Liquid Cell Culture"). *)
  status : entity_status;  (** Current status. *)
  category : sample_category;  (** Category ("Source" or "Experimental"). *)
  parent_sample_id : int option; [@yojson.option]
      (** ID of the parent sample if derived. *)
  strain_id : int option; [@yojson.option]
      (** Associated biological strain ID. *)
  community_id : int option; [@yojson.option]
      (** Associated biological community ID. *)
  created_at : float;  (** UNIX timestamp of creation. *)
  updated_at : float;  (** UNIX timestamp of the last update. *)
}
[@@deriving yojson, show, eq]
(** A sample represents an individual unit of biological or chemical material.
*)

(** The rules dictating how to represent a piece of data attached to an entity.
*)
module ResultType = struct
  type t =
    | String
    | Boolean
    | Datetime
    | Date
    | Float
    | Integer
    | FileLink
    | StringSeries
    | BooleanSeries
    | DatetimeSeries
    | DateSeries
    | FloatSeries
    | IntegerSeries
    | FileLinkSeries
  [@@deriving show, eq]

  let to_string = function
    | String -> "String"
    | Boolean -> "Boolean"
    | Datetime -> "Datetime"
    | Date -> "Date"
    | Float -> "Float"
    | Integer -> "Integer"
    | FileLink -> "FileLink"
    | StringSeries -> "StringSeries"
    | BooleanSeries -> "BooleanSeries"
    | DatetimeSeries -> "DatetimeSeries"
    | DateSeries -> "DateSeries"
    | FloatSeries -> "FloatSeries"
    | IntegerSeries -> "IntegerSeries"
    | FileLinkSeries -> "FileLinkSeries"

  let of_string = function
    | "String" -> Ok String
    | "Boolean" -> Ok Boolean
    | "Datetime" -> Ok Datetime
    | "Date" -> Ok Date
    | "Float" -> Ok Float
    | "Integer" -> Ok Integer
    | "FileLink" -> Ok FileLink
    | "StringSeries" -> Ok StringSeries
    | "BooleanSeries" -> Ok BooleanSeries
    | "DatetimeSeries" -> Ok DatetimeSeries
    | "DateSeries" -> Ok DateSeries
    | "FloatSeries" -> Ok FloatSeries
    | "IntegerSeries" -> Ok IntegerSeries
    | "FileLinkSeries" -> Ok FileLinkSeries
    | s -> Error ("Unknown result_data_type: " ^ s)

  let yojson_of_t data_type = `String (to_string data_type)

  let t_of_yojson = function
    | `String s -> (
        match of_string s with
        | Ok t -> t
        | Error msg -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error msg `Null
        )
    | _ ->
        Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
          "Expected string for result_data_type" `Null
end

type result_category = {
  id : int;
  uid : string;
  name : string;
  description : string option; [@yojson.option]
}
[@@deriving yojson, show, eq]
(** A category grouping related result definitions together. *)

type result_definition = {
  id : int;
  uid : string;
  short_id : string;
  name : string;
  description : string option; [@yojson.option]
  data_type : ResultType.t;  (** The primitive data type of the result. *)
  unit : string option; [@yojson.option]  (** Optional unit of measurement. *)
  category : result_category option; [@yojson.option]
      (** The category this definition belongs to. *)
  is_required : bool; [@default false]
  created_at : float;
  updated_at : float;
}
[@@deriving yojson, show, eq]
(** Defines the expected format and constraints for a specific measurement or
    observation. *)

(** A typed piece of data attached to a sample or plate. *)
module ResultPayload = struct
  type t =
    | String of string
    | Integer of int
    | Float of float
    | Boolean of bool
    | Datetime of float
    | Date of float
    | FileLink of string
    | StringSeries of (int * string) list
    | IntegerSeries of (int * int) list
    | FloatSeries of (int * float) list
    | BooleanSeries of (int * bool) list
    | DatetimeSeries of (int * float) list
    | DateSeries of (int * float) list
    | FileLinkSeries of (int * string) list
  [@@deriving show, eq]

  let yojson_of_t = function
    | String s -> `Assoc [ ("type", `String "String"); ("value", `String s) ]
    | Integer i -> `Assoc [ ("type", `String "Integer"); ("value", `Int i) ]
    | Float f -> `Assoc [ ("type", `String "Float"); ("value", `Float f) ]
    | Boolean b -> `Assoc [ ("type", `String "Boolean"); ("value", `Bool b) ]
    | Datetime f -> `Assoc [ ("type", `String "Datetime"); ("value", `Float f) ]
    | Date f -> `Assoc [ ("type", `String "Date"); ("value", `Float f) ]
    | FileLink s ->
        `Assoc [ ("type", `String "FileLink"); ("value", `String s) ]
    | StringSeries pts ->
        `Assoc
          [
            ("type", `String "StringSeries");
            ( "value",
              `List
                (List.map
                   (fun (t, v) ->
                     `Assoc [ ("time", `Int t); ("value", `String v) ])
                   pts) );
          ]
    | IntegerSeries pts ->
        `Assoc
          [
            ("type", `String "IntegerSeries");
            ( "value",
              `List
                (List.map
                   (fun (t, v) ->
                     `Assoc [ ("time", `Int t); ("value", `Int v) ])
                   pts) );
          ]
    | FloatSeries pts ->
        `Assoc
          [
            ("type", `String "FloatSeries");
            ( "value",
              `List
                (List.map
                   (fun (t, v) ->
                     `Assoc [ ("time", `Int t); ("value", `Float v) ])
                   pts) );
          ]
    | BooleanSeries pts ->
        `Assoc
          [
            ("type", `String "BooleanSeries");
            ( "value",
              `List
                (List.map
                   (fun (t, v) ->
                     `Assoc [ ("time", `Int t); ("value", `Bool v) ])
                   pts) );
          ]
    | DatetimeSeries pts ->
        `Assoc
          [
            ("type", `String "DatetimeSeries");
            ( "value",
              `List
                (List.map
                   (fun (t, v) ->
                     `Assoc [ ("time", `Int t); ("value", `Float v) ])
                   pts) );
          ]
    | DateSeries pts ->
        `Assoc
          [
            ("type", `String "DateSeries");
            ( "value",
              `List
                (List.map
                   (fun (t, v) ->
                     `Assoc [ ("time", `Int t); ("value", `Float v) ])
                   pts) );
          ]
    | FileLinkSeries pts ->
        `Assoc
          [
            ("type", `String "FileLinkSeries");
            ( "value",
              `List
                (List.map
                   (fun (t, v) ->
                     `Assoc [ ("time", `Int t); ("value", `String v) ])
                   pts) );
          ]

  let t_of_yojson json =
    let parse_time point =
      match List.assoc_opt "time" point with
      | Some (`Int t) -> t
      | Some (`Float t) -> int_of_float t
      | _ -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "Invalid time" json
    in
    let parse_series_points extract_val points =
      let parsed =
        List.map
          (function
            | `Assoc p -> (parse_time p, extract_val (List.assoc_opt "value" p))
            | _ ->
                Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "Invalid point"
                  json)
          points
      in
      List.sort (fun (t1, _) (t2, _) -> Int.compare t1 t2) parsed
    in
    match json with
    | `Assoc fields -> (
        let typ = List.assoc_opt "type" fields in
        let value = List.assoc_opt "value" fields in
        match (typ, value) with
        | Some (`String "String"), Some (`String s) -> String s
        | Some (`String "Integer"), Some (`Int i) -> Integer i
        | Some (`String "Float"), Some (`Float f) -> Float f
        | Some (`String "Float"), Some (`Int i) -> Float (float_of_int i)
        | Some (`String "Boolean"), Some (`Bool b) -> Boolean b
        | Some (`String "Datetime"), Some (`Float f) -> Datetime f
        | Some (`String "Datetime"), Some (`Int i) -> Datetime (float_of_int i)
        | Some (`String "Date"), Some (`Float f) -> Date f
        | Some (`String "Date"), Some (`Int i) -> Date (float_of_int i)
        | Some (`String "FileLink"), Some (`String s) -> FileLink s
        | Some (`String "StringSeries"), Some (`List pts) ->
            StringSeries
              (parse_series_points
                 (function
                   | Some (`String s) -> s
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
                         "Expected string" json)
                 pts)
        | Some (`String "IntegerSeries"), Some (`List pts) ->
            IntegerSeries
              (parse_series_points
                 (function
                   | Some (`Int i) -> i
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
                         "Expected int" json)
                 pts)
        | Some (`String "FloatSeries"), Some (`List pts) ->
            FloatSeries
              (parse_series_points
                 (function
                   | Some (`Float f) -> f
                   | Some (`Int i) -> float_of_int i
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
                         "Expected float" json)
                 pts)
        | Some (`String "BooleanSeries"), Some (`List pts) ->
            BooleanSeries
              (parse_series_points
                 (function
                   | Some (`Bool b) -> b
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
                         "Expected bool" json)
                 pts)
        | Some (`String "DatetimeSeries"), Some (`List pts) ->
            DatetimeSeries
              (parse_series_points
                 (function
                   | Some (`Float f) -> f
                   | Some (`Int i) -> float_of_int i
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
                         "Expected datetime" json)
                 pts)
        | Some (`String "DateSeries"), Some (`List pts) ->
            DateSeries
              (parse_series_points
                 (function
                   | Some (`Float f) -> f
                   | Some (`Int i) -> float_of_int i
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
                         "Expected date" json)
                 pts)
        | Some (`String "FileLinkSeries"), Some (`List pts) ->
            FileLinkSeries
              (parse_series_points
                 (function
                   | Some (`String s) -> s
                   | _ ->
                       Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
                         "Expected string" json)
                 pts)
        | _ ->
            Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
              "Invalid ResultPayload format" json)
    | _ ->
        Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
          "Expected object for result_payload" json
end

type result_value = {
  id : int;
  uid : string;
  sample_id : int option; [@yojson.option]
      (** ID of the sample this result belongs to. *)
  plate_id : int option; [@yojson.option]
      (** ID of the plate this result belongs to. *)
  result_definition_id : int;
      (** ID of the definition validating this value. *)
  value : ResultPayload.t option;  (** The actual recorded data payload. *)
  created_at : float;
  updated_at : float;
}
[@@deriving yojson, show, eq]
(** A concrete value matching a [result_definition], assigned to a sample or
    plate. *)

type product = {
  id : int;
  uid : string;
  short_id : string;
  name : string;
  brand : string option; [@yojson.option]
  manufacturer_part_number : string option; [@yojson.option]
  description : string option; [@yojson.option]
  created_at : float;
  updated_at : float;
}
[@@deriving yojson, show, eq]
(** A product definition representing a catalog item (e.g., a specific brand of
    microwell plate). *)

(** The physical layout format of a plate. *)
type plate_format = Well_24 | Well_48 | Well_96 | Well_384
[@@deriving show, eq]

let plate_format_of_string = function
  | "24-well" -> Ok Well_24
  | "48-well" -> Ok Well_48
  | "96-well" -> Ok Well_96
  | "384-well" -> Ok Well_384
  | unknown -> Error ("Unknown plate format: " ^ unknown)

let string_of_plate_format = function
  | Well_24 -> "24-well"
  | Well_48 -> "48-well"
  | Well_96 -> "96-well"
  | Well_384 -> "384-well"

let yojson_of_plate_format format = `String (string_of_plate_format format)

let plate_format_of_yojson json =
  match json with
  | `String s -> (
      match plate_format_of_string s with
      | Ok format -> format
      | Error msg -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error msg json)
  | _ ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        "Invalid format: expected a string" json

type plate = {
  id : int;
  uid : string;
  short_id : string;
  name : string;
  project_id : int;
  product_id : int option; [@yojson.option]
  plate_format : plate_format;
  created_at : float;
  updated_at : float;
}
[@@deriving yojson, show, eq]
(** A physical piece of labware containing multiple wells. *)

type well = {
  id : int;
  plate_id : int;
  sample_id : int option; [@yojson.option]
  coordinate : string;  (** Alpha-numeric coordinate (e.g., "A1"). *)
}
[@@deriving yojson, show, eq]
(** An individual well located on a specific plate, optionally containing a
    sample. *)

type external_db_definition = {
  id : int;
  uid : string;
  name : string;  (** The name of the external service (e.g., "NCBI"). *)
  url_template : string option;
      (** A template string with '\{\}' where the ID goes. *)
  created_at : float;
  updated_at : float;
}
[@@deriving yojson, show, eq]
(** A definition of an external database or service that strains can link to. *)

type strain_external_link = {
  id : int;
  uid : string;
  strain_id : int;
  external_db_definition_id : int;
  value : string;  (** The ID or accession number in the external database. *)
  created_at : float;
}
[@@deriving yojson, show, eq]
(** A link connecting a strain to an external database record. *)

type strain = {
  id : int;
  uid : string;
  genus : string;
  species : string;
  strain_name : string;  (** Specific strain designation. *)
  genotype : string option; [@yojson.option]
  parent_strain_id : int option; [@yojson.option]
  notes : string option; [@yojson.option]
  external_links : strain_external_link list option; [@yojson.option]
      (** Related database links. *)
  created_at : float;
  updated_at : float;
}
[@@deriving yojson, show, eq]
(** Biological organism or strain information. *)

type community = {
  id : int;
  uid : string;
  name : string;  (** Display name. *)
  notes : string option; [@yojson.option]
  metadata : json option; [@yojson.option]
      (** Deployment-specific metadata (JSONB). *)
  created_at : float;
  updated_at : float;
}
[@@deriving yojson, show, eq]
(** A biological community (metagenome, consortium, etc.). *)

type community_member = {
  id : int;
  uid : string;
  community_id : int;
  strain_id : int option; [@yojson.option]
      (** Known strain reference, or None for unknown OTUs/ASVs. *)
  label : string;  (** Human-readable identifier, e.g. OTU_001. *)
  taxon : string option; [@yojson.option]
      (** Lineage string for unknown members. *)
  created_at : float;
}
[@@deriving yojson, show, eq]
(** A member of a community — either a known strain or an unknown OTU. *)

(** Privileges and roles for system users. *)
type user_role = Admin | Lab_manager | Project_manager | Project_user
[@@deriving show, eq]

let string_of_role = function
  | Admin -> "admin"
  | Lab_manager -> "lab_manager"
  | Project_manager -> "project_manager"
  | Project_user -> "project_user"

let role_of_string = function
  | "admin" -> Ok Admin
  | "lab_manager" -> Ok Lab_manager
  | "project_manager" -> Ok Project_manager
  | "project_user" -> Ok Project_user
  | s -> Error ("Unknown user role: " ^ s)

let yojson_of_user_role role = `String (string_of_role role)

let user_role_of_yojson = function
  | `String s -> (
      match role_of_string s with Ok r -> r | Error msg -> failwith msg)
  | _ -> failwith "Expected a JSON string for user_role"

type user = {
  id : int;
  uid : string;
  email : string;
  password_hash : string;
  role : user_role;
  created_at : float;
  updated_at : float;
}
[@@deriving yojson, show, eq]

type system_setting = {
  id : int;
  key : string;
  value : json;
  description : string option; [@yojson.option]
  updated_at : float;
}
[@@deriving yojson, show, eq]

(** The data type of a metadata field defined by an admin. *)
type metadata_field_type = String | Enum of string list
[@@deriving yojson, show, eq]

type metadata_field_def = { key : string; field_type : metadata_field_type }
[@@deriving yojson, show, eq]
(** The schema definition for an expected metadata key. *)
