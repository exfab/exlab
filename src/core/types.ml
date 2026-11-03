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
  let days_from_civil y m d =
    let y = if m <= 2 then y - 1 else y in
    let era = (if y >= 0 then y else y - 399) / 400 in
    let yoe = y - (era * 400) in
    let doy = (((153 * if m > 2 then m - 3 else m + 9) + 2) / 5) + d - 1 in
    let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
    (era * 146097) + doe - 719468

  let normalize_year y =
    if y >= 0 && y < 70 then 2000 + y
    else if y >= 70 && y < 100 then 1900 + y
    else y

  let month_of_name s =
    match String.lowercase_ascii (String.trim s) with
    | "jan" | "january" -> Some 1
    | "feb" | "february" -> Some 2
    | "mar" | "march" -> Some 3
    | "apr" | "april" -> Some 4
    | "may" -> Some 5
    | "jun" | "june" -> Some 6
    | "jul" | "july" -> Some 7
    | "aug" | "august" -> Some 8
    | "sep" | "sept" | "september" -> Some 9
    | "oct" | "october" -> Some 10
    | "nov" | "november" -> Some 11
    | "dec" | "december" -> Some 12
    | _ -> None

  let is_all_digits s =
    String.length s > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s

  let is_leap_year y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0

  let days_in_month y = function
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> Some 31
    | 4 | 6 | 9 | 11 -> Some 30
    | 2 -> Some (if is_leap_year y then 29 else 28)
    | _ -> None

  let epoch_of_ymd y m d =
    match days_in_month y m with
    | Some max_d when d >= 1 && d <= max_d ->
        let days = days_from_civil y m d in
        Ok (float_of_int days *. 86400.0)
    | _ -> Error "Invalid calendar date"

  let parse_three_tokens t1 t2 t3 =
    match
      (int_of_string_opt t1, int_of_string_opt t2, int_of_string_opt t3)
    with
    | Some a, Some b, Some c ->
        if String.length t1 = 4 || a >= 1000 then epoch_of_ymd a b c
        else
          let y = normalize_year c in
          if a > 12 then epoch_of_ymd y b a else epoch_of_ymd y a b
    | None, Some d, Some y -> (
        match month_of_name t1 with
        | Some m -> epoch_of_ymd (normalize_year y) m d
        | None -> Error "Invalid month")
    | Some a, None, Some c -> (
        match month_of_name t2 with
        | Some m ->
            if String.length t1 = 4 || a >= 1000 then epoch_of_ymd a m c
            else epoch_of_ymd (normalize_year c) m a
        | None -> Error "Invalid month")
    | _ -> Error "Invalid date tokens"

  let tokenize_date s =
    let buf = Buffer.create 16 in
    let acc = ref [] in
    let flush () =
      if Buffer.length buf > 0 then (
        acc := Buffer.contents buf :: !acc;
        Buffer.clear buf)
    in
    String.iter
      (fun c ->
        match c with
        | '-' | '/' | '.' | ' ' | ',' -> flush ()
        | _ -> Buffer.add_char buf c)
      s;
    flush ();
    List.rev !acc

  let parse_date_string value =
    let s = String.trim value in
    if
      String.length s = 8
      && is_all_digits s
      &&
      let y = int_of_string (String.sub s 0 4) in
      let m = int_of_string (String.sub s 4 2) in
      let d = int_of_string (String.sub s 6 2) in
      y >= 1900 && y <= 2199 && m >= 1 && m <= 12 && d >= 1 && d <= 31
    then
      let y = int_of_string (String.sub s 0 4) in
      let m = int_of_string (String.sub s 4 2) in
      let d = int_of_string (String.sub s 6 2) in
      epoch_of_ymd y m d
    else
      match float_of_string_opt s with
      | Some f -> Ok f
      | None -> (
          match tokenize_date s with
          | [ t1; t2; t3 ] -> (
              match parse_three_tokens t1 t2 t3 with
              | Ok f -> Ok f
              | Error _ ->
                  Error (Printf.sprintf "Could not parse '%s' as Date" value))
          | _ -> Error (Printf.sprintf "Could not parse '%s' as Date" value))

  let parse_tz_offset tz_str =
    let tz = String.trim tz_str in
    if tz = "" || tz = "Z" || tz = "z" then Ok 0.0
    else
      let sign =
        if String.starts_with ~prefix:"+" tz then Some 1.0
        else if String.starts_with ~prefix:"-" tz then Some (-1.0)
        else None
      in
      match sign with
      | None -> Error "Invalid timezone offset"
      | Some s -> (
          let rest = String.sub tz 1 (String.length tz - 1) in
          match String.split_on_char ':' rest with
          | [ hh; mm ] -> (
              match (int_of_string_opt hh, int_of_string_opt mm) with
              | Some h, Some m -> Ok (s *. float_of_int ((h * 3600) + (m * 60)))
              | _ -> Error "Invalid timezone offset")
          | [ hhmm ] when String.length hhmm = 4 -> (
              match
                ( int_of_string_opt (String.sub hhmm 0 2),
                  int_of_string_opt (String.sub hhmm 2 2) )
              with
              | Some h, Some m -> Ok (s *. float_of_int ((h * 3600) + (m * 60)))
              | _ -> Error "Invalid timezone offset")
          | _ -> Error "Invalid timezone offset")

  let extract_am_pm s =
    let trimmed = String.trim s in
    let lower = String.lowercase_ascii trimmed in
    let len = String.length trimmed in
    if String.ends_with ~suffix:"am" lower then
      (String.trim (String.sub trimmed 0 (len - 2)), Some `AM)
    else if String.ends_with ~suffix:"pm" lower then
      (String.trim (String.sub trimmed 0 (len - 2)), Some `PM)
    else (trimmed, None)

  let split_time_and_tz time_str =
    let s = String.trim time_str in
    let len = String.length s in
    if len > 0 && (s.[len - 1] = 'Z' || s.[len - 1] = 'z') then
      (String.sub s 0 (len - 1), "Z")
    else
      let rec find_tz i =
        if i <= 0 then (s, "")
        else if s.[i] = '+' || s.[i] = '-' then
          (String.sub s 0 i, String.sub s i (len - i))
        else find_tz (i - 1)
      in
      find_tz (len - 1)

  let adjust_12h h am_pm =
    match am_pm with
    | None -> Ok h
    | Some `AM ->
        if h < 1 || h > 12 then Error "Invalid 12-hour time"
        else Ok (if h = 12 then 0 else h)
    | Some `PM ->
        if h < 1 || h > 12 then Error "Invalid 12-hour time"
        else Ok (if h = 12 then 12 else h + 12)

  let parse_time_part time_str =
    if String.trim time_str = "" then Ok 0.0
    else
      let without_ampm, am_pm = extract_am_pm time_str in
      let t_part, tz_part = split_time_and_tz without_ampm in
      match parse_tz_offset tz_part with
      | Error e -> Error e
      | Ok tz_offset -> (
          match String.split_on_char ':' (String.trim t_part) with
          | [ hh; mm ] -> (
              match (int_of_string_opt hh, int_of_string_opt mm) with
              | Some h, Some m when m >= 0 && m <= 59 -> (
                  match adjust_12h h am_pm with
                  | Ok h24 when h24 >= 0 && h24 <= 23 ->
                      Ok (float_of_int ((h24 * 3600) + (m * 60)) -. tz_offset)
                  | _ -> Error "Invalid time")
              | _ -> Error "Invalid time")
          | [ hh; mm; ss ] -> (
              match
                ( int_of_string_opt hh,
                  int_of_string_opt mm,
                  float_of_string_opt ss )
              with
              | Some h, Some m, Some sec
                when m >= 0 && m <= 59 && sec >= 0.0 && sec < 60.0 -> (
                  match adjust_12h h am_pm with
                  | Ok h24 when h24 >= 0 && h24 <= 23 ->
                      Ok
                        (float_of_int ((h24 * 3600) + (m * 60))
                        +. sec -. tz_offset)
                  | _ -> Error "Invalid time")
              | _ -> Error "Invalid time")
          | _ -> Error "Invalid time")

  let split_datetime_parts s =
    match String.index_opt s 'T' with
    | Some idx when idx > 0 && s.[idx - 1] >= '0' && s.[idx - 1] <= '9' ->
        (String.sub s 0 idx, String.sub s (idx + 1) (String.length s - idx - 1))
    | _ -> (
        match String.index_opt s 't' with
        | Some idx when idx > 0 && s.[idx - 1] >= '0' && s.[idx - 1] <= '9' ->
            ( String.sub s 0 idx,
              String.sub s (idx + 1) (String.length s - idx - 1) )
        | _ -> (
            match String.index_opt s ':' with
            | None -> (s, "")
            | Some colon_idx -> (
                let before_colon = String.sub s 0 colon_idx in
                match String.rindex_opt before_colon ' ' with
                | Some sp_idx ->
                    ( String.sub s 0 sp_idx,
                      String.sub s (sp_idx + 1) (String.length s - sp_idx - 1)
                    )
                | None -> (s, ""))))

  let parse_datetime_string value =
    let s = String.trim value in
    match float_of_string_opt s with
    | Some f
      when not
             (String.length s = 8
             && is_all_digits s
             &&
             let y = int_of_string (String.sub s 0 4) in
             y >= 1900 && y <= 2199) ->
        Ok f
    | _ -> (
        let date_part, time_part = split_datetime_parts s in
        match (parse_date_string date_part, parse_time_part time_part) with
        | Ok d_sec, Ok t_sec -> Ok (d_sec +. t_sec)
        | _ -> Error (Printf.sprintf "Could not parse '%s' as Datetime" value))

  let civil_from_days z =
    let z = z + 719468 in
    let era = (if z >= 0 then z else z - 146096) / 146097 in
    let doe = z - (era * 146097) in
    let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
    let y = yoe + (era * 400) in
    let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
    let mp = ((5 * doy) + 2) / 153 in
    let d = doy - (((153 * mp) + 2) / 5) + 1 in
    let m = if mp < 10 then mp + 3 else mp - 9 in
    let y = if m <= 2 then y + 1 else y in
    (y, m, d)

  let format_iso_date epoch =
    let total_sec = int_of_float (Float.floor epoch) in
    let days =
      if total_sec >= 0 then total_sec / 86400
      else ((total_sec + 1) / 86400) - 1
    in
    let y, m, d = civil_from_days days in
    Printf.sprintf "%04d-%02d-%02d" y m d

  let format_iso_datetime epoch =
    let total_sec = int_of_float (Float.floor epoch) in
    let days =
      if total_sec >= 0 then total_sec / 86400
      else ((total_sec + 1) / 86400) - 1
    in
    let rem_sec = total_sec - (days * 86400) in
    let y, m, d = civil_from_days days in
    let hh = rem_sec / 3600 in
    let mm = rem_sec mod 3600 / 60 in
    let ss = rem_sec mod 60 in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" y m d hh mm ss

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
        | Some (`String "Datetime"), Some (`String s) -> (
            match parse_datetime_string s with
            | Ok f -> Datetime f
            | Error msg ->
                Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error msg json)
        | Some (`String "Date"), Some (`Float f) -> Date f
        | Some (`String "Date"), Some (`Int i) -> Date (float_of_int i)
        | Some (`String "Date"), Some (`String s) -> (
            match parse_date_string s with
            | Ok f -> Date f
            | Error msg ->
                Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error msg json)
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
                   | Some (`String s) -> (
                       match parse_datetime_string s with
                       | Ok f -> f
                       | Error msg ->
                           Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error msg
                             json)
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
                   | Some (`String s) -> (
                       match parse_date_string s with
                       | Ok f -> f
                       | Error msg ->
                           Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error msg
                             json)
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
