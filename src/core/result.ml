open Types

let validate_payload data_type payload =
  match (data_type, payload) with
  | ResultType.String, ResultPayload.String _ -> Ok ()
  | ResultType.Integer, ResultPayload.Integer _ -> Ok ()
  | ResultType.Float, ResultPayload.Float _ -> Ok ()
  | ResultType.Boolean, ResultPayload.Boolean _ -> Ok ()
  | ResultType.Date, ResultPayload.Date _ -> Ok ()
  | ResultType.Datetime, ResultPayload.Datetime _ -> Ok ()
  | ResultType.FileLink, ResultPayload.FileLink _ -> Ok ()
  | ResultType.StringSeries, ResultPayload.StringSeries _ -> Ok ()
  | ResultType.IntegerSeries, ResultPayload.IntegerSeries _ -> Ok ()
  | ResultType.FloatSeries, ResultPayload.FloatSeries _ -> Ok ()
  | ResultType.BooleanSeries, ResultPayload.BooleanSeries _ -> Ok ()
  | ResultType.DateSeries, ResultPayload.DateSeries _ -> Ok ()
  | ResultType.DatetimeSeries, ResultPayload.DatetimeSeries _ -> Ok ()
  | ResultType.FileLinkSeries, ResultPayload.FileLinkSeries _ -> Ok ()
  | _ -> Error "Mismatched data_type and payload"

let validate_result (definition : result_definition) payload =
  match payload with
  | None -> if definition.is_required then Error "Field is required" else Ok ()
  | Some p -> validate_payload definition.data_type p

let payload_of_string (data_type : ResultType.t) value =
  match data_type with
  | ResultType.String -> Ok (ResultPayload.String value)
  | ResultType.Integer -> (
      match int_of_string_opt value with
      | Some i -> Ok (ResultPayload.Integer i)
      | None -> Error (Printf.sprintf "Could not parse '%s' as Integer" value))
  | ResultType.Float -> (
      match float_of_string_opt value with
      | Some f -> Ok (ResultPayload.Float f)
      | None -> Error (Printf.sprintf "Could not parse '%s' as Float" value))
  | ResultType.Boolean -> (
      match bool_of_string_opt (String.lowercase_ascii value) with
      | Some b -> Ok (ResultPayload.Boolean b)
      | None -> Error (Printf.sprintf "Could not parse '%s' as Boolean" value))
  | ResultType.Datetime -> (
      match float_of_string_opt value with
      | Some f -> Ok (ResultPayload.Datetime f)
      | None -> Error (Printf.sprintf "Could not parse '%s' as Datetime" value))
  | ResultType.Date -> (
      match float_of_string_opt value with
      | Some f -> Ok (ResultPayload.Date f)
      | None -> Error (Printf.sprintf "Could not parse '%s' as Date" value))
  | ResultType.FileLink -> Ok (ResultPayload.FileLink value)
  | ResultType.StringSeries | ResultType.IntegerSeries | ResultType.FloatSeries
  | ResultType.BooleanSeries | ResultType.DatetimeSeries | ResultType.DateSeries
  | ResultType.FileLinkSeries -> (
      try
        let json = Yojson.Safe.from_string value in
        match json with
        | `List points ->
            let mapped_points =
              List.map
                (function
                  | `List [ t; v ] -> `Assoc [ ("time", t); ("value", v) ]
                  | _ -> failwith "Points must be [time, value]")
                points
            in
            let type_str = ResultType.to_string data_type in
            let wrapped_json =
              `Assoc
                [ ("type", `String type_str); ("value", `List mapped_points) ]
            in
            Ok (ResultPayload.t_of_yojson wrapped_json)
        | _ -> Error "TimeSeries must be a JSON array"
      with
      | Yojson.Json_error msg -> Error ("Invalid JSON for TimeSeries: " ^ msg)
      | Failure msg -> Error msg
      | exn -> Error (Printexc.to_string exn))
