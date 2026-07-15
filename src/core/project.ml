(** Implementation of project business logic. *)

let generate_short_id ~project_id = Printf.sprintf "PROJ-%04d" project_id

let validate_metadata metadata_opt (template : Types.metadata_field_def list) =
  match metadata_opt with
  | None -> Ok ()
  | Some (`Assoc fields) ->
      let rec validate_fields = function
        | [] -> Ok ()
        | (key, value) :: rest -> (
            match
              List.find_opt
                (fun (def : Types.metadata_field_def) -> def.key = key)
                template
            with
            | None ->
                Error
                  (Printf.sprintf
                     "Metadata key '%s' is not defined in the template." key)
            | Some def -> (
                match def.field_type with
                | Types.String -> (
                    match value with
                    | `String _ -> validate_fields rest
                    | _ ->
                        Error
                          (Printf.sprintf "Metadata key '%s' must be a string."
                             key))
                | Types.Enum allowed_values -> (
                    match value with
                    | `String s when List.mem s allowed_values ->
                        validate_fields rest
                    | `String s ->
                        let allowed_str = String.concat ", " allowed_values in
                        Error
                          (Printf.sprintf
                             "Metadata key '%s' must be one of: [%s]. Got: '%s'"
                             key allowed_str s)
                    | _ ->
                        Error
                          (Printf.sprintf "Metadata key '%s' must be a string."
                             key))))
      in
      validate_fields fields
  | _ -> Error "Metadata must be a JSON object"
