(** Implementation of sample business logic. *)

open Types

(* The set of valid sample types. *)
let valid_sample_types =
  [ "Liquid Cell Culture"; "Solid Cell Culture"; "Library Sample"; "Unknown" ]

let validate_sample_type sample_type =
  if List.mem sample_type valid_sample_types then Ok ()
  else
    Error
      ("Invalid sample_type: '" ^ sample_type ^ "'. " ^ "Must be one of: "
      ^ String.concat ", " valid_sample_types)

let validate_topology ~category ~parent_sample_id ~strain_id ~community_id =
  match (category, parent_sample_id, strain_id, community_id) with
  | Source, Some _, _, _ ->
      Error "A Source sample cannot have a parent_sample_id."
  | Source, _, None, None ->
      Error "A Source sample must have a strain_id or community_id."
  | Source, _, Some _, Some _ ->
      Error "A Source sample cannot have both a strain_id and a community_id."
  | Experimental, None, _, _ ->
      Error "An Experimental sample must have a parent_sample_id."
  | _ -> Ok ()

(* All creation checks are bundled into a single validator. *)
let validate_creation ~sample_type ~category ~parent_sample_id ~strain_id
    ~community_id =
  match validate_sample_type sample_type with
  | Error msg -> Error msg
  | Ok () ->
      validate_topology ~category ~parent_sample_id ~strain_id ~community_id

let generate_short_id ~owner ~seq =
  match owner with
  | `Project project_short_id -> Printf.sprintf "%s-%04d" project_short_id seq
  | `Parent parent_short_id -> Printf.sprintf "%s-%02d" parent_short_id seq
