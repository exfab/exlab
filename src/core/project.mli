(** Interface file for project.ml. Contains business logic for projects. *)

val generate_short_id : project_id:int -> string
(** [generate_short_id ~project_id] generates a short ID for a project. *)

val validate_metadata :
  Yojson.Safe.t option -> Types.metadata_field_def list -> (unit, string) result
(** [validate_metadata metadata_opt template] validates project metadata against
    the given template. *)
