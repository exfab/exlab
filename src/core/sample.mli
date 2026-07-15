(** Interface file for sample.ml. Contains business rules and constraints for
    Samples. *)

open Types

val valid_sample_types : string list
(** A list of all allowed sample types. *)

val validate_sample_type : string -> (unit, string) result
(** [validate_sample_type sample_type] checks if a string is one of the valid
    sample types. *)

val validate_creation :
  sample_type:string ->
  category:sample_category ->
  parent_sample_id:int option ->
  strain_id:int option ->
  community_id:int option ->
  (unit, string) result
(** [validate_creation ~sample_type ~category ~parent_sample_id ~strain_id
     ~community_id] performs a comprehensive check on all rules governing sample
    creation, including topological requirements (e.g. Source samples must have
    a strain_id or community_id but no parents, Experimental samples must have
    parents). *)

val generate_short_id :
  category:sample_category ->
  parent_short_id_opt:string option ->
  project_prefix_opt:string option ->
  count:int ->
  (string, string) result
(** [generate_short_id ~category ~parent_short_id_opt ~project_prefix_opt
     ~count] generates a new short ID for a sample based on its category and
    context. *)
