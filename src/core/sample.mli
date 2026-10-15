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
  owner:[ `Project of string | `Parent of string ] -> seq:int -> string
(** [generate_short_id ~owner ~seq] generates a new 1-based short ID for a
    sample from its owning project or parent sample short ID. *)
