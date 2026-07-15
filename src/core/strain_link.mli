(** Interface file for strain_link.ml. Contains logic for resolving external
    database links. *)

val resolve_url :
  Types.external_db_definition -> Types.strain_external_link -> string option
(** [resolve_url db_def link] constructs a fully qualified URL by inserting a
    link value into an external database definition's URL template. Returns None
    if the template is invalid or missing. *)
