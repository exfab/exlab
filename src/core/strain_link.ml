(** Implementation of strain link business logic. *)

(** URL Logic: Template substitution Replaces "\{\}" in the template with the
    link value. *)
let resolve_url (db_def : Types.external_db_definition)
    (link : Types.strain_external_link) =
  match db_def.url_template with
  | None -> None
  | Some template -> (
      match String.split_on_char '{' template with
      | [ prefix; rest ] ->
          if String.length rest > 0 && String.get rest 0 = '}' then
            let suffix = String.sub rest 1 (String.length rest - 1) in
            Some (prefix ^ link.value ^ suffix)
          else None
      | _ -> None)
