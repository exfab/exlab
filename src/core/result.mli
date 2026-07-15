(** Interface file for result.ml. Contains logic for validating result payloads
    against definitions. *)

open Types

val validate_payload : ResultType.t -> ResultPayload.t -> (unit, string) result
(** [validate_payload data_type payload] ensures that a given payload's shape
    matches the expected [data_type] defined by a [result_definition]. Returns
    Ok () if valid, or an Error message. *)

val validate_result :
  result_definition -> ResultPayload.t option -> (unit, string) result
(** [validate_result definition payload] validates a result payload against its
    definition, enforcing constraints like [is_required]. *)

val payload_of_string :
  ResultType.t -> string -> (ResultPayload.t, string) result
(** [payload_of_string data_type value] attempts to parse a string [value] into
    a [ResultPayload.t] according to the expected [data_type]. Returns Ok
    payload if successful, or an Error message. *)
