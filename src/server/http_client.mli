(** Minimal HTTP/1.1 & HTTPS GET client with SSRF / cloud-metadata protection
    and response timeouts. *)

val get_string :
  ?timeout:float ->
  ?max_bytes:int ->
  string ->
  (int * string, string) result Lwt.t
(** [get_string ?timeout ?max_bytes url] performs an HTTP(S) GET request and
    returns [(status_code, body)], rejecting link-local / cloud metadata targets
    and enforcing [timeout] seconds (default [20.0]) and [max_bytes] (default 64
    MiB). *)
