(** Minimal HTTP/1.1 & HTTPS GET client built on Dream's existing httpun +
    lwt_ssl stack (zero extra opam packages). *)

open Lwt.Syntax

let () = Ssl.init ()

type parsed_url = { is_tls : bool; host : string; port : int; target : string }

let parse_http_url (url : string) : (parsed_url, string) result =
  let s = String.trim url in
  let scheme_opt =
    if String.starts_with ~prefix:"https://" s then Some (true, 8, 443)
    else if String.starts_with ~prefix:"http://" s then Some (false, 7, 80)
    else None
  in
  match scheme_opt with
  | None -> Error "URL must start with http:// or https://"
  | Some (is_tls, idx, default_port) -> (
      let rest = String.sub s idx (String.length s - idx) in
      let host_port, target =
        match String.index_opt rest '/' with
        | None -> (rest, "/")
        | Some slash_i ->
            ( String.sub rest 0 slash_i,
              String.sub rest slash_i (String.length rest - slash_i) )
      in
      let host, port_res =
        match String.index_opt host_port ':' with
        | None -> (host_port, Ok default_port)
        | Some col_i -> (
            let h = String.sub host_port 0 col_i in
            let p_str =
              String.sub host_port (col_i + 1)
                (String.length host_port - col_i - 1)
            in
            match int_of_string_opt p_str with
            | Some p -> (h, Ok p)
            | None -> (h, Error "Invalid port in URL"))
      in
      match port_res with
      | Error e -> Error e
      | Ok port ->
          if host = "" then Error "Empty host in URL"
          else Ok { is_tls; host; port; target })

let parse_ipv4_octets (ip : string) : (int * int * int * int) option =
  match String.split_on_char '.' ip with
  | [ a; b; c; d ] -> (
      match
        ( int_of_string_opt a,
          int_of_string_opt b,
          int_of_string_opt c,
          int_of_string_opt d )
      with
      | Some o1, Some o2, Some o3, Some o4
        when o1 >= 0 && o1 <= 255 && o2 >= 0 && o2 <= 255 && o3 >= 0
             && o3 <= 255 && o4 >= 0 && o4 <= 255 ->
          Some (o1, o2, o3, o4)
      | _ -> None)
  | _ -> None

let is_ipv4_literal (s : string) : bool = Option.is_some (parse_ipv4_octets s)

let is_blocked_ipv4 (ip : string) : bool =
  match parse_ipv4_octets ip with
  | Some (o1, o2, o3, o4) ->
      o1 = 0
      || (o1 = 169 && o2 = 254)
      || (o1 = 100 && o2 = 100 && o3 = 100 && o4 = 200)
  | None -> false

let is_blocked_hostname (host : string) : bool =
  let h = String.lowercase_ascii (String.trim host) in
  let h =
    if String.ends_with ~suffix:"." h then String.sub h 0 (String.length h - 1)
    else h
  in
  h = "metadata.google.internal"
  || h = "metadata.goog" || h = "metadata" || h = "instance-data"
  || is_blocked_ipv4 h

let is_blocked_sockaddr (addr : Unix.sockaddr) : bool =
  match addr with
  | Unix.ADDR_INET (inet_addr, _) ->
      is_blocked_ipv4 (Unix.string_of_inet_addr inet_addr)
  | Unix.ADDR_UNIX _ -> true

let safe_close_fd fd_opt_ref =
  match !fd_opt_ref with
  | None -> Lwt.return ()
  | Some fd ->
      fd_opt_ref := None;
      Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return ())

let resolve_safe_addr ~host ~port : (Unix.addr_info, string) result Lwt.t =
  let* addrs =
    Lwt_unix.getaddrinfo host (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET); Unix.(AI_SOCKTYPE SOCK_STREAM) ]
  in
  match addrs with
  | [] -> Lwt.return (Error ("Could not resolve host: " ^ host))
  | _
    when List.exists
           (fun (ai : Unix.addr_info) -> is_blocked_sockaddr ai.ai_addr)
           addrs ->
      Lwt.return
        (Error ("Blocked disallowed or cloud metadata address for host: " ^ host))
  | addr_info :: _ -> Lwt.return (Ok addr_info)

let build_get_request (parsed : parsed_url) : Httpun.Request.t =
  let host_header =
    if
      (parsed.is_tls && parsed.port = 443)
      || ((not parsed.is_tls) && parsed.port = 80)
    then parsed.host
    else Printf.sprintf "%s:%d" parsed.host parsed.port
  in
  Httpun.Request.create
    ~headers:
      (Httpun.Headers.of_list
         [ ("Host", host_header); ("Connection", "close") ])
    `GET parsed.target

let connect_tls_socket ~host (fd : Lwt_unix.file_descr) : Lwt_ssl.socket Lwt.t =
  let ctx =
    Ssl.create_context (Ssl.SSLv23 [@alert "-deprecated"]) Ssl.Client_context
  in
  Ssl.disable_protocols ctx
    [
      (Ssl.SSLv3 [@alert "-deprecated"]);
      (Ssl.TLSv1 [@alert "-deprecated"]);
      (Ssl.TLSv1_1 [@alert "-deprecated"]);
    ];
  ignore (Ssl.set_default_verify_paths ctx);
  Ssl.set_verify ctx [ Ssl.Verify_peer ] None;
  let uninit = Lwt_ssl.embed_uninitialized_socket fd ctx in
  let raw_ssl = Lwt_ssl.ssl_socket_of_uninitialized_socket uninit in
  if is_ipv4_literal host then Ssl.set_ip raw_ssl host
  else (
    Ssl.set_client_SNI_hostname raw_ssl host;
    Ssl.set_host raw_ssl host);
  Lwt_ssl.ssl_perform_handshake uninit

let exchange_http ~max_bytes ~(parsed : parsed_url) (fd : Lwt_unix.file_descr) :
    (int * string, string) result Lwt.t =
  let response_promise, notify_response = Lwt.task () in
  let settled = ref false in
  let finish v =
    if not !settled then (
      settled := true;
      Lwt.wakeup_later notify_response v)
  in
  let req = build_get_request parsed in
  let error_handler _err =
    finish (Error "HTTP connection error while reading bucket")
  in
  let response_handler (response : Httpun.Response.t) response_body =
    let status = Httpun.Status.to_code response.status in
    let buf = Buffer.create 4096 in
    let rec on_read bs ~off ~len =
      if Buffer.length buf + len <= max_bytes then
        Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
      Httpun.Body.Reader.schedule_read response_body ~on_eof ~on_read
    and on_eof () = finish (Ok (status, Buffer.contents buf)) in
    Httpun.Body.Reader.schedule_read response_body ~on_eof ~on_read
  in
  if parsed.is_tls then (
    let* ssl_sock = connect_tls_socket ~host:parsed.host fd in
    let* client = Httpun_lwt_unix.Client.SSL.create_connection ssl_sock in
    let req_body =
      Httpun_lwt_unix.Client.SSL.request client req ~error_handler
        ~response_handler
    in
    Httpun.Body.Writer.close req_body;
    let* res = response_promise in
    let* () = Httpun_lwt_unix.Client.SSL.shutdown client in
    Lwt.return res)
  else
    let* client = Httpun_lwt_unix.Client.create_connection fd in
    let req_body =
      Httpun_lwt_unix.Client.request client req ~error_handler ~response_handler
    in
    Httpun.Body.Writer.close req_body;
    let* res = response_promise in
    let* () = Httpun_lwt_unix.Client.shutdown client in
    Lwt.return res

let fetch_once ~max_bytes ~fd_ref (parsed : parsed_url) :
    (int * string, string) result Lwt.t =
  let* addr_res = resolve_safe_addr ~host:parsed.host ~port:parsed.port in
  match addr_res with
  | Error _ as err -> Lwt.return err
  | Ok addr_info ->
      let fd =
        Lwt_unix.socket addr_info.ai_family addr_info.ai_socktype
          addr_info.ai_protocol
      in
      fd_ref := Some fd;
      let* () = Lwt_unix.connect fd addr_info.ai_addr in
      exchange_http ~max_bytes ~parsed fd

let get_string ?(timeout = 20.0) ?(max_bytes = 64 * 1024 * 1024) (url : string)
    : (int * string, string) result Lwt.t =
  match parse_http_url url with
  | Error e -> Lwt.return (Error e)
  | Ok parsed when is_blocked_hostname parsed.host ->
      Lwt.return
        (Error ("Blocked disallowed or cloud metadata host: " ^ parsed.host))
  | Ok parsed ->
      let fd_ref = ref None in
      Lwt.finalize
        (fun () ->
          Lwt.catch
            (fun () ->
              Lwt_unix.with_timeout timeout (fun () ->
                  fetch_once ~max_bytes ~fd_ref parsed))
            (function
              | Lwt_unix.Timeout ->
                  Lwt.return
                    (Error
                       (Printf.sprintf "HTTP request timed out after %.2fs"
                          timeout))
              | exn ->
                  Lwt.return
                    (Error
                       ("Bucket HTTP request failed: " ^ Printexc.to_string exn))))
        (fun () -> safe_close_fd fd_ref)
