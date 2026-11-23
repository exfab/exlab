(** Vendor-neutral Object Storage URL resolver supporting HTTPS, Google Cloud
    Storage (gs:// via V4 RSA-SHA256), and S3 / MinIO compatible storage (s3://
    via AWS SigV4 HMAC-SHA256). *)

(** {1 Provider Credential Types & Parsers} *)

type gcs_credentials = {
  client_email : string;
  private_key : Mirage_crypto_pk.Rsa.priv;
}

type s3_credentials = {
  endpoint_url : string;
  region : string;
  access_key_id : string;
  secret_access_key : string;
}

type config = { gcs : gcs_credentials option; s3 : s3_credentials option }

let empty_config = { gcs = None; s3 = None }

type listed_object = { key : string; etag : string; size : int }
[@@deriving show, eq]

type list_bucket_page = {
  objects : listed_object list;
  next_continuation_token : string option;
}
[@@deriving show, eq]

let parse_endpoint_url (raw : string) :
    (string * string * string, string) result =
  let s = String_utils.strip_trailing_slashes (String.trim raw) in
  let scheme_opt =
    if String.starts_with ~prefix:"https://" s then Some ("https://", 8)
    else if String.starts_with ~prefix:"http://" s then Some ("http://", 7)
    else None
  in
  match scheme_opt with
  | None -> Error "S3 endpoint_url must start with https:// or http://"
  | Some (scheme_prefix, idx) ->
      let rest = String.sub s idx (String.length s - idx) in
      let host, base_path =
        match String.index_opt rest '/' with
        | None -> (rest, "")
        | Some slash_i ->
            ( String.sub rest 0 slash_i,
              String.sub rest slash_i (String.length rest - slash_i) )
      in
      if host = "" then Error "S3 endpoint_url must contain a valid host"
      else Ok (scheme_prefix ^ host, host, base_path)

let gcs_credentials_of_json_string (json_str : string) :
    (gcs_credentials, string) result =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `Assoc fields -> (
        let email_opt =
          match List.assoc_opt "client_email" fields with
          | Some (`String s) -> String_utils.non_empty s
          | _ -> None
        in
        let pem_opt =
          match List.assoc_opt "private_key" fields with
          | Some (`String s) when Option.is_some (String_utils.non_empty s) ->
              Some s
          | _ -> None
        in
        match (email_opt, pem_opt) with
        | Some client_email, Some pem -> (
            match X509.Private_key.decode_pem pem with
            | Ok (`RSA private_key) -> Ok { client_email; private_key }
            | Ok _ -> Error "Service account private_key must be an RSA key"
            | Error (`Msg m) -> Error ("Invalid PEM private_key: " ^ m))
        | _ ->
            Error
              "Service account JSON must contain client_email and private_key")
    | _ -> Error "Expected JSON object for service account credentials"
  with exn -> Error ("Invalid JSON: " ^ Printexc.to_string exn)

let gcs_credentials_of_base64 (b64_str : string) :
    (gcs_credentials, string) result =
  match Base64.decode (String.trim b64_str) with
  | Ok decoded -> gcs_credentials_of_json_string decoded
  | Error (`Msg m) -> Error ("Invalid base64 encoding: " ^ m)

let s3_credentials_of_json (json : Types.json) : (s3_credentials, string) result
    =
  match json with
  | `Assoc fields -> (
      let get_str k =
        match List.assoc_opt k fields with
        | Some (`String s) -> String_utils.non_empty s
        | _ -> None
      in
      let endpoint_url =
        Option.value ~default:"https://s3.amazonaws.com"
          (get_str "endpoint_url")
      in
      let region = Option.value ~default:"us-east-1" (get_str "region") in
      match (get_str "access_key_id", get_str "secret_access_key") with
      | Some access_key_id, Some secret_access_key -> (
          match parse_endpoint_url endpoint_url with
          | Ok _ ->
              Ok { endpoint_url; region; access_key_id; secret_access_key }
          | Error e -> Error e)
      | _ ->
          Error "S3 configuration requires access_key_id and secret_access_key")
  | _ -> Error "Invalid S3 configuration object"

(** {1 URI Encoding & V4 Signing Primitives} *)

let parse_bucket_uri ~scheme (uri : string) : (string * string, string) result =
  let s = String.trim uri in
  let prefix = scheme ^ "://" in
  let prefix_len = String.length prefix in
  if not (String.starts_with ~prefix s) then
    Error (Printf.sprintf "URI must start with %s" prefix)
  else
    let rest = String.sub s prefix_len (String.length s - prefix_len) in
    match String.index_opt rest '/' with
    | None -> Error (Printf.sprintf "%s URI must include an object path" prefix)
    | Some idx ->
        let bucket = String.sub rest 0 idx in
        let obj = String.sub rest (idx + 1) (String.length rest - idx - 1) in
        if bucket = "" || obj = "" then
          Error
            (Printf.sprintf "%s URI must have non-empty bucket and object path"
               prefix)
        else Ok (bucket, obj)

let uri_encode ~encode_slash (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      let is_unreserved =
        (c >= 'A' && c <= 'Z')
        || (c >= 'a' && c <= 'z')
        || (c >= '0' && c <= '9')
        || c = '-' || c = '.' || c = '_' || c = '~'
        || ((not encode_slash) && c = '/')
      in
      if is_unreserved then Buffer.add_char buf c
      else Printf.bprintf buf "%%%02X" (Char.code c))
    s;
  Buffer.contents buf

let build_sorted_query (params : (string * string) list) : string =
  params
  |> List.map (fun (k, v) ->
      (uri_encode ~encode_slash:true k, uri_encode ~encode_slash:true v))
  |> List.sort (fun (k1, v1) (k2, v2) ->
      let c = String.compare k1 k2 in
      if c <> 0 then c else String.compare v1 v2)
  |> List.map (fun (k, v) -> k ^ "=" ^ v)
  |> String.concat "&"

let format_v4_timestamps (epoch : float) : string * string =
  let total_sec = int_of_float (Float.floor epoch) in
  let days =
    if total_sec >= 0 then total_sec / 86400 else ((total_sec + 1) / 86400) - 1
  in
  let rem_sec = total_sec - (days * 86400) in
  let y, m, d = Types.ResultPayload.civil_from_days days in
  let hh = rem_sec / 3600 in
  let mm = rem_sec mod 3600 / 60 in
  let ss = rem_sec mod 60 in
  let datestamp = Printf.sprintf "%04d%02d%02d" y m d in
  let iso_timestamp =
    Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ" y m d hh mm ss
  in
  (datestamp, iso_timestamp)

let hex_of_string (s : string) : string =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Printf.bprintf buf "%02x" (Char.code c)) s;
  Buffer.contents buf

let hmac_sha256_raw ~key msg =
  Digestif.SHA256.(hmac_string ~key msg |> to_raw_string)

let sign_gcs_v4_request ~(creds : gcs_credentials) ~(canonical_uri : string)
    ~(extra_query_params : (string * string) list) ~(now : float)
    ~(expires_in : int) : (string, string) result =
  if expires_in < 1 || expires_in > 604800 then
    Error "expires_in must be between 1 and 604800 seconds"
  else
    let datestamp, iso_timestamp = format_v4_timestamps now in
    let credential_scope =
      Printf.sprintf "%s/auto/storage/goog4_request" datestamp
    in
    let credential =
      Printf.sprintf "%s/%s" creds.client_email credential_scope
    in
    let base_params =
      [
        ("X-Goog-Algorithm", "GOOG4-RSA-SHA256");
        ("X-Goog-Credential", credential);
        ("X-Goog-Date", iso_timestamp);
        ("X-Goog-Expires", string_of_int expires_in);
        ("X-Goog-SignedHeaders", "host");
      ]
    in
    let canonical_query_string =
      build_sorted_query (extra_query_params @ base_params)
    in
    let canonical_request =
      String.concat "\n"
        [
          "GET";
          canonical_uri;
          canonical_query_string;
          "host:storage.googleapis.com\n";
          "host";
          "UNSIGNED-PAYLOAD";
        ]
    in
    let string_to_sign =
      String.concat "\n"
        [
          "GOOG4-RSA-SHA256";
          iso_timestamp;
          credential_scope;
          Digestif.SHA256.(to_hex (digest_string canonical_request));
        ]
    in
    match
      X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1 (`RSA creds.private_key)
        (`Message string_to_sign)
    with
    | Error (`Msg m) -> Error ("RSA signing failed: " ^ m)
    | Ok sig_bytes ->
        Ok
          (Printf.sprintf
             "https://storage.googleapis.com%s?%s&X-Goog-Signature=%s"
             canonical_uri canonical_query_string (hex_of_string sig_bytes))

let sign_s3_v4_request ~(creds : s3_credentials) ~(resource_subpath : string)
    ~(extra_query_params : (string * string) list) ~(now : float)
    ~(expires_in : int) : (string, string) result =
  if expires_in < 1 || expires_in > 604800 then
    Error "expires_in must be between 1 and 604800 seconds"
  else
    match parse_endpoint_url creds.endpoint_url with
    | Error e -> Error e
    | Ok (origin, host, base_path) ->
        let region =
          let r = String.trim creds.region in
          if r = "" then "us-east-1" else r
        in
        let datestamp, amz_date = format_v4_timestamps now in
        let credential_scope =
          Printf.sprintf "%s/%s/s3/aws4_request" datestamp region
        in
        let credential =
          Printf.sprintf "%s/%s"
            (String.trim creds.access_key_id)
            credential_scope
        in
        let canonical_uri =
          uri_encode ~encode_slash:false
            (Printf.sprintf "%s/%s" base_path resource_subpath)
        in
        let base_params =
          [
            ("X-Amz-Algorithm", "AWS4-HMAC-SHA256");
            ("X-Amz-Credential", credential);
            ("X-Amz-Date", amz_date);
            ("X-Amz-Expires", string_of_int expires_in);
            ("X-Amz-SignedHeaders", "host");
          ]
        in
        let canonical_query_string =
          build_sorted_query (extra_query_params @ base_params)
        in
        let canonical_request =
          String.concat "\n"
            [
              "GET";
              canonical_uri;
              canonical_query_string;
              Printf.sprintf "host:%s\n" host;
              "host";
              "UNSIGNED-PAYLOAD";
            ]
        in
        let string_to_sign =
          String.concat "\n"
            [
              "AWS4-HMAC-SHA256";
              amz_date;
              credential_scope;
              Digestif.SHA256.(to_hex (digest_string canonical_request));
            ]
        in
        let k_date =
          hmac_sha256_raw ~key:("AWS4" ^ creds.secret_access_key) datestamp
        in
        let k_region = hmac_sha256_raw ~key:k_date region in
        let k_service = hmac_sha256_raw ~key:k_region "s3" in
        let k_signing = hmac_sha256_raw ~key:k_service "aws4_request" in
        let signature_hex =
          Digestif.SHA256.(hmac_string ~key:k_signing string_to_sign |> to_hex)
        in
        Ok
          (Printf.sprintf "%s%s?%s&X-Amz-Signature=%s" origin canonical_uri
             canonical_query_string signature_hex)

(** {1 Vendor-Neutral URL Resolution & Bucket Listing} *)

let resolve_download_url ~(config : config) ~(uri : string) ~(now : float)
    ~(expires_in : int) : (string, string) result =
  let trimmed = String.trim uri in
  if
    String.starts_with ~prefix:"https://" trimmed
    || String.starts_with ~prefix:"http://" trimmed
  then Ok trimmed
  else if String.starts_with ~prefix:"gs://" trimmed then
    match (config.gcs, parse_bucket_uri ~scheme:"gs" trimmed) with
    | None, _ ->
        Error
          "Google Cloud Storage (gs://) connection is not configured in Admin \
           Settings"
    | _, Error e -> Error e
    | Some creds, Ok (bucket, object_path) ->
        let canonical_uri =
          "/" ^ uri_encode ~encode_slash:false (bucket ^ "/" ^ object_path)
        in
        sign_gcs_v4_request ~creds ~canonical_uri ~extra_query_params:[] ~now
          ~expires_in
  else if String.starts_with ~prefix:"s3://" trimmed then
    match (config.s3, parse_bucket_uri ~scheme:"s3" trimmed) with
    | None, _ ->
        Error
          "S3 / MinIO (s3://) connection is not configured in Admin Settings"
    | _, Error e -> Error e
    | Some creds, Ok (bucket, object_path) ->
        let resource_subpath = Printf.sprintf "%s/%s" bucket object_path in
        sign_s3_v4_request ~creds ~resource_subpath ~extra_query_params:[] ~now
          ~expires_in
  else
    Error "Unsupported storage URI scheme (expected https://, gs://, or s3://)"

let sign_list_objects_url ~(config : config) ~(folder_uri : string)
    ?(continuation_token : string option) ~(now : float) ~(expires_in : int) ()
    : (string, string) result =
  let trimmed = String.trim folder_uri in
  let ensure_slash s = if String.ends_with ~suffix:"/" s then s else s ^ "/" in
  let list_query_params ~prefix =
    let base =
      [ ("list-type", "2"); ("max-keys", "1000"); ("prefix", prefix) ]
    in
    match continuation_token with
    | Some tok when tok <> "" -> ("continuation-token", tok) :: base
    | _ -> base
  in
  if String.starts_with ~prefix:"gs://" trimmed then
    match (config.gcs, parse_bucket_uri ~scheme:"gs" trimmed) with
    | None, _ ->
        Error
          "Google Cloud Storage (gs://) connection is not configured in Admin \
           Settings"
    | _, Error e -> Error e
    | Some creds, Ok (bucket, prefix_raw) ->
        let canonical_uri = "/" ^ uri_encode ~encode_slash:false bucket in
        let extra_query_params =
          list_query_params ~prefix:(ensure_slash prefix_raw)
        in
        sign_gcs_v4_request ~creds ~canonical_uri ~extra_query_params ~now
          ~expires_in
  else if String.starts_with ~prefix:"s3://" trimmed then
    match (config.s3, parse_bucket_uri ~scheme:"s3" trimmed) with
    | None, _ ->
        Error
          "S3 / MinIO (s3://) connection is not configured in Admin Settings"
    | _, Error e -> Error e
    | Some creds, Ok (bucket, prefix_raw) ->
        let extra_query_params =
          list_query_params ~prefix:(ensure_slash prefix_raw)
        in
        sign_s3_v4_request ~creds ~resource_subpath:bucket ~extra_query_params
          ~now ~expires_in
  else
    Error
      "Bucket auto-discovery requires a gs:// or s3:// folder URI (configure \
       credentials in Admin Panel)"

let strip_etag_quotes (s : string) : string =
  let t = String.trim s in
  let len = String.length t in
  if len >= 2 && t.[0] = '"' && t.[len - 1] = '"' then String.sub t 1 (len - 2)
  else t

let parse_list_bucket_xml (xml_str : string) : list_bucket_page =
  let signals =
    Markup.string xml_str |> Markup.parse_xml |> Markup.signals
    |> Markup.to_list
  in
  let stack = ref [] in
  let is_truncated = ref false in
  let next_token = ref None in
  let in_contents = ref false in
  let cur_key = ref "" in
  let cur_etag = ref "" in
  let cur_size = ref 0 in
  let objects = ref [] in

  List.iter
    (function
      | `Start_element ((_, local_name), _) ->
          stack := local_name :: !stack;
          if local_name = "Contents" then (
            in_contents := true;
            cur_key := "";
            cur_etag := "";
            cur_size := 0)
      | `End_element -> (
          match !stack with
          | "Contents" :: rest ->
              stack := rest;
              in_contents := false;
              if !cur_key <> "" then
                objects :=
                  {
                    key = !cur_key;
                    etag = strip_etag_quotes !cur_etag;
                    size = !cur_size;
                  }
                  :: !objects
          | _ :: rest -> stack := rest
          | [] -> ())
      | `Text strs -> (
          let txt = String.concat "" strs in
          match !stack with
          | "IsTruncated" :: _ when not !in_contents ->
              is_truncated := String_utils.to_bool txt
          | "NextContinuationToken" :: _ when not !in_contents -> (
              match String_utils.non_empty txt with
              | Some t -> next_token := Some t
              | None -> ())
          | "Key" :: "Contents" :: _ -> cur_key := !cur_key ^ txt
          | "ETag" :: "Contents" :: _ -> cur_etag := !cur_etag ^ txt
          | "Size" :: "Contents" :: _ -> (
              match String_utils.to_int_option txt with
              | Some n -> cur_size := n
              | None -> ())
          | _ -> ())
      | _ -> ())
    signals;
  {
    objects = List.rev !objects;
    next_continuation_token = (if !is_truncated then !next_token else None);
  }

(** {1 Encrypted Settings & Multi-Cloud Configuration Merging} *)

let storage_adata = "exlab.system_settings.storage_connections.v1"

let derive_aes256_key (master_secret : string) : Mirage_crypto.AES.GCM.key =
  let raw_32 =
    Digestif.SHA256.(
      hmac_string ~key:"exlab-storage-kdf-v1" master_secret |> to_raw_string)
  in
  Mirage_crypto.AES.GCM.of_secret raw_32

let seal_stored_json ~(master_secret : string) ~(nonce : string)
    (plain_json : Types.json) : Types.json =
  let key = derive_aes256_key master_secret in
  let plaintext = Yojson.Safe.to_string plain_json in
  let ciphertext_and_tag =
    Mirage_crypto.AES.GCM.authenticate_encrypt ~key ~nonce ~adata:storage_adata
      plaintext
  in
  `Assoc
    [
      ("enc", `String "aes-256-gcm-v1");
      ("nonce", `String (Base64.encode_exn nonce));
      ("ciphertext", `String (Base64.encode_exn ciphertext_and_tag));
    ]

let open_stored_json ~(master_secret : string) (stored_opt : Types.json option)
    : Types.json option =
  match stored_opt with
  | None -> None
  | Some (`Assoc fields as raw) -> (
      match
        ( List.assoc_opt "enc" fields,
          List.assoc_opt "nonce" fields,
          List.assoc_opt "ciphertext" fields )
      with
      | ( Some (`String "aes-256-gcm-v1"),
          Some (`String nonce_b64),
          Some (`String ct_b64) ) -> (
          match (Base64.decode nonce_b64, Base64.decode ct_b64) with
          | Ok nonce, Ok ct when String.length nonce = 12 -> (
              let key = derive_aes256_key master_secret in
              match
                Mirage_crypto.AES.GCM.authenticate_decrypt ~key ~nonce
                  ~adata:storage_adata ct
              with
              | Some plaintext -> (
                  try Some (Yojson.Safe.from_string plaintext) with _ -> None)
              | None -> None)
          | _ -> None)
      | _ ->
          (* Backwards-compatible fallback if an older unsealed row existed *)
          Some raw)
  | Some other -> Some other

type stored_provider_state = {
  db_gcs : gcs_credentials option;
  gcs_disabled : bool;
  db_s3 : s3_credentials option;
  s3_disabled : bool;
}

let parse_stored_provider_state (stored_opt : Types.json option) :
    stored_provider_state =
  match stored_opt with
  | Some (`Assoc fields) ->
      let bool_field key =
        match List.assoc_opt key fields with
        | Some (`Bool true) -> true
        | _ -> false
      in
      let db_gcs =
        match List.assoc_opt "gcs_service_account_json" fields with
        | Some (`String sa_str) ->
            Option.bind (String_utils.non_empty sa_str) (fun s ->
                Stdlib.Result.to_option (gcs_credentials_of_json_string s))
        | _ -> None
      in
      let db_s3 =
        match List.assoc_opt "s3" fields with
        | Some s3_json when s3_json <> `Null ->
            Stdlib.Result.to_option (s3_credentials_of_json s3_json)
        | _ -> None
      in
      {
        db_gcs;
        gcs_disabled = bool_field "gcs_disabled";
        db_s3;
        s3_disabled = bool_field "s3_disabled";
      }
  | _ ->
      { db_gcs = None; gcs_disabled = false; db_s3 = None; s3_disabled = false }

let merge_provider ~db_opt ~disabled ~env_opt =
  match db_opt with
  | Some _ -> db_opt
  | None -> if disabled then None else env_opt

let config_of_stored_json ~(env_config : config)
    (stored_opt : Types.json option) : config =
  let st = parse_stored_provider_state stored_opt in
  {
    gcs =
      merge_provider ~db_opt:st.db_gcs ~disabled:st.gcs_disabled
        ~env_opt:env_config.gcs;
    s3 =
      merge_provider ~db_opt:st.db_s3 ~disabled:st.s3_disabled
        ~env_opt:env_config.s3;
  }

let masked_summary_of_stored_json ~(env_config : config)
    (stored_opt : Types.json option) : Types.json =
  let st = parse_stored_provider_state stored_opt in
  let gcs_summary_fields source (c : gcs_credentials) =
    `Assoc
      [
        ("configured", `Bool true);
        ("source", `String source);
        ("client_email", `String c.client_email);
      ]
  in
  let s3_summary_fields source (c : s3_credentials) =
    `Assoc
      [
        ("configured", `Bool true);
        ("source", `String source);
        ("endpoint_url", `String c.endpoint_url);
        ("region", `String c.region);
        ("access_key_id", `String c.access_key_id);
      ]
  in
  let gcs_summary =
    match (st.db_gcs, env_config.gcs) with
    | Some c, _ -> gcs_summary_fields "admin_settings" c
    | None, Some c when not st.gcs_disabled ->
        gcs_summary_fields "environment" c
    | _ -> `Assoc [ ("configured", `Bool false) ]
  in
  let s3_summary =
    match (st.db_s3, env_config.s3) with
    | Some c, _ -> s3_summary_fields "admin_settings" c
    | None, Some c when not st.s3_disabled -> s3_summary_fields "environment" c
    | _ -> `Assoc [ ("configured", `Bool false) ]
  in
  `Assoc [ ("gcs", gcs_summary); ("s3", s3_summary) ]
