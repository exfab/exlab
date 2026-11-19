(** Vendor-neutral Object Storage facade supporting direct HTTPS passthrough,
    Google Cloud Storage ([gs://] via V4 RSA-SHA256), and S3 / MinIO compatible
    storage ([s3://] via AWS SigV4 HMAC-SHA256).

    Provider-specific credential types and parsers ({!gcs_credentials},
    {!s3_credentials}) populate the unified {!config} container consumed by
    vendor-neutral URL resolution, bucket listing, and encrypted settings
    helpers. *)

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

val empty_config : config

val gcs_credentials_of_json_string : string -> (gcs_credentials, string) result
(** Parse a Google Cloud IAM Service Account JSON key containing [client_email]
    and an RSA [private_key] PEM. *)

val gcs_credentials_of_base64 : string -> (gcs_credentials, string) result
(** Decode a base64-encoded Google Cloud IAM Service Account JSON key. *)

val s3_credentials_of_json : Types.json -> (s3_credentials, string) result
(** Parse S3 / MinIO credentials ([access_key_id], [secret_access_key], and
    optional [endpoint_url] / [region]) from a JSON object. *)

(** {1 Vendor-Neutral URI & Bucket Operations} *)

type listed_object = { key : string; etag : string; size : int }
[@@deriving show, eq]

type list_bucket_page = {
  objects : listed_object list;
  next_continuation_token : string option;
}
[@@deriving show, eq]

val parse_bucket_uri :
  scheme:string -> string -> (string * string, string) result
(** [parse_bucket_uri ~scheme uri] splits ["<scheme>://<bucket>/<object_path>"]
    into [(bucket, object_path)]. *)

val resolve_download_url :
  config:config ->
  uri:string ->
  now:float ->
  expires_in:int ->
  (string, string) result
(** Resolve a storage URI ([https://], [gs://], or [s3://]) into a time-limited
    signed HTTPS download URL using the matching provider in [config]. *)

val sign_list_objects_url :
  config:config ->
  folder_uri:string ->
  ?continuation_token:string ->
  now:float ->
  expires_in:int ->
  unit ->
  (string, string) result
(** Sign a S3 ListObjectsV2-compatible XML listing URL for a [gs://] or [s3://]
    folder prefix. *)

val parse_list_bucket_xml : string -> list_bucket_page
(** Parse a ListBucketResult XML payload returned by GCS or S3 / MinIO. *)

(** {1 Encrypted Settings & Multi-Cloud Configuration Merging} *)

val seal_stored_json :
  master_secret:string -> nonce:string -> Types.json -> Types.json
(** Encrypt a plaintext [storage_connections] JSON document using AES-256-GCM
    authenticated encryption. *)

val open_stored_json :
  master_secret:string -> Types.json option -> Types.json option
(** Decrypt an AES-256-GCM sealed [storage_connections] JSON document, returning
    [None] if authentication fails. *)

val config_of_stored_json : env_config:config -> Types.json option -> config
(** Merge decrypted Admin Settings credentials with server environment fallback
    credentials ([env_config]), honoring explicit per-provider revocation flags.
*)

val masked_summary_of_stored_json :
  env_config:config -> Types.json option -> Types.json
(** Build a secret-free JSON summary of active cloud storage connections and
    their sources (["admin_settings"] vs ["environment"]) for the Admin UI. *)
