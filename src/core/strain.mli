(** Interface file for strain.ml. Contains logic for processing and formatting
    strain data. *)

val normalize_name : string -> string
(** [normalize_name input] trims and capitalizes the first letter of a name
    (like genus or species). *)

val validate_creation :
  genus:string ->
  species:string ->
  strain_name:string ->
  genotype:string option ->
  parent_strain_id:int option ->
  (string * string * string * string option * int option, string) result
(** [validate_creation ~genus ~species ~strain_name ~genotype ~parent_strain_id]
    validates and normalizes inputs for strain creation. Returns cleaned data
    tuple or an Error. *)

val default_taxonomic_groups : string list
(** Default controlled vocabulary for [strain.taxonomic_group] when no custom
    [strain_taxonomic_groups] setting overrides it. *)

val validate_domain : string option -> (Types.strain_domain, string) result
(** [validate_domain domain_opt] parses and normalizes an optional strain domain
    string, defaulting [None] to [Types.Unassigned]. *)

val validate_taxonomic_group :
  allowed_groups:string list -> string option -> (string option, string) result
(** [validate_taxonomic_group ~allowed_groups group_opt] normalizes an optional
    taxonomic group against [allowed_groups] (case-insensitively), mapping
    [None] or blank strings to [Ok None]. *)

val validate_classification :
  allowed_groups:string list ->
  domain:string option ->
  taxonomic_group:string option ->
  (Types.strain_domain * string option, string) result
(** [validate_classification ~allowed_groups ~domain ~taxonomic_group] validates
    both classification attributes together. *)

val format_label : Types.strain -> string
(** [format_label strain] produces a standardized display name for a strain,
    usually combining species and strain names. *)
