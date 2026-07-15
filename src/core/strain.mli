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

val format_label : Types.strain -> string
(** [format_label strain] produces a standardized display name for a strain,
    usually combining species and strain names. *)
