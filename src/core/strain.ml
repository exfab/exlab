(** Implementation of strain business logic. *)

let normalize_name input =
  match String_utils.non_empty input with
  | None -> ""
  | Some trimmed ->
      let first = String.sub trimmed 0 1 |> String.uppercase_ascii in
      let rest = String.sub trimmed 1 (String.length trimmed - 1) in
      first ^ rest

let validate_creation ~genus ~species ~strain_name ~genotype ~parent_strain_id =
  match
    ( normalize_name genus,
      String_utils.non_empty species,
      String_utils.non_empty strain_name )
  with
  | "", _, _ -> Error "Genus cannot be empty"
  | _, None, _ -> Error "Species cannot be empty"
  | _, _, None -> Error "Strain Name cannot be empty"
  | clean_genus, Some clean_species, Some clean_strain -> (
      let clean_genotype = String_utils.clean_option genotype in
      match parent_strain_id with
      | Some id when id <= 0 -> Error "Parent Strain ID must be positive"
      | pid -> Ok (clean_genus, clean_species, clean_strain, clean_genotype, pid)
      )

let unassigned_domain = "Unassigned"
let default_domains = [ "Fungi"; "Bacteria"; "Archaea"; "Algae" ]
let default_taxonomic_groups = [ "Yeast"; "Filamentous Fungi"; "Macrofungi" ]

let validate_domain ~allowed_domains domain_opt =
  match String_utils.clean_option domain_opt with
  | None -> Ok unassigned_domain
  | Some trimmed -> (
      let all_allowed = unassigned_domain :: allowed_domains in
      match String_utils.find_canonical_ci trimmed all_allowed with
      | Some canonical -> Ok canonical
      | None ->
          Error
            (Printf.sprintf "Invalid strain domain '%s'. Allowed values: %s"
               trimmed
               (String.concat ", " all_allowed)))

let validate_taxonomic_group ~allowed_groups group_opt =
  match String_utils.clean_option group_opt with
  | None -> Ok None
  | Some trimmed -> (
      match String_utils.find_canonical_ci trimmed allowed_groups with
      | Some canonical -> Ok (Some canonical)
      | None ->
          Error
            (Printf.sprintf "Invalid taxonomic_group '%s'. Allowed values: %s"
               trimmed
               (String.concat ", " allowed_groups)))

let validate_classification ~allowed_domains ~allowed_groups ~domain
    ~taxonomic_group =
  match validate_domain ~allowed_domains domain with
  | Error _ as err -> err
  | Ok valid_domain -> (
      match validate_taxonomic_group ~allowed_groups taxonomic_group with
      | Error _ as err -> err
      | Ok valid_group -> Ok (valid_domain, valid_group))

let format_label (strain : Types.strain) =
  let full_species = strain.genus ^ " " ^ strain.species in
  let strain_name = strain.strain_name in

  let species_len = String.length full_species in
  let strain_len = String.length strain_name in

  if
    strain_len >= species_len
    && String.sub strain_name 0 species_len = full_species
  then strain_name
  else full_species ^ " " ^ strain_name
