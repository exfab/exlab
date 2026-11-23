(** Implementation of strain business logic. *)

let normalize_name input =
  let trimmed = String.trim input in
  if trimmed = "" then ""
  else
    let first = String.sub trimmed 0 1 |> String.uppercase_ascii in
    let rest = String.sub trimmed 1 (String.length trimmed - 1) in
    first ^ rest

let validate_creation ~genus ~species ~strain_name ~genotype ~parent_strain_id =
  let clean_genus = normalize_name genus in
  let clean_species = String.trim species in
  let clean_strain = String.trim strain_name in

  if clean_genus = "" then Error "Genus cannot be empty"
  else if clean_species = "" then Error "Species cannot be empty"
  else if clean_strain = "" then Error "Strain Name cannot be empty"
  else
    (* Convert empty or whitespace-only genotypes to None *)
    let clean_genotype =
      match genotype with
      | Some g ->
          let t = String.trim g in
          if t = "" then None else Some t
      | None -> None
    in

    (* Basic sanity check on Parent ID *)
    let valid_parent =
      match parent_strain_id with
      | Some id when id <= 0 -> Error "Parent Strain ID must be positive"
      | _ -> Ok parent_strain_id
    in

    match valid_parent with
    | Error e -> Error e
    | Ok pid ->
        Ok (clean_genus, clean_species, clean_strain, clean_genotype, pid)

let default_taxonomic_groups = [ "Yeast"; "Filamentous Fungi"; "Macrofungi" ]

let validate_domain = function
  | None -> Ok Types.Unassigned
  | Some s -> Types.strain_domain_of_string s

let validate_taxonomic_group ~allowed_groups = function
  | None -> Ok None
  | Some raw -> (
      let trimmed = String.trim raw in
      if trimmed = "" then Ok None
      else
        let target = String.lowercase_ascii trimmed in
        match
          List.find_opt
            (fun g -> String.lowercase_ascii (String.trim g) = target)
            allowed_groups
        with
        | Some canonical -> Ok (Some (String.trim canonical))
        | None ->
            Error
              (Printf.sprintf "Invalid taxonomic_group '%s'. Allowed values: %s"
                 trimmed
                 (String.concat ", " allowed_groups)))

let validate_classification ~allowed_groups ~domain ~taxonomic_group =
  match validate_domain domain with
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
