open Lwt_result.Syntax
module StringSet = Set.Make (String)

let generate_experimental_run ~project_id
    (req : Api_types.Plate.generate_run_request) =
  (* 1. Validate Source Samples *)
  let* source_samples =
    Exlab_storage.Sample.get_many_by_short_ids req.source_sample_short_ids
  in
  let* () =
    if List.length source_samples <> List.length req.source_sample_short_ids
    then
      Lwt.return
        (Error (`Bad_Request "One or more source sample short IDs are invalid"))
    else Lwt.return (Ok ())
  in

  (* 2. Build Experimental Samples Payload *)
  let create_sample_payloads =
    List.concat_map
      (fun source_sample ->
        List.init req.replicates (fun _ ->
            {
              Exlab_storage.Sample.sample_type =
                source_sample.Exlab_core.Types.sample_type;
              category = Exlab_core.Types.Experimental;
              parent_sample_id = Some source_sample.id;
              strain_id = source_sample.strain_id;
              community_id = source_sample.community_id;
              result_definition_ids = [];
            }))
      source_samples
  in

  (* 3. Determine Usable Wells & Plates Count *)
  let* plate_format =
    match Exlab_core.Plate.validate_format req.plate_format with
    | Ok f -> Lwt_result.return f
    | Error msg -> Lwt_result.fail (`Bad_Request msg)
  in

  let all_wells = Exlab_core.Plate.generate_wells plate_format in
  let reserved_set =
    List.fold_left
      (fun acc w ->
        match Exlab_core.Plate.normalize_coordinate plate_format w with
        | Ok cw -> StringSet.add cw acc
        | _ -> acc)
      StringSet.empty req.reserved_wells
  in
  let usable_wells =
    List.filter (fun w -> not (StringSet.mem w reserved_set)) all_wells
  in
  let num_usable_wells = List.length usable_wells in

  let* () =
    if num_usable_wells = 0 then
      Lwt_result.fail
        (`Bad_Request "No usable wells left after reserving wells")
    else Lwt_result.return ()
  in

  let total_samples = List.length create_sample_payloads in
  let total_plates =
    (total_samples + num_usable_wells - 1) / num_usable_wells
  in
  let total_plates = max 1 total_plates in

  (* 4. Create Samples *)
  let* created_samples =
    Exlab_storage.Sample.create_many ~project_id create_sample_payloads
  in

  (* 5. Create Plates *)
  let rec create_plates acc count =
    if count > total_plates then Lwt_result.return (List.rev acc)
    else
      let name = Printf.sprintf "%s - %d" req.plate_name_prefix count in
      let* plate = Exlab_storage.Plate.add ~name ~project_id ~plate_format () in
      create_plates (plate :: acc) (count + 1)
  in
  let* new_plates = create_plates [] 1 in

  (* 6. Round-Robin Distribution Algorithm *)
  let plate_states =
    Array.of_list
      (List.map
         (fun p -> (p.Exlab_core.Types.id, ref usable_wells, ref []))
         new_plates)
  in

  let rec assign_sample sample plate_idx attempts =
    if attempts >= total_plates then
      Error "Not enough usable wells to assign all samples"
    else
      let plate_id, remaining_wells, layout_acc = plate_states.(plate_idx) in
      match !remaining_wells with
      | [] ->
          assign_sample sample ((plate_idx + 1) mod total_plates) (attempts + 1)
      | well :: rest ->
          remaining_wells := rest;
          layout_acc := (well, sample.Exlab_core.Types.id) :: !layout_acc;
          Ok ((plate_idx + 1) mod total_plates)
  in

  let rec distribute samples plate_idx =
    match samples with
    | [] -> Ok ()
    | s :: ss -> (
        match assign_sample s plate_idx 0 with
        | Ok next_idx -> distribute ss next_idx
        | Error e -> Error e)
  in

  let* () =
    match distribute created_samples 0 with
    | Ok () -> Lwt_result.return ()
    | Error e -> Lwt_result.fail (`Bad_Request e)
  in

  (* 7. Update Well Layouts *)
  let* () =
    Lwt_list.iter_s
      (fun (plate_id, _, layout_acc) ->
        match%lwt Exlab_storage.Well.update_layout ~plate_id !layout_acc with
        | Ok () -> Lwt.return_unit
        | Error e -> Lwt.fail_with (Caqti_error.show e))
      (Array.to_list plate_states)
    |> Lwt_result.ok
  in

  Lwt_result.return new_plates
