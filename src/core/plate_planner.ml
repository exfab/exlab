let distribute_round_robin ~total_plates ~usable_wells ~items =
  let plate_states =
    Array.init total_plates (fun _ -> (ref usable_wells, ref []))
  in

  let rec assign_item item plate_idx attempts =
    if attempts >= total_plates then
      Error "Not enough usable wells to assign all items"
    else
      let remaining_wells, layout_acc = plate_states.(plate_idx) in
      match !remaining_wells with
      | [] -> assign_item item ((plate_idx + 1) mod total_plates) (attempts + 1)
      | well :: rest ->
          remaining_wells := rest;
          layout_acc := (well, item) :: !layout_acc;
          Ok ((plate_idx + 1) mod total_plates)
  in

  let rec distribute items plate_idx =
    match items with
    | [] -> Ok ()
    | it :: tail -> (
        match assign_item it plate_idx 0 with
        | Ok next_idx -> distribute tail next_idx
        | Error e -> Error e)
  in

  match distribute items 0 with
  | Error e -> Error e
  | Ok () ->
      Ok (Array.to_list plate_states |> List.map (fun (_, acc) -> List.rev !acc))

let generate_autofill_layout ~strategy ~wells ~sample_ids =
  let strat = Option.value ~default:"sequential" strategy in
  match strat with
  | "sequential" ->
      let rec zip w_list s_list acc =
        match (w_list, s_list) with
        | [], _ | _, [] -> Ok (List.rev acc)
        | w :: ws, s :: ss -> zip ws ss ((w, s) :: acc)
      in
      zip wells sample_ids []
  | _ -> Error ("Unknown auto-fill strategy: " ^ strat)

let generate_shuffled_layouts ~strategy ~format ~reserved_wells ~fixed_maps ~items_per_plate ~num_blanks =
  let rows, cols = Plate.get_plate_dimensions format in
  let initial_state =
    { Well_shuffled.used_edge_samples = []; neighbor_pairs = [] }
  in
  
  let reserved_rc_result =
    List.fold_left
      (fun acc coord ->
        match acc with
        | Error e -> Error e
        | Ok map_acc -> (
            match Plate.parse_coordinate coord with
            | Ok rc -> Ok (rc :: map_acc)
            | Error e -> Error e))
      (Ok []) reserved_wells
  in

  let rec process_plates reserved_rc remaining_items remaining_fixed state acc =
    match (remaining_items, remaining_fixed) with
    | [], [] -> Ok (List.rev acc)
    | [], _ | _, [] -> Error "Mismatched number of fixed maps and plates"
    | items :: rest_items, fixed_map_alnum :: rest_fixed -> (
        let blanks = List.init num_blanks (fun _ -> "__BLANK__") in
        let samples = List.map fst items @ blanks in

        let fixed_map_rc_result =
          List.fold_left
            (fun acc (coord, (source_id, item)) ->
              match acc with
              | Error e -> Error e
              | Ok map_acc -> (
                  match Plate.parse_coordinate coord with
                  | Ok rc -> Ok ((rc, source_id, item) :: map_acc)
                  | Error e -> Error e))
            (Ok []) fixed_map_alnum
        in

        match fixed_map_rc_result with
        | Error e -> Error e
        | Ok fixed_map_rc -> (
            let fixed_map_shuffler =
              List.map (fun (rc, src, _) -> (rc, src)) fixed_map_rc
            in
            (* Add reserved wells to the shuffler's fixed map so it avoids them *)
            let fixed_map_with_reserved =
              List.fold_left (fun acc rc -> (rc, "__RESERVED__") :: acc) fixed_map_shuffler reserved_rc
            in
            
            let raw_plate_map, new_state =
              Well_shuffled.generate_plate ~strategy ~rows ~cols ~samples
                ~fixed_map:fixed_map_with_reserved ~state
            in

            (* Hydrate raw plate map into standard alphanumeric coordinates and items *)
            let variable_pool = ref items in

            let hydrated_map_result =
              List.fold_left
                (fun acc (rc, source_id) ->
                  match acc with
                  | Error e -> Error e
                  | Ok layout_acc -> (
                      if source_id = "__RESERVED__" then Ok layout_acc
                      else if source_id = "__BLANK__" then Ok layout_acc
                      else
                        let r, c = rc in
                        let coord_str =
                          Printf.sprintf "%c%d" (char_of_int (r + 65)) (c + 1)
                        in

                        (* Is it a fixed item? *)
                        match
                          List.find_opt (fun (c, _, _) -> c = rc) fixed_map_rc
                        with
                        | Some (_, _, item) -> Ok ((coord_str, item) :: layout_acc)
                        | None -> (
                            (* It must be a variable item. Pop from variable_pool. *)
                            let rec pop_item src pool acc_pool =
                              match pool with
                              | [] -> None
                              | (s, item) :: rest when s = src ->
                                  variable_pool := List.rev acc_pool @ rest;
                                  Some item
                              | x :: rest -> pop_item src rest (x :: acc_pool)
                            in
                            match pop_item source_id !variable_pool [] with
                            | Some item -> Ok ((coord_str, item) :: layout_acc)
                            | None ->
                                Error
                                  (Printf.sprintf
                                     "Failed to hydrate: missing item for source \
                                      %s"
                                     source_id))))
                (Ok []) raw_plate_map
            in

            match hydrated_map_result with
            | Error e -> Error e
            | Ok hydrated_map ->
                process_plates reserved_rc rest_items rest_fixed new_state
                  (List.rev hydrated_map :: acc)))
  in
  match reserved_rc_result with
  | Error e -> Error e
  | Ok reserved_rc -> process_plates reserved_rc items_per_plate fixed_maps initial_state []
