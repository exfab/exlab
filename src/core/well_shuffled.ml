type strategy = Simple | Neighbor_aware
type coordinate = int * int

type state = {
  used_edge_samples : string list;
  neighbor_pairs : (string * string) list;
}

let shuffle_list lst =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let is_edge (r, c) rows cols = r = 0 || r = rows - 1 || c = 0 || c = cols - 1

let extract_neighbors plate_map =
  let get_sample r c =
    match List.find_opt (fun ((pr, pc), _) -> pr = r && pc = c) plate_map with
    | Some (_, s) -> Some s
    | None -> None
  in
  let make_pair a b = if String.compare a b <= 0 then (a, b) else (b, a) in
  List.fold_left
    (fun acc ((r, c), sample) ->
      let right_neighbor = get_sample r (c + 1) in
      let down_neighbor = get_sample (r + 1) c in
      let acc1 =
        match right_neighbor with
        | Some s -> make_pair sample s :: acc
        | None -> acc
      in
      let acc2 =
        match down_neighbor with
        | Some s -> make_pair sample s :: acc1
        | None -> acc1
      in
      acc2)
    [] plate_map

let shuffle_list lst =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let make_pair a b = if String.compare a b <= 0 then (a, b) else (b, a)

let has_bad_neighbor (r, c) candidate plate_map bad_pairs =
  let get_sample pr pc =
    match List.find_opt (fun ((r', c'), _) -> pr = r' && pc = c') plate_map with
    | Some (_, s) -> Some s
    | None -> None
  in
  let neighbors =
    List.filter_map
      (fun (dr, dc) -> get_sample (r + dr) (c + dc))
      [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
  in
  List.exists (fun n -> List.mem (make_pair candidate n) bad_pairs) neighbors

let generate_plate ~strategy ~rows ~cols ~samples ~fixed_map ~state =
  let all_indices =
    List.init rows (fun r -> List.init cols (fun c -> (r, c))) |> List.flatten
  in

  let available_indices =
    List.filter
      (fun coord -> not (List.exists (fun (c, _) -> c = coord) fixed_map))
      all_indices
  in

  let edge_indices, interior_indices =
    List.partition (fun coord -> is_edge coord rows cols) available_indices
  in

  let randomized_edge_indices = shuffle_list edge_indices in
  let randomized_interior_indices = shuffle_list interior_indices in
  let randomized_indices =
    randomized_edge_indices @ randomized_interior_indices
  in

  let available_for_edge, used_on_edge =
    List.partition (fun s -> not (List.mem s state.used_edge_samples)) samples
  in

  let randomized_available_for_edge = shuffle_list available_for_edge in
  let randomized_used_on_edge = shuffle_list used_on_edge in
  let prioritized_samples =
    randomized_available_for_edge @ randomized_used_on_edge
  in

  let rec assign remaining_indices remaining_samples acc_map =
    match remaining_indices with
    | [] -> List.rev acc_map
    | idx :: idx_rest -> (
        match strategy with
        | Simple -> (
            (* Blindly pop the next sample *)
            match remaining_samples with
            | [] -> List.rev acc_map
            | s :: s_rest -> assign idx_rest s_rest ((idx, s) :: acc_map))
        | Neighbor_aware -> (
            (* Find the first candidate that does not violate neighbor constraints *)
            let full_map = fixed_map @ acc_map in
            let rec find_good_candidate candidates checked =
              match candidates with
              | [] -> None
              | c :: rest ->
                  if has_bad_neighbor idx c full_map state.neighbor_pairs then
                    find_good_candidate rest (c :: checked)
                  else Some (c, List.rev checked @ rest)
            in
            match find_good_candidate remaining_samples [] with
            | Some (best_candidate, remaining_after_pick) ->
                assign idx_rest remaining_after_pick
                  ((idx, best_candidate) :: acc_map)
            | None -> (
                (* Fallback: assign the first remaining sample to break a deadlock. *)
                match remaining_samples with
                | [] -> List.rev acc_map
                | s :: s_rest -> assign idx_rest s_rest ((idx, s) :: acc_map))))
  in

  let variable_map = assign randomized_indices prioritized_samples [] in
  let plate_map = fixed_map @ variable_map in

  let newly_used_edge_samples =
    List.filter_map
      (fun (coord, sample) ->
        if is_edge coord rows cols then Some sample else None)
      variable_map
  in
  let combined_edge_samples =
    List.sort_uniq String.compare
      (state.used_edge_samples @ newly_used_edge_samples)
  in

  let new_neighbor_pairs =
    match strategy with
    | Neighbor_aware ->
        let pairs = extract_neighbors plate_map in
        List.sort_uniq compare (state.neighbor_pairs @ pairs)
    | Simple -> state.neighbor_pairs
  in

  ( plate_map,
    {
      used_edge_samples = combined_edge_samples;
      neighbor_pairs = new_neighbor_pairs;
    } )

let generate_multiple_plates ~strategy:_ ~rows:_ ~cols:_ ~samples_per_plate:_
    ~fixed_map:_ =
  []
