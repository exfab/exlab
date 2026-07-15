let test_simple_mapper_plate_generation_basic () =
  let samples = List.init 70 (fun i -> "sample-" ^ string_of_int (i + 1)) in
  let fixed_map = [] in
  let state =
    { Exlab_core.Well_shuffled.used_edge_samples = []; neighbor_pairs = [] }
  in

  let plate_map, _new_state =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Simple ~rows:8 ~cols:12 ~samples
      ~fixed_map ~state
  in

  (* Basic checks: Should have exactly 70 placements *)
  Alcotest.(check int)
    "Plate should have 70 assigned wells" 70 (List.length plate_map);

  (* Check that all sample strings are unique in this generation *)
  let placed_samples = List.map snd plate_map |> List.sort String.compare in
  let unique_samples = List.sort_uniq String.compare placed_samples in
  Alcotest.(check int)
    "All placed samples are unique" 70
    (List.length unique_samples)

let test_simple_mapper_is_randomized () =
  let samples = List.init 96 (fun i -> "sample-" ^ string_of_int (i + 1)) in
  let fixed_map = [] in
  let state =
    { Exlab_core.Well_shuffled.used_edge_samples = []; neighbor_pairs = [] }
  in

  let plate_map, _ =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Simple ~rows:8 ~cols:12 ~samples
      ~fixed_map ~state
  in

  (* Sort the plate map by coordinates (row, col) to see the actual grid placement order *)
  let sorted_by_coord =
    List.sort
      (fun ((r1, c1), _) ((r2, c2), _) ->
        let r_cmp = compare r1 r2 in
        if r_cmp = 0 then compare c1 c2 else r_cmp)
      plate_map
  in

  (* Extract just the ordered list of samples from the grid placement order *)
  let placed_samples_in_grid_order = List.map snd sorted_by_coord in

  (* It is extremely unlikely (1 in 96!) that a random shuffle perfectly matches sequential input.
     If it matches exactly, it's not randomizing. *)
  let is_identical = placed_samples_in_grid_order = samples in
  Alcotest.(check bool)
    "Output should be randomized and not match sequential input exactly" false
    is_identical

let test_simple_mapper_fixed_map () =
  let samples = [ "S1"; "S2"; "S3" ] in
  let fixed_map = [ ((0, 0), "Control-1"); ((7, 11), "Control-2") ] in
  let state =
    { Exlab_core.Well_shuffled.used_edge_samples = []; neighbor_pairs = [] }
  in

  let plate_map, _ =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Simple ~rows:8 ~cols:12 ~samples
      ~fixed_map ~state
  in

  Alcotest.(check int)
    "Plate should have 5 assigned wells (3 samples + 2 controls)" 5
    (List.length plate_map);

  let get_sample_at coord =
    match List.find_opt (fun (c, _) -> c = coord) plate_map with
    | Some (_, s) -> Some s
    | None -> None
  in

  Alcotest.(check (option string))
    "Control-1 is fixed at 0, 0" (Some "Control-1")
    (get_sample_at (0, 0));
  Alcotest.(check (option string))
    "Control-2 is fixed at 7, 11" (Some "Control-2")
    (get_sample_at (7, 11))

let test_simple_mapper_edge_tracking () =
  (* 96 samples, fully filling a 96 well plate. 
     A 8x12 plate has 36 edge wells (8+8+10+10). *)
  let samples = List.init 96 (fun i -> "S" ^ string_of_int i) in
  let fixed_map = [] in
  let state =
    { Exlab_core.Well_shuffled.used_edge_samples = []; neighbor_pairs = [] }
  in

  let plate_map, new_state =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Simple ~rows:8 ~cols:12 ~samples
      ~fixed_map ~state
  in

  (* We expect exactly 36 samples to be tracked as used on the edge *)
  Alcotest.(check int)
    "Should track 36 edge samples" 36
    (List.length new_state.used_edge_samples);

  (* Check that the tracked samples actually correspond to the ones on the edge *)
  let edge_samples_on_plate =
    List.filter_map
      (fun ((r, c), sample) ->
        if r = 0 || r = 7 || c = 0 || c = 11 then Some sample else None)
      plate_map
  in

  let sorted_tracked = List.sort String.compare new_state.used_edge_samples in
  let sorted_actual = List.sort String.compare edge_samples_on_plate in

  Alcotest.(check (list string))
    "Tracked edge samples match actual edge placements" sorted_actual
    sorted_tracked

let test_simple_mapper_edge_minimization () =
  let samples = List.init 96 (fun i -> "S" ^ string_of_int i) in
  let fixed_map = [] in
  let state =
    { Exlab_core.Well_shuffled.used_edge_samples = []; neighbor_pairs = [] }
  in

  (* Plate 1 *)
  let _, state1 =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Simple ~rows:8 ~cols:12 ~samples
      ~fixed_map ~state
  in

  (* Plate 2 *)
  let _, state2 =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Simple ~rows:8 ~cols:12 ~samples
      ~fixed_map ~state:state1
  in

  (* In a 96 well plate with 96 samples, 36 are on the edge. 
     There are 60 internal samples from Plate 1 that have NEVER been on the edge.
     When generating Plate 2, the algorithm should prioritize those 60 for the 36 edge spots.
     Therefore, NO sample that was on the edge in Plate 1 should be on the edge in Plate 2.
     
     If state2 tracks *all* unique edge samples across both plates, it should now have 36 + 36 = 72 samples. *)
  let unique_edge_samples =
    List.sort_uniq String.compare state2.used_edge_samples
  in
  Alcotest.(check int)
    "Should have 72 unique edge samples after 2 plates" 72
    (List.length unique_edge_samples)

let test_neighbor_aware_tracking () =
  let samples = [ "A"; "B"; "C"; "D" ] in
  let fixed_map = [] in
  let state =
    { Exlab_core.Well_shuffled.used_edge_samples = []; neighbor_pairs = [] }
  in

  let plate_map, new_state =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Neighbor_aware ~rows:2 ~cols:2 ~samples
      ~fixed_map ~state
  in

  (* For a 2x2 grid, there are 4 unique neighbor edges (up/down, left/right). 
     Therefore, the state should contain exactly 4 unique neighbor pairs. *)
  Alcotest.(check int)
    "Should track 4 neighbor pairs in a 2x2 plate" 4
    (List.length new_state.neighbor_pairs);

  (* We should verify the pairs are actually the ones on the plate *)
  let get_sample r c =
    match List.find_opt (fun ((pr, pc), _) -> pr = r && pc = c) plate_map with
    | Some (_, s) -> s
    | None -> failwith "Missing sample"
  in
  let s00 = get_sample 0 0 in
  let s01 = get_sample 0 1 in
  let s10 = get_sample 1 0 in
  let s11 = get_sample 1 1 in

  let make_pair a b = if String.compare a b <= 0 then (a, b) else (b, a) in
  let expected_pairs =
    [
      make_pair s00 s01;
      (* 0,0 right *)
      make_pair s00 s10;
      (* 0,0 down *)
      make_pair s01 s11;
      (* 0,1 down *)
      make_pair s10 s11;
      (* 1,0 right *)
    ]
    |> List.sort compare
  in

  let actual_pairs = List.sort compare new_state.neighbor_pairs in
  Alcotest.(check (list (pair string string)))
    "Neighbor pairs match actual adjacencies" expected_pairs actual_pairs

let test_neighbor_aware_minimization () =
  let samples = List.init 16 (fun i -> "S" ^ string_of_int i) in
  let fixed_map = [] in
  let state =
    { Exlab_core.Well_shuffled.used_edge_samples = []; neighbor_pairs = [] }
  in

  (* Plate 1 *)
  let _, state1 =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Neighbor_aware ~rows:4 ~cols:4 ~samples
      ~fixed_map ~state
  in

  (* Extract just the pairs from plate 1 *)
  let pairs1 = state1.neighbor_pairs in
  Alcotest.(check int)
    "Should track 24 neighbor pairs for 4x4 plate" 24 (List.length pairs1);

  (* Plate 2 *)
  let _, state2 =
    Exlab_core.Well_shuffled.generate_plate
      ~strategy:Exlab_core.Well_shuffled.Neighbor_aware ~rows:4 ~cols:4 ~samples
      ~fixed_map ~state:state1
  in

  (* Check how many pairs from plate 1 appeared again in plate 2.
     With 16 items, there are (16*15)/2 = 120 total possible pairs.
     We use 24 per plate. If it actively minimizes, the intersection should be very small or 0. *)
  let pairs2_only =
    List.filter (fun p -> not (List.mem p pairs1)) state2.neighbor_pairs
  in
  let intersection_size =
    List.length state2.neighbor_pairs
    - List.length pairs1 - List.length pairs2_only
  in

  (* We expect ideally 0 reused pairs, but let's allow up to 2 just in case the greedy algorithm gets stuck,
     though for a 4x4 it usually finds a perfect solution. Right now, without avoidance logic, 
     a random shuffle will have an expected intersection of (24/120) * 24 = ~4.8. 
     We can assert it's strictly less than 2 if neighbor aware is working perfectly. *)
  Alcotest.(check bool)
    "Should avoid reusing neighbor pairs" true (intersection_size < 2)

let suite =
  [
    ( "Well Shuffled",
      [
        Alcotest.test_case "Simple mapper basic generation" `Quick
          test_simple_mapper_plate_generation_basic;
        Alcotest.test_case "Simple mapper randomizes output" `Quick
          test_simple_mapper_is_randomized;
        Alcotest.test_case "Simple mapper respects fixed map" `Quick
          test_simple_mapper_fixed_map;
        Alcotest.test_case "Simple mapper tracks edge usage" `Quick
          test_simple_mapper_edge_tracking;
        Alcotest.test_case "Simple mapper minimizes edge reuse" `Quick
          test_simple_mapper_edge_minimization;
        Alcotest.test_case "Neighbor aware tracks neighbors" `Quick
          test_neighbor_aware_tracking;
        Alcotest.test_case "Neighbor aware minimizes neighbor reuse" `Quick
          test_neighbor_aware_minimization;
      ] );
  ]
