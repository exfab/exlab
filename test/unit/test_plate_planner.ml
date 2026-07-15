let test_round_robin_success () =
  let usable_wells = [ "A1"; "A2"; "A3" ] in
  let items = [ 1; 2; 3; 4; 5 ] in
  let result =
    Exlab_core.Plate_planner.distribute_round_robin ~total_plates:2
      ~usable_wells ~items
  in
  match result with
  | Ok layouts ->
      Alcotest.(check int) "Should have 2 layouts" 2 (List.length layouts);
      let layout1 = List.nth layouts 0 in
      let layout2 = List.nth layouts 1 in
      Alcotest.(check int) "Plate 1 should have 3 items" 3 (List.length layout1);
      Alcotest.(check int) "Plate 2 should have 2 items" 2 (List.length layout2);

      (* Check exact assignments. Round robin should be:
         P1: 1 (A1), 3 (A2), 5 (A3)
         P2: 2 (A1), 4 (A2)
         Because we distribute item by item, picking next plate. *)

      (* The items in layouts might be in reverse order depending on how it's accumulated, 
         so we'll sort them by well coordinate to verify *)
      let sort_layout =
        List.sort (fun (w1, _) (w2, _) -> String.compare w1 w2)
      in
      let sorted1 = sort_layout layout1 in
      let sorted2 = sort_layout layout2 in

      Alcotest.(check (list (pair string int)))
        "Plate 1 assignments"
        [ ("A1", 1); ("A2", 3); ("A3", 5) ]
        sorted1;

      Alcotest.(check (list (pair string int)))
        "Plate 2 assignments"
        [ ("A1", 2); ("A2", 4) ]
        sorted2
  | Error e -> Alcotest.failf "Unexpected error: %s" e

let test_round_robin_insufficient_wells () =
  let usable_wells = [ "A1" ] in
  let items = [ 1; 2; 3 ] in
  (* 2 plates * 1 well = 2 wells, but we have 3 items. Should fail. *)
  let result =
    Exlab_core.Plate_planner.distribute_round_robin ~total_plates:2
      ~usable_wells ~items
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected an error but got Ok"
  | Error e ->
      Alcotest.(check string)
        "Correct error message" "Not enough usable wells to assign all items" e

let test_generate_autofill_layout_sequential () =
  let wells = [ "A1"; "A2"; "A3" ] in
  let sample_ids = [ "S1"; "S2" ] in
  let result =
    Exlab_core.Plate_planner.generate_autofill_layout
      ~strategy:(Some "sequential") ~wells ~sample_ids
  in
  match result with
  | Ok layout ->
      Alcotest.(check int) "Should have 2 layouts" 2 (List.length layout);
      Alcotest.(check (list (pair string string)))
        "Matches expected sequential layout"
        [ ("A1", "S1"); ("A2", "S2") ]
        layout
  | Error e -> Alcotest.failf "Unexpected error: %s" e

let test_generate_autofill_layout_unknown_strategy () =
  let result =
    Exlab_core.Plate_planner.generate_autofill_layout ~strategy:(Some "unknown")
      ~wells:[] ~sample_ids:[]
  in
  match result with
  | Ok _ -> Alcotest.fail "Expected error for unknown strategy"
  | Error e ->
      Alcotest.(check string)
        "Correct error message" "Unknown auto-fill strategy: unknown" e

let test_generate_shuffled_layouts () =
  (* Setup a simple 24 well plate (4x6) for testing hydration *)
  let format = Exlab_core.Types.Well_24 in

  (* We want 2 replicates of Source-A, 1 replicate of Source-B *)
  let items_plate1 =
    [
      ("Source-A", "Sample-A-01");
      ("Source-A", "Sample-A-02");
      ("Source-B", "Sample-B-01");
    ]
  in

  let fixed_map1 = [ ("A1", ("Control-X", "Control-X-01")) ] in

  let result =
    Exlab_core.Plate_planner.generate_shuffled_layouts
      ~strategy:Exlab_core.Well_shuffled.Simple ~format
      ~reserved_wells:[]
      ~fixed_maps:[ fixed_map1 ] ~items_per_plate:[ items_plate1 ]
      ~num_blanks:0
  in

  match result with
  | Error e -> Alcotest.failf "Unexpected error: %s" e
  | Ok layouts ->
      Alcotest.(check int) "Should have 1 layout" 1 (List.length layouts);
      let layout = List.hd layouts in
      Alcotest.(check int)
        "Layout should have 4 assignments (3 samples + 1 control)" 4
        (List.length layout);

      (* Verify Control is at A1 *)
      let control_assignment =
        List.find_opt (fun (coord, _) -> coord = "A1") layout
      in
      Alcotest.(check (option string))
        "Control-X-01 is at A1" (Some "Control-X-01")
        (Option.map snd control_assignment);

      (* Verify the physical items A-01, A-02, B-01 all exist in the layout, meaning hydration worked perfectly *)
      let items_in_layout = List.map snd layout |> List.sort String.compare in
      let expected_items =
        [ "Control-X-01"; "Sample-A-01"; "Sample-A-02"; "Sample-B-01" ]
        |> List.sort String.compare
      in
      Alcotest.(check (list string))
        "All specific aliquots are present in layout" expected_items
        items_in_layout

let suite =
  [
    ( "Plate Planner",
      [
        Alcotest.test_case "Round robin distributes successfully" `Quick
          test_round_robin_success;
        Alcotest.test_case "Round robin fails gracefully when wells run out"
          `Quick test_round_robin_insufficient_wells;
        Alcotest.test_case "Autofill layout sequential" `Quick
          test_generate_autofill_layout_sequential;
        Alcotest.test_case "Autofill layout unknown strategy" `Quick
          test_generate_autofill_layout_unknown_strategy;
        Alcotest.test_case "Shuffled layout with hydration" `Quick
          test_generate_shuffled_layouts;
      ] );
  ]
