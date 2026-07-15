(*
 * Main entry point for the Exlab test suite.
 *
 * This file collects all the test suites from the different test modules
 * and runs them using Alcotest.
 *)

let () =
  Alcotest.run "Exlab_Unit_Test_Suite"
    (Test_project.suite @ Test_sample.suite @ Test_product.suite
   @ Test_plate.suite @ Test_export.suite @ Test_results.suite
   @ Test_matrix.suite @ Test_result_category.suite @ Test_strain.suite
   @ Test_community.suite @ Test_csv_utils.suite @ Test_decoders.suite
   @ Test_user.suite @ Test_setting.suite @ Test_plate_planner.suite
   @ Test_string_utils.suite @ Test_well_shuffled.suite)
