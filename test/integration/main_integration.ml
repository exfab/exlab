(*
 * Main entry point for the Exlab Integration (API) test suite.
 *
 * This file collects all the ASYNC test suites from the different test_api_* modules
 * and runs them using Alcotest_lwt.
 *)

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Exlab_Integration_Test_Suite"
       (Test_api_result_definitions.suite @ Test_api_project.suite
      @ Test_api_product.suite @ Test_api_plate.suite @ Test_api_sample.suite
      @ Test_api_result_value.suite @ Test_api_strain.suite
      @ Test_api_external_db.suite @ Test_api_strain_link.suite
      @ Test_api_user.suite @ Test_api_auth.suite @ Test_api_bulk_results.suite
      @ Test_api_bulk_plate.suite @ Test_api_plate_autofill.suite
      @ Test_api_multi_plate_planner.suite @ Test_api_well.suite
      @ Test_api_result_update.suite @ Test_api_dashboard.suite
      @ Test_api_setting.suite @ Test_api_transfer_map.suite
      @ Test_api_community.suite (* @ Test_api_bulk_sample.suite *))
