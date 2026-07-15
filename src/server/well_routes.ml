(** * This module defines the API routes for managing Wells on a Plate. * It
    provides an endpoint for assigning a sample to a specific well, identified *
    by its coordinate on a given plate. * * This module handles the HTTP layer,
    translating requests and responses * between the client and the underlying
    storage and core logic layers. It * uses helper functions from the [Utils]
    module for common tasks like * parameter parsing and response serialization.
    * * Key functionalities include: * - Assigning a sample to a well. * -
    Validating that samples of different categories are not mixed on the same
    plate. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

(** Handles `PATCH /api/v1/plates/:plate_id/wells/:coordinate`.

    This endpoint assigns a sample to a specific well on a plate. The well is
    identified by its parent [plate_id] and its [coordinate] (e.g., "A1",
    "H12"). The request body must contain the [sample_id] to be assigned.

    Before assigning the sample, it performs a critical validation step to
    prevent mixing of sample categories (e.g., 'Source' and 'Experimental') on
    the same plate. If the new sample's category is incompatible with the
    categories of samples already on the plate, it returns a [400 Bad Request]
    error.

    @param plate_id The integer ID of the plate.
    @param coordinate
      The coordinate of the well (e.g., "A1").
      - **Body**: (json) A JSON object containing the `sample_id` to assign.
    @return
      A JSON object with a status message on success (e.g.,
      ["status": "updated"]) or a JSON error object on failure. *)
let patch_well_handler request =
  let coordinate = Dream.param request "coordinate" in

  let result =
    try
      let* plate_id = Api_utils.get_int_param request "plate_id" in
      let%lwt body = Dream.body request in
      let json = Yojson.Safe.from_string body in

      let req = Api_types.Well.update_of_yojson json in

      let* () =
        match req.sample_id with
        | None -> Lwt.return (Ok ())
        | Some new_sample_id -> (
            let* sample =
              Api_utils.find_sample_by_identifier (string_of_int new_sample_id)
            in

            let* existing_categories =
              Storage.Well.get_plate_categories plate_id
            in

            match
              Core.Plate.validate_sample_mixture ~existing_categories
                ~new_category:sample.category
            with
            | Ok () -> Lwt.return (Ok ())
            | Error msg -> Lwt.return (Error (`Bad_Request msg)))
      in

      let* () =
        Storage.Well.assign_by_coordinate ~plate_id ~coordinate
          ~sample_id:req.sample_id
      in
      let response : Api_types.Common.status_response =
        { Api_types.Common.status = "updated" }
      in
      Lwt.return (Ok response)
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid JSON structure: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON parsing error: " ^ msg)))
  in

  Api_utils.handle_response ~status:`OK
    ~serializer:Api_types.Common.yojson_of_status_response result

let routes =
  [
    Dream.patch "/api/v1/plates/:plate_id/wells/:coordinate" patch_well_handler;
  ]
