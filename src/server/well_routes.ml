(** API routes for assigning and clearing samples on individual plate wells. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

let patch_well_handler request =
  let coordinate = Dream.param request "coordinate" in
  let result =
    let* plate_id = Api_utils.get_int_param request "plate_id" in
    let* req =
      Api_utils.parse_body_json Api_types.Well.update_of_yojson request
    in
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
    Lwt.return (Ok { Api_types.Common.status = "updated" })
  in
  Api_utils.handle_response ~status:`OK
    ~serializer:Api_types.Common.yojson_of_status_response result

let routes =
  [
    Dream.patch "/api/v1/plates/:plate_id/wells/:coordinate" patch_well_handler;
  ]
