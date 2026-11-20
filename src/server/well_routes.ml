(** API routes for assigning and clearing samples on individual plate wells. *)

module Storage = Exlab_storage
module Core = Exlab_core
open Lwt_result.Syntax

let patch_well_handler request =
  let raw_coordinate = Dream.param request "coordinate" in
  let result =
    Auth.with_plate_access request "identifier"
      (fun ~user_id ~user_role ~plate ->
        let* req =
          Api_utils.parse_body_json Api_types.Well.update_of_yojson request
        in
        let* coordinate =
          match
            Core.Plate.normalize_coordinate plate.plate_format raw_coordinate
          with
          | Ok coord -> Lwt_result.return coord
          | Error msg -> Lwt_result.fail (`Bad_Request msg)
        in
        let* () =
          match req.sample_id with
          | None -> Lwt_result.return ()
          | Some new_sample_id -> (
              let* sample_opt = Storage.Sample.get_by_id new_sample_id in
              match sample_opt with
              | None ->
                  Lwt_result.fail
                    (`Not_Found
                       (Printf.sprintf "Sample not found with ID %d"
                          new_sample_id))
              | Some sample -> (
                  let* () =
                    Auth.check_project_access ~user_id ~user_role
                      ~project_id:sample.project_id
                  in
                  if sample.project_id <> plate.project_id then
                    Lwt_result.fail
                      (`Bad_Request
                         "Sample does not belong to this plate's project")
                  else
                    let* existing_categories =
                      Storage.Well.get_plate_categories plate.id
                    in
                    match
                      Core.Plate.validate_sample_mixture ~existing_categories
                        ~new_category:sample.category
                    with
                    | Ok () -> Lwt_result.return ()
                    | Error msg -> Lwt_result.fail (`Bad_Request msg)))
        in
        let* () =
          Storage.Well.assign_by_coordinate ~plate_id:plate.id ~coordinate
            ~sample_id:req.sample_id
        in
        Lwt_result.return { Api_types.Common.status = "updated" })
  in
  Api_utils.handle_response ~status:`OK
    ~serializer:Api_types.Common.yojson_of_status_response result

let routes =
  [
    Dream.patch "/api/v1/plates/:identifier/wells/:coordinate"
      patch_well_handler;
  ]
