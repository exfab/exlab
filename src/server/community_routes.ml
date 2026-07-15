module Storage = Exlab_storage
module Core = Exlab_core
module Api = Api_types
open Lwt_result.Syntax

let get_all_communities_handler request =
  let result =
    let* communities = Storage.Community.get_all () in
    let response =
      { Api.Community.data = communities; count = List.length communities }
    in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response ~serializer:Api.Community.yojson_of_list_response
    result

let get_community_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* community = Api_utils.find_community_by_identifier identifier in
    let* members = Storage.Community_member.get_by_community_id community.id in
    let* source_samples_with_projects =
      Storage.Sample.get_source_with_project_by_community_id community.id
    in
    let source_samples =
      List.map
        (fun (sample, project) -> { Api.Community.sample; project })
        source_samples_with_projects
    in
    let response = { Api.Community.community; members; source_samples } in
    Lwt.return (Ok response)
  in
  Api_utils.handle_response ~serializer:Api.Community.yojson_of_detailed result

let create_community_handler request =
  let result =
    let* req =
      Api_utils.parse_body_json Api.Community.create_of_yojson request
    in
    let* template_json_opt =
      Storage.Setting.get_by_key "community_metadata_template"
    in
    let template =
      match template_json_opt with
      | Some s -> (
          match s.value with
          | `List l ->
              List.filter_map
                (fun item ->
                  try Some (Core.Types.metadata_field_def_of_yojson item)
                  with _ -> None)
                l
          | _ -> [])
      | None -> []
    in
    let* () =
      Lwt.return
        (Core.Project.validate_metadata req.metadata template
        |> Result.map_error (fun e -> `Bad_Request e))
    in
    let* new_community =
      Storage.Community.add ~name:req.name ~notes:req.notes
        ~metadata:req.metadata
    in
    Lwt.return (Ok new_community)
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_community result

let update_community_handler request =
  let%lwt body = Dream.body request in
  let result =
    try
      let identifier = Dream.param request "identifier" in
      let* community = Api_utils.find_community_by_identifier identifier in
      let json = Yojson.Safe.from_string body in
      let req = Api.Community.update_of_yojson json in
      let* template_json_opt =
        Storage.Setting.get_by_key "community_metadata_template"
      in
      let template =
        match template_json_opt with
        | Some s -> (
            match s.value with
            | `List l ->
                List.filter_map
                  (fun item ->
                    try Some (Core.Types.metadata_field_def_of_yojson item)
                    with _ -> None)
                  l
            | _ -> [])
        | None -> []
      in
      let* () =
        Lwt.return
          (Core.Project.validate_metadata req.metadata template
          |> Result.map_error (fun e -> `Bad_Request e))
      in
      Storage.Community.update ~id:community.id ~name:req.name ~notes:req.notes
        ~metadata:req.metadata
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid payload: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON parsing error: " ^ msg)))
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_community result

let get_community_members_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* community = Api_utils.find_community_by_identifier identifier in
    let* members = Storage.Community_member.get_by_community_id community.id in
    Lwt.return (Ok members)
  in
  Api_utils.handle_response
    ~serializer:(fun members ->
      `Assoc
        [
          ( "data",
            `List (List.map Core.Types.yojson_of_community_member members) );
          ("count", `Int (List.length members));
        ])
    result

let add_community_member_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* community = Api_utils.find_community_by_identifier identifier in
    let* req =
      Api_utils.parse_body_json Api.CommunityMember.create_of_yojson request
    in
    let* new_member =
      Storage.Community_member.add ~community_id:community.id
        ~strain_id:req.strain_id ~label:req.label ~taxon:req.taxon
    in
    Lwt.return (Ok new_member)
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:Core.Types.yojson_of_community_member result

let update_community_member_handler request =
  let%lwt body = Dream.body request in
  let result =
    try
      let identifier = Dream.param request "identifier" in
      let* community = Api_utils.find_community_by_identifier identifier in
      let member_id = int_of_string (Dream.param request "member_id") in
      let json = Yojson.Safe.from_string body in
      let req = Api.CommunityMember.update_of_yojson json in
      Storage.Community_member.update ~id:member_id ~strain_id:req.strain_id
        ~label:req.label ~taxon:req.taxon
    with
    | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, _) ->
        let msg = Printexc.to_string exn in
        Lwt.return (Error (`Bad_Request ("Invalid payload: " ^ msg)))
    | Yojson.Json_error msg ->
        Lwt.return (Error (`Bad_Request ("JSON parsing error: " ^ msg)))
    | Failure msg -> Lwt.return (Error (`Bad_Request msg))
  in
  Api_utils.handle_response ~serializer:Core.Types.yojson_of_community_member
    result

let remove_community_member_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* _community = Api_utils.find_community_by_identifier identifier in
    let member_id = int_of_string (Dream.param request "member_id") in
    let* () = Storage.Community_member.remove member_id in
    Lwt.return (Ok ())
  in
  Api_utils.handle_unit_result result

let add_community_members_bulk_handler request =
  let identifier = Dream.param request "identifier" in
  let result =
    let* community = Api_utils.find_community_by_identifier identifier in
    let* req =
      Api_utils.parse_body_json
        Api.CommunityMember.bulk_create_request_of_yojson request
    in
    let rec process_all (acc : Core.Types.community_member list) = function
      | [] -> Lwt.return (Ok (List.rev acc))
      | (item : Api.CommunityMember.create) :: tail -> (
          match%lwt
            Storage.Community_member.add ~community_id:community.id
              ~strain_id:item.strain_id ~label:item.label ~taxon:item.taxon
          with
          | Ok new_member -> process_all (new_member :: acc) tail
          | Error e -> Lwt.return (Error e))
    in
    process_all [] req.members
  in
  Api_utils.handle_response ~status:`Created
    ~serializer:(fun list ->
      `Assoc
        [
          ( "members",
            `List (List.map Core.Types.yojson_of_community_member list) );
        ])
    result

let routes =
  [
    Dream.get "/api/v1/communities" get_all_communities_handler;
    Dream.get "/api/v1/communities/:identifier" get_community_handler;
    Dream.post "/api/v1/communities" create_community_handler;
    Dream.put "/api/v1/communities/:identifier" update_community_handler;
    Dream.get "/api/v1/communities/:identifier/members"
      get_community_members_handler;
    Dream.post "/api/v1/communities/:identifier/members"
      add_community_member_handler;
    Dream.post "/api/v1/communities/:identifier/members/bulk"
      add_community_members_bulk_handler;
    Dream.put "/api/v1/communities/:identifier/members/:member_id"
      update_community_member_handler;
    Dream.delete "/api/v1/communities/:identifier/members/:member_id"
      remove_community_member_handler;
  ]
