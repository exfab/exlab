module Storage = Exlab_storage
module Core = Exlab_core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type match_item = {
  resource_type : string;
  id : int;
  short_id : string option;
  name : string;
  url : string;
}
[@@deriving yojson]

type lookup_response = { query : string; matches : match_item list }
[@@deriving yojson]

let lookup_sample q =
  match%lwt Api_utils.find_sample_by_identifier q with
  | Ok (s : Core.Types.sample) ->
      let name =
        Printf.sprintf "%s (%s)" s.sample_type
          (Core.Types.string_of_sample_category s.category)
      in
      Lwt.return
        [
          {
            resource_type = "sample";
            id = s.id;
            short_id = Some s.short_id;
            name;
            url = "/samples/" ^ s.short_id;
          };
        ]
  | Error _ -> Lwt.return []

let lookup_project q =
  match%lwt Api_utils.find_project_by_identifier q with
  | Ok (p : Core.Types.project) ->
      Lwt.return
        [
          {
            resource_type = "project";
            id = p.id;
            short_id = Some p.short_id;
            name = p.name;
            url = "/projects/" ^ p.short_id;
          };
        ]
  | Error _ -> Lwt.return []

let lookup_plate q =
  match%lwt Api_utils.find_plate_by_identifier q with
  | Ok (pl : Core.Types.plate) ->
      Lwt.return
        [
          {
            resource_type = "plate";
            id = pl.id;
            short_id = Some pl.short_id;
            name = pl.name;
            url = Printf.sprintf "/plates/%d" pl.id;
          };
        ]
  | Error _ -> Lwt.return []

let lookup_product q =
  match%lwt Api_utils.find_product_by_identifier q with
  | Ok (pr : Core.Types.product) ->
      Lwt.return
        [
          {
            resource_type = "product";
            id = pr.id;
            short_id = Some pr.short_id;
            name = pr.name;
            url = "/products/" ^ pr.short_id;
          };
        ]
  | Error _ -> Lwt.return []

let lookup_result_definition q =
  match%lwt Api_utils.find_result_definition_by_identifier q with
  | Ok (rd : Core.Types.result_definition) ->
      Lwt.return
        [
          {
            resource_type = "result_definition";
            id = rd.id;
            short_id = Some rd.short_id;
            name = rd.name;
            url = "/result-definitions/" ^ rd.short_id;
          };
        ]
  | Error _ -> Lwt.return []

let match_of_strain (st : Core.Types.strain) =
  {
    resource_type = "strain";
    id = st.id;
    short_id = Some st.strain_name;
    name = Printf.sprintf "%s %s (%s)" st.genus st.species st.strain_name;
    url = Printf.sprintf "/strains/%d" st.id;
  }

let lookup_strain q =
  match%lwt Api_utils.find_strain_by_identifier q with
  | Ok (st : Core.Types.strain) -> Lwt.return [ match_of_strain st ]
  | Error _ -> (
      let filters : Storage.Strain.strain_filters =
        { filter_term = q; filter_genus = None; filter_species = None }
      in
      match%lwt Storage.Strain.search_with_filters filters with
      | Ok strains ->
          let q_lower = String.lowercase_ascii q in
          let exact =
            List.filter
              (fun (s : Core.Types.strain) ->
                String.lowercase_ascii s.strain_name = q_lower)
              strains
          in
          Lwt.return (List.map match_of_strain exact)
      | Error _ -> Lwt.return [])

let lookup_community q =
  match%lwt Api_utils.find_community_by_identifier q with
  | Ok (c : Core.Types.community) ->
      Lwt.return
        [
          {
            resource_type = "community";
            id = c.id;
            short_id = None;
            name = c.name;
            url = Printf.sprintf "/communities/%d" c.id;
          };
        ]
  | Error _ -> Lwt.return []

let lookup_handler request =
  let q = Dream.query request "q" |> Option.value ~default:"" |> String.trim in
  let scope =
    Dream.query request "scope"
    |> Option.value ~default:"all"
    |> String.trim |> String.lowercase_ascii
  in
  let result =
    if q = "" then
      Lwt.return (Error (`Bad_Request "Query parameter 'q' is required"))
    else
      let lookup_tasks_res =
        match scope with
        | "" | "all" ->
            Ok
              [
                lookup_sample q;
                lookup_plate q;
                lookup_project q;
                lookup_strain q;
                lookup_community q;
                lookup_product q;
                lookup_result_definition q;
              ]
        | "sample" -> Ok [ lookup_sample q ]
        | "plate" -> Ok [ lookup_plate q ]
        | "project" -> Ok [ lookup_project q ]
        | "strain" -> Ok [ lookup_strain q ]
        | "community" -> Ok [ lookup_community q ]
        | "product" -> Ok [ lookup_product q ]
        | "result_definition" | "result-definition" ->
            Ok [ lookup_result_definition q ]
        | _ -> Error (`Bad_Request ("Invalid lookup scope: " ^ scope))
      in
      match lookup_tasks_res with
      | Error e -> Lwt.return (Error e)
      | Ok tasks ->
          let%lwt lists = Lwt.all tasks in
          let matches = List.concat lists in
          Lwt.return (Ok { query = q; matches })
  in
  Api_utils.handle_response ~serializer:yojson_of_lookup_response result

let routes = [ Dream.get "/api/v1/lookup" lookup_handler ]
