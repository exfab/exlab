open Alcotest

(* Helpers *)
let pp_system_setting = Exlab_core.Types.pp_system_setting
let equal_system_setting = Exlab_core.Types.equal_system_setting

let check_roundtrip (type a) (testable : a testable)
    (to_json : a -> Yojson.Safe.t) (of_json : Yojson.Safe.t -> a) (msg : string)
    (input : a) =
  let json = to_json input in
  let decoded = of_json json in
  check testable msg input decoded

(* Helper to simulate a CSV row *)
let mock_row (pairs : (string * string) list) :
    Exlab_server.Csv_utils.row_accessor =
  let tbl = Hashtbl.create (List.length pairs) in
  List.iter (fun (k, v) -> Hashtbl.add tbl k v) pairs;
  fun k -> try Hashtbl.find tbl k with Not_found -> ""
