open Alcotest
open Exlab_core.Types

(* We need to redefine result_payload_t locally or expose it *)
let result_payload_t = testable ResultPayload.pp ResultPayload.equal

let test_timeseries_append_success () =
  let initial = ResultPayload.FloatSeries [ (1, 42.0) ] in
  let append_point = (2, 84.0) in
  let expected = ResultPayload.FloatSeries [ (1, 42.0); (2, 84.0) ] in
  let actual =
    match initial with
    | ResultPayload.FloatSeries pts ->
        ResultPayload.FloatSeries
          (List.sort
             (fun (t1, _) (t2, _) -> Int.compare t1 t2)
             (append_point :: pts))
    | _ -> initial
  in
  Alcotest.(check result_payload_t) "Appended point" expected actual

let suite =
  [
    ( "Result Payload PATCH",
      [ test_case "Append point" `Quick test_timeseries_append_success ] );
  ]
