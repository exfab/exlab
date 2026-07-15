(* test/unit/test_user.ml *)

open Alcotest
open Exlab_core.Types
open Test_helpers

(*  Define the Alcotest testable for the user type using the generated show and eq functions *)
let user_t = testable pp_user equal_user

(* Create the dummy user factory *)
let make_dummy_user () : user =
  {
    id = 1;
    uid = "user-1234-abcd";
    email = "test@exlab.com";
    password_hash = "some_secure_hash_string";
    role = Lab_manager;
    created_at = 1700000000.0;
    updated_at = 1700000000.0;
  }

(* The Roundtrip Test *)
let test_user_roundtrip () =
  check_roundtrip user_t yojson_of_user user_of_yojson "User Roundtrip"
    (make_dummy_user ())

(* Password Hashing Tests *)

let test_validate_password_valid () =
  let result = Exlab_core.User.validate_password "securepassword123" in
  Alcotest.(check (result unit string))
    "Valid password returns Ok ()" (Ok ()) result

let test_validate_password_too_short () =
  let result = Exlab_core.User.validate_password "short" in
  Alcotest.(check (result unit string))
    "Short password returns Error"
    (Error "Password must be at least 8 characters long") result

let test_hash_not_plaintext () =
  let plaintext = "my_super_secret_password" in
  let hashed = Exlab_core.User.hashed_password plaintext in
  Alcotest.(check bool)
    "Hash should not equal plaintext" true (plaintext <> hashed)

let test_verify_correct_password () =
  let plaintext = "correct_horse_battery_staple" in
  let hashed = Exlab_core.User.hashed_password plaintext in
  let is_valid = Exlab_core.User.verify_password ~plaintext ~hashed in
  Alcotest.(check bool)
    "Verification returns true for correct password" true is_valid

let test_verify_incorrect_password () =
  let plaintext = "real_password" in
  let hashed = Exlab_core.User.hashed_password plaintext in
  let is_valid =
    Exlab_core.User.verify_password ~plaintext:"wrong_password" ~hashed
  in
  Alcotest.(check bool)
    "Verification returns false for wrong password" false is_valid

let test_to_safe_user_omits_hash () =
  let raw_user = make_dummy_user () in
  let safe_user = Exlab_server.Api_types.User.to_safe_user raw_user in
  let json = Exlab_server.Api_types.User.yojson_of_safe_user safe_user in
  let json_str = Yojson.Safe.to_string json in

  let re = Re.compile (Re.str "password_hash") in
  let contains_hash = Re.execp re json_str in

  Alcotest.(check bool)
    "JSON should not contain password_hash" false contains_hash

let suite =
  [
    ( "User",
      [
        test_case "Roundtrip" `Quick test_user_roundtrip;
        test_case "Validate valid password" `Quick test_validate_password_valid;
        test_case "Validate short password" `Quick
          test_validate_password_too_short;
        test_case "Hash obfuscation" `Quick test_hash_not_plaintext;
        test_case "Verify correct password" `Quick test_verify_correct_password;
        test_case "Reject incorrect password" `Quick
          test_verify_incorrect_password;
        test_case "Safe user omits hash" `Quick test_to_safe_user_omits_hash;
      ] );
  ]
