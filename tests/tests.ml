(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2

let _ = List.find
let series = "Tests" >::: [ ("" >:: fun _ -> assert_equal (2 + 3) 4) ]
let () = run_test_tt_main series
