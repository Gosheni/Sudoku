(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2
open Lib

let _ = List.find

let test_pretty_printer _ =
  let expected_empty =
    "-------------------------\n\
     |       |       |       |\n\
     |       |       |       |\n\
     |       |       |       |\n\
     -------------------------\n\
     |       |       |       |\n\
     |       |       |       |\n\
     |       |       |       |\n\
     -------------------------\n\
     |       |       |       |\n\
     |       |       |       |\n\
     |       |       |       |\n\
     -------------------------\n"
  in
  assert_equal expected_empty @@ Sudoku_board.(pretty_print empty)

let series = "Tests" >::: [ "" >:: test_pretty_printer ]
let () = run_test_tt_main series
