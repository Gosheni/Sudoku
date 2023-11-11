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

let create_board elems_list = 
  let rec loop_board i acc_board = 
    if i >= 81 then acc_board else
      let row = i / 9 in
      let col = i mod 9 in
      let elem_int = List.nth_exn elems_list row |> Fn.flip List.nth_exn col in
      Sudoku_board.set acc_board row col (Volatile elem_int) 
      |> loop_board (i + 1) 
  in
  loop_board 0 Sudoku_board.empty

let example_board_ints_1 = 
  [[1;2;3;6;7;8;9;4;5];
   [5;8;4;2;3;9;7;6;1];
   [9;6;7;1;4;5;3;2;8];
   [3;7;2;4;6;1;5;8;9];
   [6;9;1;5;8;3;2;7;4];
   [4;5;8;7;9;2;6;1;3];
   [8;3;6;9;2;4;1;5;7];
   [2;1;9;8;5;7;4;3;6];
   [7;4;5;3;1;6;8;9;2]]

let example_board_1 = create_board example_board_ints_1

let test_is_solved _ =
  assert_equal true @@ Sudoku_board.is_solved example_board_1

let series = "Tests" >::: 
[ "test pretty print" >:: test_pretty_printer;
  "test is_solved"    >:: test_is_solved;
]
let () = run_test_tt_main series
