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
    if i >= 81 then acc_board
    else
      let row = i / 9 in
      let col = i mod 9 in
      let elem_int = List.nth_exn elems_list row |> Fn.flip List.nth_exn col in
      if elem_int = 0 then
        Sudoku_board.set_forced acc_board row col Empty |> loop_board (i + 1)
      else
        Sudoku_board.set_forced acc_board row col (Fixed elem_int)
        |> loop_board (i + 1)
  in
  loop_board 0 Sudoku_board.empty

let example_board_ints_1 =
  [
    [ 1; 2; 3; 6; 7; 8; 9; 4; 5 ];
    [ 5; 8; 4; 2; 3; 9; 7; 6; 1 ];
    [ 9; 6; 7; 1; 4; 5; 3; 2; 8 ];
    [ 3; 7; 2; 4; 6; 1; 5; 8; 9 ];
    [ 6; 9; 1; 5; 8; 3; 2; 7; 4 ];
    [ 4; 5; 8; 7; 9; 2; 6; 1; 3 ];
    [ 8; 3; 6; 9; 2; 4; 1; 5; 7 ];
    [ 2; 1; 9; 8; 5; 7; 4; 3; 6 ];
    [ 7; 4; 5; 3; 1; 6; 8; 9; 2 ];
  ]

let example_board_1 = create_board example_board_ints_1

let example_board_ints_2 =
  [
    [ 5; 3; 4; 6; 7; 8; 9; 1; 2 ];
    [ 6; 7; 2; 1; 9; 5; 3; 4; 8 ];
    [ 1; 9; 8; 3; 4; 2; 5; 6; 7 ];
    [ 8; 5; 9; 7; 6; 1; 4; 2; 3 ];
    [ 4; 2; 6; 8; 5; 3; 7; 9; 1 ];
    [ 7; 1; 3; 9; 2; 4; 8; 5; 6 ];
    [ 9; 6; 1; 5; 3; 7; 2; 8; 4 ];
    [ 2; 8; 7; 4; 1; 9; 6; 3; 5 ];
    [ 3; 4; 5; 2; 8; 6; 1; 7; 9 ];
  ]

let example_board_2 = create_board example_board_ints_2

let example_invalid_board_ints =
  [
    [ 5; 3; 4; 6; 7; 8; 9; 1; 2 ];
    [ 6; 7; 2; 1; 9; 5; 3; 4; 8 ];
    [ 1; 9; 8; 3; 4; 2; 5; 6; 7 ];
    [ 8; 5; 9; 7; 6; 1; 4; 2; 3 ];
    [ 4; 2; 6; 8; 5; 3; 7; 9; 1 ];
    [ 7; 1; 3; 9; 4; 2; 8; 5; 6 ];
    [ 9; 6; 1; 5; 3; 7; 2; 8; 4 ];
    [ 2; 8; 7; 4; 1; 9; 6; 3; 5 ];
    [ 3; 4; 5; 2; 8; 6; 1; 7; 9 ];
  ]

let example_invalid = create_board example_invalid_board_ints

let example_board_ints_3 =
  [
    [ 0; 0; 6; 0; 0; 0; 0; 0; 1 ];
    [ 0; 7; 0; 0; 6; 0; 0; 5; 0 ];
    [ 8; 0; 0; 1; 0; 3; 2; 0; 0 ];
    [ 0; 0; 5; 0; 4; 0; 8; 0; 0 ];
    [ 0; 4; 0; 7; 0; 2; 0; 9; 0 ];
    [ 0; 0; 8; 0; 1; 0; 7; 0; 0 ];
    [ 0; 0; 1; 2; 0; 5; 0; 0; 3 ];
    [ 0; 6; 0; 0; 7; 0; 0; 8; 0 ];
    [ 2; 0; 0; 0; 0; 0; 4; 0; 0 ];
  ]

let example_board_3 = create_board example_board_ints_3

(* this + above can probably be used to test a solve function once we implement that *)
let example_board_ints_3_solved =
  [
    [ 5; 3; 6; 8; 2; 7; 9; 4; 1 ];
    [ 1; 7; 2; 9; 6; 4; 3; 5; 8 ];
    [ 8; 9; 4; 1; 5; 3; 2; 6; 7 ];
    [ 7; 1; 5; 3; 4; 9; 8; 2; 6 ];
    [ 6; 4; 3; 7; 8; 2; 1; 9; 5 ];
    [ 9; 2; 8; 5; 1; 6; 7; 3; 4 ];
    [ 4; 8; 1; 2; 9; 5; 6; 7; 3 ];
    [ 3; 6; 9; 4; 7; 1; 5; 8; 2 ];
    [ 2; 5; 7; 6; 3; 8; 4; 1; 9 ];
  ]

let example_board_3_solved = create_board example_board_ints_3_solved

let example_board_ints_4 = 
  [ 
    [ 3; 0; 6; 5; 0; 8; 4; 0; 0 ];
    [ 5; 2; 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 8; 7; 0; 0; 0; 0; 3; 1 ];
    [ 0; 0; 3; 0; 1; 0; 0; 8; 0 ];
    [ 9; 0; 0; 8; 6; 3; 0; 0; 5 ];
    [ 0; 5; 0; 0; 9; 0; 6; 0; 0 ];
    [ 1; 3; 0; 0; 0; 0; 2; 5; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0; 7; 4 ];
    [ 0; 0; 5; 2; 0; 6; 3; 0; 0 ];
  ]

let example_board_4 = create_board example_board_ints_4

let test_is_solved _ =
  assert_equal true @@ Sudoku_board.is_solved example_board_1;
  assert_equal false @@ Sudoku_board.is_solved Sudoku_board.empty;
  assert_equal true @@ Sudoku_board.is_solved example_board_2;
  assert_equal false @@ Sudoku_board.is_solved example_invalid;
  assert_equal false @@ Sudoku_board.is_solved example_board_4 

let test_is_valid _ =
  assert_equal true @@ Sudoku_board.is_valid example_board_1;
  assert_equal true @@ Sudoku_board.is_valid example_board_2;
  assert_equal false @@ Sudoku_board.is_valid example_invalid;
  assert_equal true @@ Sudoku_board.is_valid example_board_3;
  assert_equal true @@ Sudoku_board.is_valid example_board_3_solved;
  assert_equal true @@ Sudoku_board.is_valid example_board_4

let series =
  "Tests"
  >::: [
         "test pretty print" >:: test_pretty_printer;
         "test is_solved" >:: test_is_solved;
         "test is_valid" >:: test_is_valid;
       ]

let () = run_test_tt_main series
