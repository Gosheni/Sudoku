(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2
open Board

(* open Hint *)
open Game
(* open Grid *)

let force_unwrap = Option.value_exn

let test_pretty_printer _ =
  let expected_empty =
    "    1 2 3   4 5 6   7 8 9\n\
    \  -------------------------\n\
     1 |       |       |       |\n\
     2 |       |       |       |\n\
     3 |       |       |       |\n\
    \  -------------------------\n\
     4 |       |       |       |\n\
     5 |       |       |       |\n\
     6 |       |       |       |\n\
    \  -------------------------\n\
     7 |       |       |       |\n\
     8 |       |       |       |\n\
     9 |       |       |       |\n\
    \  -------------------------\n"
  in

  let expected_two_elements =
    "    1 2 3   4 5 6   7 8 9\n\
    \  -------------------------\n\
     1 |       |   1   |       |\n\
     2 |       |       |       |\n\
     3 |       |       |       |\n\
    \  -------------------------\n\
     4 |       |       |       |\n\
     5 |       |       |       |\n\
     6 |       |       |       |\n\
    \  -------------------------\n\
     7 |       |       |       |\n\
     8 |   7   |       |       |\n\
     9 |       |       |       |\n\
    \  -------------------------\n"
  in
  let two_elements =
    Sudoku_board.empty |> fun board ->
    Sudoku_board.set board 0 4 @@ Fixed 1 |> fun board ->
    Sudoku_board.set board 7 1 @@ Volatile 7
  in
  (assert_equal expected_empty @@ Sudoku_board.(pretty_print empty));
  assert_equal expected_two_elements @@ Sudoku_board.(pretty_print two_elements)

let create_board elems_list =
  let rec loop_board i acc_board =
    if i >= 81 then acc_board
    else
      let row = i / 9 in
      let col = i mod 9 in
      let elem_int = List.nth_exn elems_list row |> Fn.flip List.nth_exn col in
      if elem_int = 0 then
        Sudoku_board.set acc_board row col Empty |> loop_board (i + 1)
      else
        Sudoku_board.set acc_board row col (Fixed elem_int) |> loop_board (i + 1)
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

(* needs crooks to generate hints *)
let example_board_5_ints =
  [
    [ 0; 3; 9; 5; 0; 0; 0; 0; 0 ];
    [ 0; 0; 1; 8; 0; 9; 0; 7; 0 ];
    [ 0; 0; 0; 0; 1; 0; 9; 0; 4 ];
    [ 1; 0; 0; 4; 0; 0; 0; 0; 3 ];
    [ 0; 0; 0; 0; 0; 0; 0; 0; 7 ];
    [ 0; 0; 7; 0; 0; 0; 8; 6; 0 ];
    [ 0; 0; 6; 7; 0; 8; 2; 0; 0 ];
    [ 0; 1; 0; 0; 9; 0; 0; 0; 5 ];
    [ 0; 0; 0; 0; 0; 1; 0; 0; 8 ];
  ]

let example_board_5 = create_board example_board_5_ints

(* unsolvable *)
let example_board_6_ints =
  [
    [ 7; 8; 1; 5; 4; 3; 9; 2; 6 ];
    [ 0; 0; 6; 1; 7; 9; 5; 0; 0 ];
    [ 9; 5; 4; 6; 2; 8; 7; 3; 1 ];
    [ 6; 9; 5; 8; 3; 7; 2; 1; 4 ];
    [ 1; 4; 8; 2; 6; 5; 3; 7; 9 ];
    [ 3; 2; 7; 9; 1; 4; 8; 0; 0 ];
    [ 4; 1; 3; 7; 5; 2; 6; 9; 8 ];
    [ 0; 0; 2; 0; 0; 0; 4; 0; 0 ];
    [ 5; 7; 9; 4; 8; 6; 1; 0; 3 ];
  ]

let example_board_6 = create_board example_board_6_ints

(* multiple solutions *)
let example_board_7_ints =
  [
    [ 0; 8; 0; 0; 0; 9; 7; 4; 3 ];
    [ 0; 5; 0; 0; 0; 8; 0; 1; 0 ];
    [ 0; 1; 0; 0; 0; 0; 0; 0; 0 ];
    [ 8; 0; 0; 0; 0; 5; 0; 0; 0 ];
    [ 0; 0; 0; 8; 0; 4; 0; 0; 0 ];
    [ 0; 0; 0; 3; 0; 0; 0; 0; 6 ];
    [ 0; 0; 0; 0; 0; 0; 0; 7; 0 ];
    [ 0; 3; 0; 5; 0; 0; 0; 8; 0 ];
    [ 9; 7; 2; 4; 0; 0; 0; 5; 0 ];
  ]

let example_board_7 = create_board example_board_7_ints

(* should need guessing to solve *)
let example_board_8_ints =
  [
    [ 0; 9; 0; 7; 0; 0; 8; 6; 0 ];
    [ 0; 3; 1; 0; 0; 5; 0; 2; 0 ];
    [ 8; 0; 6; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 7; 0; 5; 0; 0; 0; 6 ];
    [ 0; 0; 0; 3; 0; 7; 0; 0; 0 ];
    [ 5; 0; 0; 0; 1; 0; 7; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 1; 0; 9 ];
    [ 0; 2; 0; 6; 0; 0; 3; 5; 0 ];
    [ 0; 5; 4; 0; 0; 8; 0; 7; 0 ];
  ]

let example_board_8 = create_board example_board_8_ints

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

let test_deserialize_valid_json _ =
  let json =
    `Assoc
      [
        ( "0",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 1 ]);
              ("1", `List [ `String "Fixed"; `Int 2 ]);
              ("2", `List [ `String "Fixed"; `Int 3 ]);
              ("3", `List [ `String "Fixed"; `Int 6 ]);
              ("4", `List [ `String "Fixed"; `Int 7 ]);
              ("5", `List [ `String "Fixed"; `Int 8 ]);
              ("6", `List [ `String "Fixed"; `Int 9 ]);
              ("7", `List [ `String "Fixed"; `Int 4 ]);
              ("8", `List [ `String "Fixed"; `Int 5 ]);
            ] );
        ( "1",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 5 ]);
              ("1", `List [ `String "Fixed"; `Int 8 ]);
              ("2", `List [ `String "Fixed"; `Int 4 ]);
              ("3", `List [ `String "Fixed"; `Int 2 ]);
              ("4", `List [ `String "Fixed"; `Int 3 ]);
              ("5", `List [ `String "Fixed"; `Int 9 ]);
              ("6", `List [ `String "Fixed"; `Int 7 ]);
              ("7", `List [ `String "Fixed"; `Int 6 ]);
              ("8", `List [ `String "Fixed"; `Int 1 ]);
            ] );
        ( "2",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 9 ]);
              ("1", `List [ `String "Fixed"; `Int 6 ]);
              ("2", `List [ `String "Fixed"; `Int 7 ]);
              ("3", `List [ `String "Fixed"; `Int 1 ]);
              ("4", `List [ `String "Fixed"; `Int 4 ]);
              ("5", `List [ `String "Fixed"; `Int 5 ]);
              ("6", `List [ `String "Fixed"; `Int 3 ]);
              ("7", `List [ `String "Fixed"; `Int 2 ]);
              ("8", `List [ `String "Fixed"; `Int 8 ]);
            ] );
        ( "3",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 3 ]);
              ("1", `List [ `String "Fixed"; `Int 7 ]);
              ("2", `List [ `String "Fixed"; `Int 2 ]);
              ("3", `List [ `String "Fixed"; `Int 4 ]);
              ("4", `List [ `String "Fixed"; `Int 6 ]);
              ("5", `List [ `String "Fixed"; `Int 1 ]);
              ("6", `List [ `String "Fixed"; `Int 5 ]);
              ("7", `List [ `String "Fixed"; `Int 8 ]);
              ("8", `List [ `String "Fixed"; `Int 9 ]);
            ] );
        ( "4",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 6 ]);
              ("1", `List [ `String "Fixed"; `Int 9 ]);
              ("2", `List [ `String "Fixed"; `Int 1 ]);
              ("3", `List [ `String "Fixed"; `Int 5 ]);
              ("4", `List [ `String "Fixed"; `Int 8 ]);
              ("5", `List [ `String "Fixed"; `Int 3 ]);
              ("6", `List [ `String "Fixed"; `Int 2 ]);
              ("7", `List [ `String "Fixed"; `Int 7 ]);
              ("8", `List [ `String "Fixed"; `Int 4 ]);
            ] );
        ( "5",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 4 ]);
              ("1", `List [ `String "Fixed"; `Int 5 ]);
              ("2", `List [ `String "Fixed"; `Int 8 ]);
              ("3", `List [ `String "Fixed"; `Int 7 ]);
              ("4", `List [ `String "Fixed"; `Int 9 ]);
              ("5", `List [ `String "Fixed"; `Int 2 ]);
              ("6", `List [ `String "Fixed"; `Int 6 ]);
              ("7", `List [ `String "Fixed"; `Int 1 ]);
              ("8", `List [ `String "Fixed"; `Int 3 ]);
            ] );
        ( "6",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 8 ]);
              ("1", `List [ `String "Fixed"; `Int 3 ]);
              ("2", `List [ `String "Fixed"; `Int 6 ]);
              ("3", `List [ `String "Fixed"; `Int 9 ]);
              ("4", `List [ `String "Fixed"; `Int 2 ]);
              ("5", `List [ `String "Fixed"; `Int 4 ]);
              ("6", `List [ `String "Fixed"; `Int 1 ]);
              ("7", `List [ `String "Fixed"; `Int 5 ]);
              ("8", `List [ `String "Fixed"; `Int 7 ]);
            ] );
        ( "7",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 2 ]);
              ("1", `List [ `String "Fixed"; `Int 1 ]);
              ("2", `List [ `String "Fixed"; `Int 9 ]);
              ("3", `List [ `String "Fixed"; `Int 8 ]);
              ("4", `List [ `String "Fixed"; `Int 5 ]);
              ("5", `List [ `String "Fixed"; `Int 7 ]);
              ("6", `List [ `String "Fixed"; `Int 4 ]);
              ("7", `List [ `String "Fixed"; `Int 3 ]);
              ("8", `List [ `String "Fixed"; `Int 6 ]);
            ] );
        ( "8",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 7 ]);
              ("1", `List [ `String "Fixed"; `Int 4 ]);
              ("2", `List [ `String "Fixed"; `Int 5 ]);
              ("3", `List [ `String "Fixed"; `Int 3 ]);
              ("4", `List [ `String "Fixed"; `Int 1 ]);
              ("5", `List [ `String "Fixed"; `Int 6 ]);
              ("6", `List [ `String "Fixed"; `Int 8 ]);
              ("7", `List [ `String "Fixed"; `Int 9 ]);
              ("8", `List [ `String "Fixed"; `Int 2 ]);
            ] );
      ]
  in
  Sudoku_board.deserialize json
  |> Option.map ~f:(Sudoku_board.equal example_board_1)
  |> Option.value_or_thunk ~default:(fun _ -> false)
  |> assert_bool ""

let test_serialize_deserialize _ =
  let full_sudoku = Sudoku_board.generate_random () in
  full_sudoku |> Sudoku_board.serialize |> Sudoku_board.deserialize
  |> (function
       | None -> false | Some board -> Sudoku_board.equal board full_sudoku)
  |> assert_bool ""

let test_deserialize_invalid : test =
  let json_of_single_row_sudoku : Sudoku_board.json =
    `Assoc [ ("0", `Assoc [ ("0", `List [ `String "Fixed"; `Int 1 ]) ]) ]
  in
  let json_with_invalid_element_correct_type : Sudoku_board.json =
    `Assoc [ ("0", `Assoc [ ("5", `List [ `String "Fixed"; `Int 100 ]) ]) ]
  in
  let json_with_invalid_element_incorrect_type1 : Sudoku_board.json =
    `Assoc
      [
        ( "0",
          `Assoc
            [
              ("4", `List [ `String "Fixed"; `Int 7 ]);
              ("5", `List [ `String "123"; `Int 1 ]);
            ] );
      ]
  in
  let json_with_invalid_element_incorrect_type2 : Sudoku_board.json =
    `Assoc
      [
        ( "0",
          `Assoc
            [
              ("7", `List [ `String "Fixed"; `Int 4 ]);
              ("8", `List [ `String "Fixed"; `String "5" ]);
            ] );
      ]
  in
  let json_with_duplicate_key1 : Sudoku_board.json =
    `Assoc [ ("0", `Assoc []); ("0", `Assoc []) ]
  in
  let json_with_duplicate_key2 : Sudoku_board.json =
    `Assoc
      [
        ( "0",
          `Assoc
            [
              ("0", `List [ `String "Fixed"; `Int 1 ]);
              ("0", `List [ `String "Fixed"; `Int 2 ]);
            ] );
      ]
  in
  let json_not_the_right_type : Sudoku_board.json =
    `String "This is not a board"
  in

  let all_invalid =
    [
      json_of_single_row_sudoku;
      json_with_invalid_element_correct_type;
      json_with_invalid_element_incorrect_type1;
      json_with_invalid_element_incorrect_type2;
      json_with_duplicate_key1;
      json_with_duplicate_key2;
      json_not_the_right_type;
    ]
  in

  List.map all_invalid ~f:Sudoku_board.deserialize
  |> List.map ~f:Option.is_none
  |> List.map ~f:(fun hopefully_true _ -> assert_bool "" hopefully_true)
  |> List.map ~f:(( >:: ) "test deserialize_invalid")
  |> test_list

let test_seed : test =
  test_list
    [
      ( "test seed with quickcheck" >:: fun _ ->
        Quickcheck.test ~sexp_of:[%sexp_of: int] Int.quickcheck_generator
          ~f:(fun i ->
            i |> Sudoku_board.seed_to_list
            |> List.sort ~compare:Int.compare
            |> assert_equal (List.range 1 10)) );
      ( "test that a seed of 0 is [1 2 3 4 5 6 7 8 9]" >:: fun _ ->
        assert_equal (Sudoku_board.seed_to_list 0) (List.range 1 10) );
    ]

let simple_invalid_board : Sudoku_board.t =
  Sudoku_board.(
    set empty 0 0 (Volatile 1) |> fun board -> set board 0 1 (Volatile 1))

let test_solve : test =
  let open Sudoku_board in
  let solved = solve_with_backtracking empty 0 is_valid in
  let test1 : test =
    "test" >:: fun _ ->
    match solved with
    | None ->
        assert_failure
          "solve_with_backtracking should always be able to solve an empty \
           sudoku"
    | Some sudoku ->
        assert_bool "" @@ is_solved sudoku;
        assert_bool "" @@ equal sudoku
        @@ (solve_with_backtracking sudoku 0 is_valid |> force_unwrap)
  in
  let test_invalid : test =
    "test" >:: fun _ ->
    solve_with_backtracking simple_invalid_board 0 is_valid |> assert_equal None
  in

  test_list [ test1; test_invalid ]

let test_solve_uniquely _ =
  let open Sudoku_board in
  assert_equal None @@ solve_with_unique_solution empty;
  let solved = solve_with_backtracking empty 0 is_valid |> force_unwrap in
  let missing_one = set solved 0 0 Empty in
  assert_bool ""
  @@ equal solved (solve_with_unique_solution missing_one |> force_unwrap)

let test_generate_solved : test =
  List.init 10 ~f:(fun _ ->
      "test generate_solved" >:: fun _ ->
      assert_bool "" (Sudoku_board.is_solved @@ Sudoku_board.generate_random ()))
  |> test_list

let number_of_empty (board : Sudoku_board.t) =
  Sudoku_board.get_all board |> List.count ~f:(Sudoku_board.equal_element Empty)

let test_generate_unsolved : test =
  let test_generate_single_unsolved _ : test =
    let board = Sudoku_board.generate_random () in
    50 :: List.range 1 20
    |> List.map ~f:(fun to_remove ->
           "test generate_single_unsolved" >:: fun _ ->
           let unsolved = Sudoku_board.generate_unsolved board to_remove in
           assert_equal false @@ Sudoku_board.is_solved unsolved;
           assert_bool ""
             (Sudoku_board.solve_with_unique_solution unsolved |> Option.is_some);
           if to_remove <= 3 then
             assert_equal to_remove @@ number_of_empty unsolved
             (* If you remove 3 elements from a solved sudoku it is guaranteed to still be solvable *)
           else assert_bool "" @@ (number_of_empty unsolved <= to_remove))
    |> test_list
  in

  List.init 20 ~f:test_generate_single_unsolved |> test_list

let example_board_1_incomplete = Sudoku_board.set example_board_1 0 0 Empty
let example_move = Game.{ x = 0; y = 0; value = Some 1 }

let test_hint_forced_moves _ =
  assert_equal Game.Already_solved @@ Game.generate_hint example_board_1;
  match Game.generate_hint example_board_1_incomplete with
  | Game.Suggested_move (new_move, _) -> assert_equal example_move new_move
  | _ -> assert_failure ""

let test_bad_boards _ =
  assert_equal None @@ Sudoku_board.solve_with_unique_solution example_board_6;
  (* unsolvable *)
  assert_equal None
  @@ Sudoku_board.solve_with_unique_solution
       example_board_7 (* multiple solutions *)

let apply_hints_till_solved_or_guess board =
  let rec loop board =
    match Game.generate_hint ~use_crooks:true board with
    | Game.Suggested_move (move, _) -> (
        match Game.do_move board move with
        | Error _ -> board
        | Ok new_board ->
            if Sudoku_board.is_solved new_board then new_board
            else loop new_board)
    | _ -> board
  in
  loop board

let apply_hints_without_crooks board =
  let rec loop board =
    match Game.generate_hint ~use_crooks:false board with
    | Game.Suggested_move (move, _) -> (
        match Game.do_move board move with
        | Error _ -> board
        | Ok new_board ->
            if Sudoku_board.is_solved new_board then new_board
            else loop new_board)
    | _ -> board
  in
  loop board

(* this test finds that approximately 1/5 randomly generated boards need crooks to solve *)
let test_is_crooks_needed : test =
  List.init 10 ~f:(fun _ ->
      "test generate_solved" >:: fun _ ->
      let solved = Sudoku_board.generate_random () in
      let unsolved = Sudoku_board.generate_unsolved solved 50 in
      let hints_applied = apply_hints_without_crooks unsolved in
      if Sudoku_board.is_solved hints_applied then assert_bool "" true
      else
        (* tests that crooks can come up with moves after non-crooks fails *)
        let hint = Game.generate_hint ~use_crooks:true hints_applied in
        match hint with
        | Suggest_guess _ -> assert_bool "" true
        | Already_solved -> assert_bool "" false
        | Suggested_move _ -> assert_bool "" true
        | _ -> assert_bool "" false)
  (* applying hints should never make a mistake *)
  |> test_list

let test_hints_and_moves _ =
  assert_equal true @@ Sudoku_board.is_solved
  @@ apply_hints_till_solved_or_guess example_board_3;
  assert_equal true @@ Sudoku_board.is_solved
  @@ apply_hints_till_solved_or_guess example_board_4;
  assert_equal true @@ Sudoku_board.is_solved
  @@ apply_hints_till_solved_or_guess
       example_board_5 (* board that needs crooks to solve *)

let test_io _ =
  let open Iolib.Configuration in
  let game = add_game "Test_game" 0 example_board_5 |> force_unwrap in
  let most_recent, _ = get_most_recent () |> force_unwrap in
  assert_equal game most_recent

let test_guess _ =
  let almost_solved = apply_hints_till_solved_or_guess example_board_8 in
  let hint = Game.generate_hint ~use_crooks:true almost_solved in
  match hint with
  | Suggest_guess _ -> assert_bool "" true
  | _ -> assert_bool "" false

let series =
  "Tests"
  >::: [
         "test pretty print" >:: test_pretty_printer;
         "test is_solved" >:: test_is_solved;
         "test is_valid" >:: test_is_valid;
         "test deserialize_valid" >:: test_deserialize_valid_json;
         test_deserialize_invalid;
         "test serialize_deserialize" >:: test_serialize_deserialize;
         test_seed;
         test_solve;
         "test solve uniquely" >:: test_solve_uniquely;
         test_generate_solved;
         test_generate_unsolved;
         "test hint forced moves" >:: test_hint_forced_moves;
         "test hints till solved" >:: test_hints_and_moves;
         "test bad boards" >:: test_bad_boards;
         test_is_crooks_needed;
         "test io" >:: test_io;
         "test guess needed" >:: test_guess;
       ]

let () = run_test_tt_main series
