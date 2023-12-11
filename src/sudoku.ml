open Core
open Board
open Game.Sudoku_game
open Iolib

let default_game_file = "sudoku_game.json"

let make_move current_board (v : int option) r c =
  let move = { x = r - 1; y = c - 1; value = v } in

  match do_move current_board move with
  | Ok board ->
      save_board_to_json default_game_file board;
      Stdio.print_endline (Sudoku_board.pretty_print board)
  | Error e ->
      match e with 
        | Invalid_position -> Stdio.printf "Invalid position given\n"
        | Fixed_cell -> Stdio.printf "Cannot change a fixed cell\n"
        | Already_present -> Stdio.printf "Value already present in cell\n"

let get_board_exn (filename : string) =
  match load_board_from_json filename with
  | None -> failwith "Current game not found"
  | Some a -> a

let get_current_board_exn _ = get_board_exn default_game_file

let () =
  Command.basic ~summary:"sudoku.exe - Generate Sudoku Game from a command line"
    (let%map_open.Command command_string = anon ("COMMAND" %: string)
     and command_args = anon (maybe (sequence ("arg" %: string))) in
     fun () ->
       match (String.lowercase command_string, command_args) with
       | "init", None ->
           let full_board = Sudoku_board.generate_random () in
           let current_board = Sudoku_board.generate_degenerate full_board 54 in
           (* Default number of elements to be removed from full board: 54 *)
           save_board_to_json default_game_file current_board;
           Stdio.print_endline "Initialized a new game!";
           Stdio.print_endline (Sudoku_board.pretty_print current_board)
       | "hint", None ->
           let current_board = get_current_board_exn () in
           (match generate_hint current_board with
           | Incorrect_cell -> Stdio.printf "Puzzle no longer has a unique solution. There is an incorrect cell somewhere\n"
           | Suggest_guess (move, desc) ->
               Stdio.printf "No suggested move is present. Try to guess at row %d col %d\n"
                  (move.x + 1) (move.y + 1);
               Stdio.print_endline desc
           | Suggested_move (move, desc) ->
               Stdio.printf "Suggested move: Add value %d to row %d col %d\n"
                 (Option.value_exn move.value)
                 (move.x + 1) (move.y + 1);
               Stdio.print_endline desc
           | Already_solved ->
               Stdio.print_endline "The puzzle is already solved!");
           Stdio.print_endline (Sudoku_board.pretty_print current_board)
       | "solve", None -> (
           match
             Sudoku_board.solve_with_unique_solution @@ get_current_board_exn ()
           with
           | Some board ->
               save_board_to_json default_game_file board;
               Stdio.print_endline "Solved the Sudoku game!";
               Stdio.print_endline (Sudoku_board.pretty_print board)
           | None -> Stdio.print_endline "Unsolvable!")
       | "move", Some [ a; b; c ] -> (
           let current_board = get_current_board_exn () in
           try
             let value = int_of_string a in
             let row = int_of_string b in
             let col = int_of_string c in
             if
               (1 <= value && value <= 9)
               && (1 <= row && row <= 9)
               && 1 <= col && col <= 9
             then (
               Stdio.printf "Making a move: Add value %d to row %d col %d\n"
                 value row col;
               make_move current_board (Some value) row col)
             else
               Stdio.print_endline
                 "Invalid arguments for move command: Values out of range (1-9)"
           with Failure _ ->
             Stdio.print_endline
               "Invalid arguments for move command: Integers expected")
       | "remove", Some [ a; b ] -> (
           let current_board = get_current_board_exn () in
           try
             let row = int_of_string a in
             let col = int_of_string b in

             if (1 <= row && row <= 9) && 1 <= col && col <= 9 then (
               Stdio.printf "Removing a value from row %d col %d\n" row col;
               make_move current_board None row col)
             else
               Stdio.print_endline
                 "Invalid arguments for remove command: Values out of range \
                  (1-9)"
           with Failure _ ->
             Stdio.print_endline
               "Invalid arguments for remove command: Integers expected")
       | "save", Some [ arg ] when String.is_suffix arg ~suffix:".json" ->
           save_board_to_json arg @@ get_current_board_exn ();
           Stdio.printf "Current board saved to %s:\n" arg
       | "load", Some [ arg ] when String.is_suffix arg ~suffix:".json" ->
           Stdio.printf "Loading board from %s:\n" arg;
           Stdio.print_endline (Sudoku_board.pretty_print (get_board_exn arg))
       | ("init" | "hint" | "solve"), Some _ ->
           Stdio.print_endline
             "Unexpected arguments provided for init, hint, or solve command"
       | ("move" | "remove" | "save" | "load"), _ ->
           Stdio.print_endline
             "Invalid arguments for move, remove, save, or load command"
       | _ -> Stdio.print_endline "Invalid command")
  |> Command_unix.run
