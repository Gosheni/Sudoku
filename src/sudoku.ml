open Core
open Board
open Game

let save_to_json filename data =
  let json_opt = Sudoku_board.serialize data in
  match json_opt with
  | Some json ->
    Yojson.Safe.to_file filename json;
    "Current board saved"
  | None ->
    "Failed to serialize data"

let load_from_json filename =
  try
    match Sudoku_board.deserialize filename with
    | Some board -> Sudoku_board.pretty_print board
    | None -> "Board is empty"
  with
  | _ -> "Invalid file"

let () =
  Command.basic
    ~summary:"sudoku.exe - Generate Sudoku Game from a command line"
    (let%map_open.Command command_string =
        flag "--command" (required string) ~doc:"Command (init, hint, solve, save, move)"
      and command_args =
        anon (maybe (sequence ("arg" %: string)))
      in
    fun () ->
      let current_board = Sudoku_board.empty in

      match (String.lowercase command_string, command_args) with
      | ("init", None) ->
        let full_board = Sudoku_board.generate_random () in
        let current_board = Sudoku_board.generate_degenerate full_board 54 in (* Default number of elements to be removed from full board: 54 *)
        Stdio.print_endline "Initialized a new game!";
        Stdio.print_endline (Sudoku_board.pretty_print current_board)

      | ("hint", None) ->
        (match Sudoku_game.generate_hint current_board with
        | Incorrect_cell (row, col) ->
          Stdio.printf "Incorrect cell at %d, %d\n" row col;
        | Suggested_move move ->
          Stdio.printf "Suggested move: Add value %d to row %d col %d\n" (Option.value_exn move.value) move.x move.y;
        | Already_solved ->
          Stdio.print_endline "The puzzle is already solved!";
        )

      | ("solve", None) ->
        (match Sudoku_board.solve current_board with 
        | Some board ->
          Stdio.print_endline "Solved the Sudoku game!";
          Stdio.print_endline (Sudoku_board.pretty_print board)
        | None ->
          Stdio.print_endline "Unsolvable!"
        )

      | "move", Some [a; b; c] ->
        (try
          let value = int_of_string a in
          let row = int_of_string b in
          let col = int_of_string c in
          if (1 <= value && value <= 9) && (1 <= row && row <= 9) && (1 <= col && col <= 9) then
            Stdio.printf "Made a move: Add value %d to row %d col %d\n" value row col
            
          else
            Stdio.print_endline "Invalid arguments for move command: Values out of range (1-9)"
        with
        | Failure _ -> Stdio.print_endline "Invalid arguments for move command: Integers expected")

      | "save", Some [arg] when String.is_suffix arg ~suffix:".json" ->
        Stdio.printf "Saving board to %s:\n" arg;
        Stdio.print_endline (save_to_json arg current_board)

      | "load", Some [arg] when String.is_suffix arg ~suffix:".json" ->
        Stdio.printf "Loading board from %s:\n" arg;
        let json_txt = In_channel.read_all arg in
        Stdio.print_endline (load_from_json (Yojson.Safe.from_string json_txt))

      | ("init" | "hint" | "solve"), Some _ ->
        Stdio.print_endline "Unexpected arguments provided for init, hint, or solve command"

      | ("move" | "save" | "load"), Some _ ->
        Stdio.print_endline "Invalid arguments for move, save, or load command"

      | ("move" | "save" | "load"), None ->
        Stdio.print_endline "No arguments provided for move, save, or load command"

      | _ ->
        Stdio.print_endline "Invalid command"   
    ) 
  |> Command_unix.run
