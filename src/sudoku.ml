open Core
open Board
open Game.Sudoku_game

let save_to_json filename data =
  let json_opt = Sudoku_board.serialize data in
  match json_opt with
  | Some json ->
    Yojson.Safe.to_file filename json;
  | None ->
    failwith "Failed to serialize data"

let load_from_json filename =
  try
    let json_txt = In_channel.read_all filename in
    let json = Yojson.Safe.from_string json_txt in
    match Sudoku_board.deserialize json with
    | Some board -> board
    | None -> failwith "Board is empty"
  with
  | _ -> failwith "Invalid file"

let make_move current_board a b c =
  (try
    let value = int_of_string a in
    let row = int_of_string b in
    let col = int_of_string c in
    if (1 <= value && value <= 9) && (1 <= row && row <= 9) && (1 <= col && col <= 9) then (
      Stdio.printf "Making a move: Add value %d to row %d col %d\n" value row col;
      let move = { x = row; y = col; value = Some value } in
      match do_move current_board move with 
      | Ok board -> 
        save_to_json "sudoku_game.json" board;
        Stdio.print_endline (Sudoku_board.pretty_print board)
      | Error _ -> 
        Stdio.print_endline "Error\n"
    ) else
      Stdio.print_endline "Invalid arguments for move command: Values out of range (1-9)"
  with
  | Failure _ -> Stdio.print_endline "Invalid arguments for move command: Integers expected")  

  
let () =
  Command.basic
    ~summary:"sudoku.exe - Generate Sudoku Game from a command line"
    (let%map_open.Command command_string =
        flag "--command" (required string) ~doc:"Command (init, hint, solve, save, move)"
      and command_args =
        anon (maybe (sequence ("arg" %: string)))
      in
    fun () ->
      match (String.lowercase command_string, command_args) with
      | "init", None ->
        let full_board = Sudoku_board.generate_random () in
        let current_board = Sudoku_board.generate_degenerate full_board 54 in (* Default number of elements to be removed from full board: 54 *)
        save_to_json "sudoku_game.json" current_board;
        Stdio.print_endline "Initialized a new game!";
        Stdio.print_endline (Sudoku_board.pretty_print current_board)

      | "hint", None ->
        let current_board = load_from_json "sudoku_game.json" in
        (match generate_hint current_board with
        | Incorrect_cell ->
          Stdio.printf "Incorrect cell somewhere\n";
        | Suggest_guess ->
          Stdio.printf "No suggested move. Try to guess first\n";
        | Suggested_move (move, desc) ->
          Stdio.printf "Suggested move: Add value %d to row %d col %d\n" (Option.value_exn move.value) move.x move.y;
          Stdio.print_endline desc;
        | Already_solved ->
          Stdio.print_endline "The puzzle is already solved!";
        );
        Stdio.print_endline (Sudoku_board.pretty_print current_board)

      | "solve", None ->
        let current_board = load_from_json "sudoku_game.json" in
        (match Sudoku_board.solve_with_unique_solution current_board with 
        | Some board ->
          save_to_json "sudoku_game.json" board;
          Stdio.print_endline "Solved the Sudoku game!";
          Stdio.print_endline (Sudoku_board.pretty_print board)
        | None ->
          Stdio.print_endline "Unsolvable!"
        )

      | "move", Some [a; b; c] ->
        let current_board = load_from_json "sudoku_game.json" in
        make_move current_board a b c;

      | "save", Some [arg] when String.is_suffix arg ~suffix:".json" ->
        let current_board = load_from_json "sudoku_game.json" in
        save_to_json arg current_board;
        Stdio.printf "Current board saved to %s:\n" arg

      | "load", Some [arg] when String.is_suffix arg ~suffix:".json" ->
        Stdio.printf "Loading board from %s:\n" arg;
        Stdio.print_endline (Sudoku_board.pretty_print (load_from_json arg))

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
