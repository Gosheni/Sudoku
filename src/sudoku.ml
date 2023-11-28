open Core

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
      | ("init", None) ->
        Stdio.print_endline "Initialized a new game!"
      | ("hint", None) ->
        Stdio.print_endline "Possible move is 8 at 2, 9"
      | ("solve", None) ->
        Stdio.print_endline "Solved the Sudoku game!"
      | ("move" | "save"), None ->
        Stdio.print_endline "No arguments provided for move or save command"
      | ("init" | "hint" | "solve"), Some _ ->
        Stdio.print_endline "Invalid arguments for command"
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
        | Failure _ -> Stdio.print_endline "Invalid arguments for move command")
      | "move", Some _ ->
        Stdio.print_endline "Invalid arguments for move command"
      | "save", Some [arg] when String.is_suffix arg ~suffix:".json" ->
        Stdio.printf "Current board saved to %s\n" arg
      | "save", Some _ ->
        Stdio.print_endline "Invalid filename"   
      | _ ->
        Stdio.print_endline "Invalid command"   
    ) 
  |> Command_unix.run
