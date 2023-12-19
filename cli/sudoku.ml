open Core
open Board
open Game
open Iolib

exception NotIntError of string
exception OutsideOfRangeError of (int * int)
exception NoRecentGame
exception GameWithNameAlreadyExists

let get_most_recent_exn _ : Configuration.game * Board.Sudoku_board.t =
  match Configuration.get_most_recent () with
  | None -> raise NoRecentGame
  | Some a -> a

let get_int_in_range_or_exn (str : string) (min : int) (max : int) : int =
  match int_of_string_opt str with
  | None -> raise (NotIntError str)
  | Some a ->
      if a < min || a > max then raise (OutsideOfRangeError (min, max)) else a

let handle_exn (e : exn) : unit =
  match e with
  | NotIntError str ->
      Stdio.print_endline
      @@ "Invalid arguments for command: Integers were expected, instead the \
          command received " ^ str
  | OutsideOfRangeError (min, max) ->
      Stdio.printf
        "Invalid arguments for command: Values out of range (%d-%d)\n" min max
  | NoRecentGame ->
      Stdio.printf
        "There is no recent game. You can create one with the init command\n"
  | GameWithNameAlreadyExists ->
      Stdio.printf "A game with that name already exists\n"
  | _ -> Stdio.print_endline "An unknown error occured"

let make_move (game : Configuration.game) (v : int option) r c =
  let move = { x = r - 1; y = c - 1; value = v } in
  let current_board =
    match Configuration.load_board_from_json game with
    | None -> failwith "Board not found"
    | Some a -> a
  in
  match do_move current_board move with
  | Ok board ->
      Stdio.print_endline (Sudoku_board.pretty_print board);
      if Sudoku_board.is_solved board then (
        Stdio.print_endline "Solved the Sudoku game!";
        (match Configuration.finish_game game true with
        | Ok () -> Stdio.print_endline "Game finished and recorded\n"
        | Error msg -> Stdio.print_endline ("Error: " ^ msg));
        Configuration.update_name_for_highscore game.name game.name)
      else Configuration.update_game game board
  | Error e -> (
      match e with
      | Invalid_position -> Stdio.printf "Invalid position given\n"
      | Fixed_cell -> Stdio.printf "Cannot change a fixed cell\n"
      | Already_present -> Stdio.printf "Value already present in cell\n")

let init_with (name : string) (difficulty : int) =
  let current_board =
    Sudoku_board.generate_unsolved (Sudoku_board.generate_random ()) difficulty
  in
  match Configuration.add_game name 50 current_board with
  | None -> raise GameWithNameAlreadyExists
  | Some _ ->
      Stdio.printf "Initialized a new game %s!\n" name;
      Stdio.print_endline (Sudoku_board.pretty_print current_board)

let handle_command command_string command_args =
  match (String.lowercase command_string, command_args) with
  | "init", Some [ a ] ->
      init_with a 50 (* Generates a board with (up to) 50 missing elements *)
  | "init", Some [ a; b ] ->
      let difficulty = get_int_in_range_or_exn a 1 70 in
      init_with b difficulty
  | "hint", None ->
      let _, current_board = get_most_recent_exn () in
      let hint = generate_hint ~use_crooks:true current_board in
      Stdio.print_endline @@ describe_hint hint;
      Stdio.print_endline (Sudoku_board.pretty_print current_board)
  | "solve", None -> (
      let metadata, current_board = get_most_recent_exn () in
      match Sudoku_board.solve_with_unique_solution current_board with
      | Some board -> (
          Stdio.print_endline "Solved the Sudoku game!";
          Stdio.print_endline (Sudoku_board.pretty_print board);
          match Configuration.finish_game metadata false with
          | Ok () -> Stdio.print_endline "Game finished and recorded\n"
          | Error msg -> Stdio.print_endline ("Error: " ^ msg))
      | None ->
          Stdio.print_endline "This board does not have a unique solution!")
  | "move", Some [ a; b; c ] ->
      let metadata, _ = get_most_recent_exn () in
      let value = get_int_in_range_or_exn a 1 9 in
      let row = get_int_in_range_or_exn b 1 9 in
      let col = get_int_in_range_or_exn c 1 9 in
      Stdio.printf "Making a move: Add value %d to row %d col %d\n" value row
        col;
      make_move metadata (Some value) row col
  | "remove", Some [ a; b ] ->
      let metadata, _ = get_most_recent_exn () in
      let row = get_int_in_range_or_exn a 1 9 in
      let col = get_int_in_range_or_exn b 1 9 in
      Stdio.printf "Removing a value from row %d col %d\n" row col;
      make_move metadata None row col
  | "load", Some [ arg ] -> (
      Stdio.printf "Loading game %s:\n" arg;
      match Configuration.move_game_to_first arg with
      | Some game -> Stdio.print_endline (Sudoku_board.pretty_print game)
      | None -> Stdio.print_endline "Unable to load game")
  | "scores", None -> (
      Stdio.print_endline "High scores:";

      match Configuration.get_highscores () with
      | None -> Stdio.print_endline "Play some games to get some high scores"
      | Some (recent, top10) ->
          let _, high_scores =
            List.fold top10 ~init:(1, "") ~f:(fun (idx, acc_str) highscore ->
                ( idx + 1,
                  acc_str ^ Int.to_string idx ^ ". "
                  ^ Option.value highscore.username ~default:highscore.id
                  ^ " Score: "
                  ^ Float.to_string highscore.total_time
                  ^ "\n" ))
          in
          Stdio.print_endline high_scores;
          (* As the top10 is nonempty exists a most recent highscore *)
          Stdio.print_endline
            ("Most recent game: \n"
            ^ Option.value recent.username ~default:recent.id
            ^ " Score: "
            ^ Float.to_string recent.total_time
            ^ "\n"))
  | "print", None ->
      let _, board = get_most_recent_exn () in
      Stdio.print_endline (Sudoku_board.pretty_print board)
  | "list", None ->
      let names = Configuration.get_all_names () in
      let _ = List.map names ~f:Stdio.print_endline in
      ()
  | ("hint" | "solve" | "scores" | "print" | "list"), Some _ ->
      Stdio.print_endline
        "Unexpected arguments provided for hint, solve, scores, print, or list \
         command"
  | ("move" | "remove" | "load" | "init"), _ ->
      Stdio.print_endline
        "Invalid arguments for init, move, remove, save, or load command"
  | _ -> Stdio.print_endline "Invalid command"

let () =
  Command.basic ~summary:"sudoku.exe - Generate Sudoku Game from a command line"
    (let%map_open.Command command_string = anon ("COMMAND" %: string)
     and command_args = anon (maybe (sequence ("arg" %: string))) in
     fun () ->
       try handle_command command_string command_args with a -> handle_exn a)
  |> Command_unix.run
