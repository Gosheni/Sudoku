open Board
open Core

module Configuration = struct
  type highscore = { name : string; difficulty : int; total_time : float }
  [@@deriving equal, yojson]

  type highscore_list = highscore list [@@deriving equal, yojson]

  type game = {
    name : string;
    file_location : string;
    start_time : float;
    difficulty : int;
  }
  [@@deriving equal, yojson]

  type t = { highscores : highscore_list; games : game list }
  [@@deriving equal, yojson]

  let empty = { highscores = []; games = [] }
  let location = "sudoku.config"

  let save_board_to_json game data =
    let filename = game.file_location in
    if String.(suffix filename 5 = ".json") && String.length filename > 5 then
      Sudoku_board.serialize data |> Yojson.Safe.to_file filename
    else failwith "Invalid filename"

  let delete_game game =
    let filename = game.file_location in
    if
      String.contains filename '/'
      || String.is_substring ~substring:".." filename
    then ()
    else
      try Sys_unix.remove filename
      with Sys_error msg ->
        Stdio.eprintf "Error deleting file '%s': %s\n" filename msg

  let load_board_from_json game : Sudoku_board.t option =
    try Yojson.Safe.from_file game.file_location |> Sudoku_board.deserialize
    with _ -> None

  let load_config _ : t =
    try
      let possible_config = Yojson.Safe.from_file location |> of_yojson in
      match possible_config with Ok a -> a | _ -> empty
    with _ -> empty

  let save_config (config : t) : unit =
    to_yojson config |> Yojson.Safe.to_file location

  let add_game (name : string) (difficulty : int) (board : Sudoku_board.t) :
      game =
    let filename = (name |> String.filter ~f:Char.is_alphanum) ^ ".json" in
    let config = load_config () in
    let game =
      {
        name;
        file_location = filename;
        start_time = Core_unix.time ();
        difficulty;
      }
    in
    save_board_to_json game board;
    save_config { highscores = config.highscores; games = game :: config.games };
    game

  let get_game_with_name (name : string) : game option =
    let config = load_config () in
    List.find config.games ~f:(fun game -> String.(game.name = name))

  let update_game (game : game) (board : Sudoku_board.t) : unit =
    match get_game_with_name game.name with
    | None -> ()
    | Some game_data -> save_board_to_json game_data board

  let get_game (name : string) : (game * Board.Sudoku_board.t) option =
    match get_game_with_name name with
    | None -> None
    | Some game_data -> (
        match load_board_from_json game_data with
        | None -> None
        | Some board -> Some (game_data, board))

  let get_most_recent _ : (game * Board.Sudoku_board.t) option =
    let config = load_config () in
    List.hd config.games |> Option.bind ~f:(fun game -> get_game game.name)

  let get_most_recent_exn _ : game * Board.Sudoku_board.t =
    match get_most_recent () with
    | None -> failwith "Current game not found"
    | Some a -> a
  
  let get_highscores _ : highscore list =
    let config = load_config () in
    config.highscores

  (* keep only best 10 scores *)
  let add_new_score (score : highscore) : highscore list =
    let config = load_config () in
    let top_ten = 
      List.sort (score::config.highscores) ~compare:(fun a b ->
          Float.compare a.total_time b.total_time)
      |> Fn.flip List.take 10
    in
    score::top_ten


  let move_game_to_first game_name : Sudoku_board.t option =
    match get_game_with_name game_name with
    | None -> None
    | Some game ->
        let config = load_config () in
        let new_games_list =
          List.filter config.games ~f:(fun g -> String.(g.name <> game_name))
        in
        save_config
          { highscores = config.highscores; games = game :: new_games_list };
        load_board_from_json game

  let finish_game (game : game) : (unit, string) result =
    let config = load_config () in
    match get_game_with_name game.name with
    | None -> Error "This game does not exist"
    | Some game ->
        let time_spent = Float.(Core_unix.time () - game.start_time) in
        let new_games_list =
          List.filter config.games ~f:(fun other_game ->
              String.(other_game.name <> game.name))
        in
        let new_highscore : highscore =
          {
            name = game.name;
            difficulty = game.difficulty;
            total_time = time_spent;
          }
        in
        let new_highscores_list = add_new_score new_highscore in
        save_config { highscores = new_highscores_list; games = new_games_list };
        delete_game game;
        Ok ()
end
