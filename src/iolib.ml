open Board
open Core

type errorMessage = { title : string; message : string } [@@deriving yojson]

module Configuration = struct
  type highscore = {
    username : string option;
    id : string;
    difficulty : int;
    total_time : float;
  }
  [@@deriving yojson]

  type highscore_list = highscore list [@@deriving yojson]

  type game = {
    name : string;
    file_location : string;
    start_time : float;
    difficulty : int;
  }
  [@@deriving equal, yojson]

  type t = {
    highscores : highscore_list;
    most_recent_highscore : highscore option;
    games : game list;
  }
  [@@deriving yojson]

  let empty = { highscores = []; most_recent_highscore = None; games = [] }
  let location = "sudoku.config"

  let save_board_to_json game data =
    let filename = game.file_location in
    if String.(suffix filename 5 = ".json") && String.length filename > 5 then
      Sudoku_board.serialize data |> Yojson.Safe.to_file filename
    else failwith "Invalid filename"

  let delete_game_file game =
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
      game option =
    let config = load_config () in
    if
      config.games
      |> List.map ~f:(fun game -> game.name)
      |> List.map ~f:String.lowercase
      |> Fn.flip List.mem (String.lowercase name) ~equal:String.equal
    then None
    else
      let filename = (name |> String.filter ~f:Char.is_alphanum) ^ ".json" in
      let game =
        {
          name;
          file_location = filename;
          start_time = Core_unix.time ();
          difficulty;
        }
      in
      save_board_to_json game board;
      save_config { config with games = game :: config.games };
      Some game

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

  let update_name_for_highscore (id : string) (new_name : string) : unit =
    let config = load_config () in
    let find_condition highscore =
      String.(highscore.id = id && Option.is_none highscore.username)
    in

    (* Updates the most recent highscore *)
    let most_recent_highscore =
      match config.most_recent_highscore with
      | None -> None
      | Some score ->
          if String.(score.id = id) then
            Some { score with username = Some new_name }
          else Some score
    in
    match List.find config.highscores ~f:find_condition with
    | Some highscore when Option.is_none highscore.username ->
        (* Updates the toplist *)
        let new_highscore = { highscore with username = Some new_name } in
        let new_highscores =
          new_highscore
          :: List.filter config.highscores ~f:(fun highscore ->
                 highscore |> find_condition |> not)
        in
        save_config
          { config with highscores = new_highscores; most_recent_highscore }
    | _ -> save_config { config with most_recent_highscore }

  let get_highscores _ : (highscore * highscore list) option =
    let config = load_config () in
    config.most_recent_highscore
    |> Option.map ~f:(Fn.flip Tuple2.create config.highscores)

  (* keep only best 10 scores *)
  let add_new_score (score : highscore) : highscore list =
    let config = load_config () in
    List.sort (score :: config.highscores) ~compare:(fun a b ->
        Float.compare a.total_time b.total_time)
    |> Fn.flip List.take 10

  let move_game_to_first game_name : Sudoku_board.t option =
    match get_game_with_name game_name with
    | None -> None
    | Some game ->
        let config = load_config () in
        let new_games_list =
          List.filter config.games ~f:(fun g -> String.(g.name <> game_name))
        in
        save_config { config with games = game :: new_games_list };
        load_board_from_json game

  let finish_game (game : game) (save_highscore : bool) : (unit, string) result
      =
    let config = load_config () in
    match get_game_with_name game.name with
    | None -> Error "This game does not exist"
    | Some game ->
        let new_games_list =
          List.filter config.games ~f:(fun other_game ->
              String.(other_game.name <> game.name))
        in

        if save_highscore then
          let time_spent = Float.(Core_unix.time () - game.start_time) in
          let new_highscore : highscore =
            {
              id = game.name;
              username = None;
              difficulty = game.difficulty;
              total_time = time_spent;
            }
          in

          save_config
            {
              highscores = add_new_score new_highscore;
              most_recent_highscore = Some new_highscore;
              games = new_games_list;
            }
        else save_config { config with games = new_games_list };
        delete_game_file game;
        Ok ()

  let get_all_names _ : string list =
    let config = load_config () in
    List.map config.games ~f:(fun game -> game.name)
end
