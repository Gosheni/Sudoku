open Core
open Board

let save_board_to_json filename data =
  Sudoku_board.serialize data |> Yojson.Safe.to_file filename

let load_board_from_json filename : Sudoku_board.t option =
  try Yojson.Safe.from_file filename |> Sudoku_board.deserialize
  with _ -> None

module Configuration = struct
  type highscore = { title : string; difficulty : int; total_time : float }
  [@@deriving equal, yojson]

  type game = {
    title : string;
    file_location : string;
    start_time : float;
    difficulty : int;
  }
  [@@deriving equal, yojson]

  type t = { highscores : highscore list; games : game list }
  [@@deriving equal, yojson]

  let empty = { highscores = []; games = [] }
  let location = "sudoku.config"

  let load_config _ : t =
    try
      let possible_config = Yojson.Safe.from_file location |> of_yojson in
      match possible_config with Ok a -> a | _ -> empty
    with _ -> empty

  let save_config (config : t) : unit =
    to_yojson config |> Yojson.Safe.to_file location

  let update = save_config

  let finish_game (title : string) : (unit, string) result =
    let config = load_config () in
    match
      List.find config.games ~f:(fun game -> String.(game.title = title))
    with
    | None -> Error "This game does not exist"
    | Some game ->
        let time_spent = Float.(Core_unix.time () - game.start_time) in
        let new_games_list =
          List.filter config.games ~f:(fun game -> String.(game.title <> title))
        in
        let new_highscore : highscore =
          {
            title = game.title;
            difficulty = game.difficulty;
            total_time = time_spent;
          }
        in
        let new_highscores_list = new_highscore :: config.highscores in
        save_config { highscores = new_highscores_list; games = new_games_list };
        Ok ()
end
