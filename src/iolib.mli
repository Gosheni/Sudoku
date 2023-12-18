type errorMessage = { title : string; message : string } [@@deriving yojson]

module Configuration : sig
  type highscore = {
    username : string option;
    id : string;
    difficulty : int;
    total_time : float;
  }
  [@@deriving yojson]

  type highscore_list = highscore list [@@deriving yojson]

  type game

  val save_board_to_json : game -> Board.Sudoku_board.t -> unit
  val load_board_from_json : game -> Board.Sudoku_board.t option
  val add_game : string -> int -> Board.Sudoku_board.t -> game
  val get_game : string -> (game * Board.Sudoku_board.t) option
  val get_most_recent : unit -> (game * Board.Sudoku_board.t) option
  val get_highscores : unit -> highscore list
  val update_name_for_highscore : string -> string -> unit
  val move_game_to_first : string -> Board.Sudoku_board.t option
  val update_game : game -> Board.Sudoku_board.t -> unit
  val finish_game : game -> (unit, string) result
end
