val save_board_to_json : string -> Board.Sudoku_board.t -> unit
val load_board_from_json : string -> Board.Sudoku_board.t option

module Configuration : sig
  type highscore = { name : string; difficulty : int; total_time : float }

  val equal_highscore : highscore -> highscore -> bool

  type game = {
    name : string;
    file_location : string;
    start_time : float;
    difficulty : int;
  }

  val equal_game : game -> game -> bool

  type t = { highscores : highscore list; games : game list }

  val equal : t -> t -> bool
  val load_config : unit -> t
  val save_config : t -> unit
  val update : t -> unit
  val add_game : string -> int -> Board.Sudoku_board.t -> unit
  val get_game : string -> Board.Sudoku_board.t option
  val get_name : unit -> string
  val move_game_to_first : string -> unit
  val update_game : string -> Board.Sudoku_board.t -> unit
  val finish_game : string -> (unit, string) result
end
