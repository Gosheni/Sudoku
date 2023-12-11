val save_board_to_json : string -> Board.Sudoku_board.t -> unit
val load_board_from_json : string -> Board.Sudoku_board.t option
module Configuration :
  sig
    type highscore = {
      title : string;
      difficulty : int;
      total_time : float;
    }
    
    val equal_highscore : highscore -> highscore -> bool
    type game = {
      title : string;
      file_location : string;
      start_time : float;
      difficulty : int;
    }
    
    val equal_game : game -> game -> bool
    type t = { highscores : highscore list; games : game list; }
    val equal : t -> t -> bool
    val update: t -> unit
    val add_game: string -> int -> Board.Sudoku_board.t -> unit 
    val get_game : string -> Board.Sudoku_board.t option
    val update_game: string -> Board.Sudoku_board.t  -> unit
    val finish_game : string -> (unit, string) result
  end
