type errorMessage = { title : string; message : string } [@@deriving yojson]

module Configuration : sig
  type highscore = {
    username : string option;
    id : string;
    difficulty : int;
    total_time : float;
  }
  [@@deriving yojson]
  (** A type that represents a highscore, including a username, the game's id, 
      the game's difficulty and how much time was spent on solving the puzzle *)

  type highscore_list = highscore list [@@deriving yojson]
  (** A type that represents a list of highscore, required for synthesizing json *)


  type game
  (** A type that represents the metadata of a game, including its id and filelocation *)
  
  val load_board_from_json : game -> Board.Sudoku_board.t option
  (** Loads a sudoku board from file using its metadata *)

  val add_game : string -> int -> Board.Sudoku_board.t -> game option
  (** Saves a sudoku game including the game's name/id and difficulty *)
  val get_game : string -> (game * Board.Sudoku_board.t) option
  (** Retrieves a sudoku board and its metadata from the games name/id *)
  val get_most_recent : unit -> (game * Board.Sudoku_board.t) option
  (** Retrieves the most recently created sudoku board and its metadata *)
  val get_highscores : unit -> (highscore * highscore list) option 
  (** Retrieves the most recent highscore and the top 10 *)
  val update_name_for_highscore : string -> string -> unit
  (** Replaces the temporary id associated with a highscore  *)
  val move_game_to_first : string -> Board.Sudoku_board.t option
  (** Make a game the most recent *)
  val update_game : game -> Board.Sudoku_board.t -> unit
  (** Make a game the most recent *)
  val finish_game : game -> bool -> (unit, string) result
  (** Marks a game as completed and optionally saves the highscore *)
  val get_all_names: unit -> string list
  (** Returns a list of all the ongoing games *)
end
