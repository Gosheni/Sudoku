val save_board_to_json : string -> Board.Sudoku_board.t -> unit
val load_board_from_json : string -> Board.Sudoku_board.t option

(*
module Configuration : sig
  type t

  val equal : t -> t -> bool
  val update : t -> unit
  val finish_game : string -> (unit, string) result
end
*)