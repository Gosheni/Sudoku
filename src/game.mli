module Sudoku_game : sig
  (** Interface for the game *)
  type error_states =
    | Fixed_cell
    | Already_present
    | Invalid_position
        (** Fixed cell is used when the user attempts to change a cell that is fixed. Already present is used when the user's move would make a row/column/3x3 square have a duplicate entry *)

  type move = { x : int; y : int; value : int option }

  type hint =
    | Incorrect_cell of (int * int)
    | Suggest_guess
    | Suggested_move of (move * string)
    | Already_solved  (** The kinds of hints allowed *)

  val do_move :
    Board.Sudoku_board.t -> move -> (Board.Sudoku_board.t, error_states) result
  (** Fails if attempting to change a fixed cell or the user makes a blatantly invalid move, like adding a 2 to a row that already contains a 2. If the move succeeds the updated board will be returned *)

  val generate_hint : ?use_crooks:bool -> Board.Sudoku_board.t -> hint
  (** Generate a hint for the user given the current board state *)
end
