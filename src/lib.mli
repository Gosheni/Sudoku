module Sudoku_board : sig
  type element =
    | Empty
    | Fixed of int
    | Volatile of int  (** Contains the board state including which *)
  [@@deriving yojson]

  type row
  type t

  val equal : t -> t -> bool

  val empty : t
  (** First index is the row *)

  val get : t -> int -> int -> element option
  (** given a board and a position, return Some(element) at that position, 
                                           None if board is invalid or invalid position given *)
  val set : t -> int -> int -> element -> t
  (** given a board, a position and an element, set the entry on the board at that position to the element *)
  val set_forced : t -> int -> int -> element -> t
  (** identical to set except set_forced does not enforce is_valid precondition, used to make invalid test boards *)

  val is_valid : t -> bool
  (** checks that the board does not violate any sudoku rules, but could have empty values *)

  val is_solved : t -> bool
  (** checks that the board is solved i.e. does not violate any sudoku rules and all cells are non-empty *)

  val generate_random : unit -> t
  (** generate a random fully solved sudoku board *)

  val generate_degenerate : t -> int -> t
  (** Takes a fully solved sudoko and removes elements randomly while making sure the resulting board is still solvable. 
     Also takes an int that determines how many elements will be removed. This method expects a fully solved sudoku *)

  val seed_to_list : int -> int list
  (** Returns a list containing the numbers 1 through 9 exactly once. The order of the elements depend on the seed 
      This is a helper function for backtracking that is exposed to allow testing *)

  val solve_with_backtracking : t -> int -> (t -> bool) -> t option
  (** Solves a sudoku with backtracking requiring the solution to be unique *)

  val solve_with_unique_solution : t -> t option
  (** Solves a sudoku with backtracking requiring the solution to be unique *)

  val solve : t -> t option
  (** Solves a sudoku while requiring the solution to be unique *)

  type json = Yojson.Safe.t

  val serialize : t -> json option
  (** converts a board to json *)
  val deserialize : json -> t option
  (** loads a board from json *)
  

  val pretty_print : t -> string
  (** prints out the board in a human readable format, for CLI *)
end

module Sudoku_game : sig
  (** Fixed cell is used when the user attempts to change a cell that is fixed. Already present is used when the user's move would make a row/column/3x3 square have a duplicate entry *)
  type error_states = Fixed_cell | Already_present | Invalid_position

  type move = { x : int; y : int; value : int option }

  type hint =
    | Incorrect_cell of (int * int)
    | Suggested_move of move
    | Already_solved

  val do_move : Sudoku_board.t -> move -> (Sudoku_board.t, error_states) result
  (** Fails if attempting to change a fixed cell or the user makes a blatantly invalid move, like adding a 2 to a row that already contains a 2. If the move succeeds the updated board will be returned *)

  val generate_hint : Sudoku_board.t -> hint
  (** Generate a hint for the user given the current board state *)
end