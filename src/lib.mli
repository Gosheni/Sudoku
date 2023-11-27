module Sudoku_board : sig
  type element = Empty | Fixed of int | Volatile of int [@@deriving equal]

  type t

  val equal : t -> t -> bool
  (** Compares two sudoku boards, returns true iff the two boards has the same dimension and all the elements in are equal *)

  val empty : t
  (** First index is the row *)

  val get : t -> int -> int -> element option
  (** Given a board and a position, returns the element at that position, 
      otherwise None is returned if the board is invalid or an invalid position was given *)
  val set : t -> int -> int -> element -> t
  (** Changes the value at a given coordinate to the given given element. Enforces that the given board is valid. *)
  val set_forced : t -> int -> int -> element -> t
  (** Identical to set except set_forced does not enforce is_valid precondition. Used for creating invalid test boards *)

  val get_all : t -> element list 
  val get_row : t -> int -> element list
  (** Returns the row at the given index. Enforces that the given index and board are valid *)
  val get_col : t -> int -> element list
  (** Returns the column at the given index. Enforces that the given index and board are valid *)
  val get_block : t -> int -> element list
  (** Returns the 3x3 block at the given index. Enforces that the given index and board are valid *)
  
  val is_valid : ?updated:(int * int) -> t -> bool
  (** Checks that the board does not violate any sudoku rules, but could have empty values *)
  val is_solved : t -> bool
  (** Checks that the board is solved i.e. does not violate any sudoku rules and all cells are non-empty *)

  val generate_random : unit -> t
  (** Generate a random fully solved sudoku board *)
  val generate_degenerate : t -> int -> t
  (** Takes a fully solved sudoko and removes elements randomly while making sure the resulting board is still solvable. 
     Also takes an int that determines how many elements will be removed. This method expects a fully solved sudoku *)

  val seed_to_list : int -> int list
  (** Returns a list containing the numbers 1 through 9 exactly once. The order of the elements depend on the seed 
      This is a helper function for backtracking that is exposed to allow testing *)

  val solve_with_backtracking : t -> int -> (?updated:(int * int)  -> t -> bool) -> t option
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
  (** Interface for the game *)
  type error_states =
    | Fixed_cell
    | Already_present
    | Invalid_position
        (** Fixed cell is used when the user attempts to change a cell that is fixed. Already present is used when the user's move would make a row/column/3x3 square have a duplicate entry *)

  type move = { x : int; y : int; value : int option }

  type hint =
    | Incorrect_cell of (int * int)
    | Suggested_move of move
    | Already_solved  (** The kinds of hints allowed *)

  val do_move : Sudoku_board.t -> move -> (Sudoku_board.t, error_states) result
  (** Fails if attempting to change a fixed cell or the user makes a blatantly invalid move, like adding a 2 to a row that already contains a 2. If the move succeeds the updated board will be returned *)

  val generate_hint : Sudoku_board.t -> hint
  (** Generate a hint for the user given the current board state *)
end
