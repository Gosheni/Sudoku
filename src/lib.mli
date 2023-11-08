module Sudoku_board : sig
  type element =
    | Empty
    | Fixed of int
    | Volatile of int  (** Contains the board state including which *)

  type t
  type difficulty = int 

  val empty: t
  (* Convenience methods *)
  (** First index is the row *)
  val get : t -> int -> int -> element option
  val is_solved : t -> bool

  val generate_random : unit -> t
  (** Takes a fully solved sudoko. This method expects a fully solved sudoku *)
  val generate_degenerate: t -> difficulty -> t 

  (** *)
  val solve: t -> t option 

  (** Generates a solved sudoko with all the cells filled *)

  type json = [
    | `Assoc of (string * json) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `List of json list
    | `Null
    | `String of string
  ]

  val de_serialize : string -> json option
  val serialize : json -> string
  val pretty_print: t -> string
end

module Sudoku_game : sig
  (** Fixed cell is used when the user attempts to change a cell that is fixed. Already present is used when the user's move would make a row/column/3x3 square have a duplicate entry *)
  type error_states = Fixed_cell | Already_present
  type move = { x : int; y : int; value : int option }

  type hint =
    | Incorrect_cell of (int * int)
    | Suggested_move of move
    | Alread_solved

  val do_move : Sudoku_board.t -> move -> (Sudoku_board.t, error_states) result
  (** Fails if attempting to change a fixed cell or the user makes a blatantly invalid move, like adding a 2 to a row that already contains a 2. If the move succeeds the updated board will be returned *)

  val generate_hint : Sudoku_board.t -> hint
end


(* Use monads *)
