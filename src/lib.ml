[@@@ocaml.warning "-27"]

open Core 
module Sudoku_board  = struct
  type element =
    | Empty
    | Fixed of int
    | Volatile of int  (** Contains the board state including which *)
  type t = (int, (int, element, Int.comparator_witness) Map.t , Int.comparator_witness) Map.t
  type difficulty = int 


  let get (board: t) (x: int) (y: int): element option = 
    let open Option.Let_syntax in
    Map.find board x 
    >>= Fn.flip Map.find y 
    
  let is_solved (board: t): bool = false

  let generate_random _ = failwith "Not implemented"
  (** Takes a fully solved sudoko. This method expects a fully solved sudoku *)
  let generate_degenerate (board: t) (difficulty: difficulty): t = failwith "Not implemented"

  (** *)
  let solve (_: t): t option = None 

  (** Generates a solved sudoko with all the cells filled *)

  let de_serialize (str: string): t option = None
  let serialize (_: t): string = ""

end

module Sudoku_game = struct
  (** Fixed cell is used when the user attempts to change a cell that is fixed. Already present is used when the user's move would make a row/column/3x3 square have a duplicate entry *)
  type error_states = Fixed_cell | Already_present
  type move = { x : int; y : int; value : int option }

  type hint =
    | Incorrect_cell of (int * int)
    | Suggested_move of move
    | Alread_solved

  let do_move (board: Sudoku_board.t) (move: move): (Sudoku_board.t, error_states) result = failwith "Not implented"
  (** Fails if attempting to change a fixed cell or the user makes a blatantly invalid move, like adding a 2 to a row that already contains a 2. If the move succeeds the updated board will be returned *)

  let generate_hint (board: Sudoku_board.t): move option = failwith "Not implented"
end
