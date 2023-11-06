

module Sudoku_board = sig 
  type element = Option int
  type t [@@ deriving equal]


  (* Convenience methods *)
  val get x y: element
  

end 