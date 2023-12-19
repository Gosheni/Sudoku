module Sudoku_board : sig
  module S_element: sig
    type element = Empty | Fixed of int | Volatile of int
  end 


  include Grid.Sudoku_grid with type element = S_element.element
  
  val is_valid : ?updated:coordinate -> t -> bool
  (** Checks that the board does not violate any sudoku rules, but could have empty values *)
  val is_solved : t -> bool
  (** Checks that the board is solved i.e. does not violate any sudoku rules and all cells are non-empty *)

  val generate_random : unit -> t
  (** Generate a random fully solved sudoku board *)
  val generate_unsolved : t -> int -> t
  (** Takes a fully solved sudoku and removes elements randomly while making sure the resulting board is still solvable. 
     Also takes an int that determines how many elements will be removed. This method expects a fully solved sudoku *)

  val seed_to_list : int -> int list
  (** Returns a list containing the numbers 1 through 9 exactly once. The order of the elements depend on the seed 
      This is a helper function for backtracking that is exposed to allow testing *)

  val solve_with_backtracking : t -> int -> (?updated:coordinate  -> t -> bool) -> t option
  (** Solves a sudoku with backtracking requiring the solution to be unique *)

  val solve_with_unique_solution : ?known_solution:t -> t -> t option
  (** Solves a sudoku with backtracking requiring the solution to be unique *)
  
  val pretty_print : t -> string
  (** prints out the board in a human readable format, for CLI *)
end
