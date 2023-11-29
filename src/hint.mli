module Hint_system : sig 
    module S_element: sig
        type element = int list
      end 
    include Grid.Sudoku_grid with type element = S_element.element

    val is_valid : t -> bool

    val make_possibility_sets : Board.Sudoku_board.t -> t

    val get_forced_moves : t -> (int * int * int) list
end