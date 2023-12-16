module Hint_system : sig
  module S_element : sig
    type element = int list
    (** sudoku grid now stores list of possible moves at each square, called a markup of a sudoku puzzle *)
  end

  include Grid.Sudoku_grid with type element = S_element.element

  val make_possibility_sets : Board.Sudoku_board.t -> t
  (** Transform a sudoku board into a markup by finding all possible moves at every space on the board *)

  type forced_source = Row | Col | Block | Single | Incorrect
  (** hints are forced moves from either a certain section, being the only option or having a mistake *)

  val forced_source_to_string : forced_source -> string
  (** function to convert forced_source to string *)

  val get_forced_moves : t -> (coordinate * int * forced_source) list
  (** Takes a markup and returns a list of moves that are forced by the sudoku rules. Each returned move
        is of the form row, col, element, description.
        Description is a string that tells us what exactly forced this move *)

  val crooks : t -> t
  (** Apply crook's algorithm (see https://www.ams.org/notices/200904/rtx090400460p.pdf) to eliminate
        more possibilities from the markup. This lets us run forced_moves again and get more information 
        from the board without guessing *)
  
  val get_best_guess : t -> coordinate
  (** Take a markup and return the coordinates of the cell with the fewest possible entries *)
end
