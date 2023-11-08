[@@@ocaml.warning "-27"]

open Core 
module Sudoku_board  = struct
  type element =
    | Empty
    | Fixed of int
    | Volatile of int  (** Contains the board state including which *)
  type row = (int, element, Int.comparator_witness) Map.t
  type t = (int, row , Int.comparator_witness) Map.t
  type difficulty = Difficulty of int 

  let element_to_string = function 
  | Empty -> " "
  | Fixed a | Volatile a -> Int.to_string a

  let get (board: t) (x: int) (y: int): element option = 
    let open Option.Let_syntax in
    Map.find board x 
    >>= Fn.flip Map.find y 
  
  let fold_row ((acc, seen) : bool * int list) (elem : element) = 
    match elem with
    | Empty -> false, seen
    | Fixed a -> acc && true, a::seen
    | Volatile a -> acc && true, a::seen

  let is_solved (board: t): bool = 
    if Map.keys board |> List.equal equal_int (List.range 0 9) |> not then (* check row keys are 0-8 *)
      false (* if not, return false (invalid board) *)
    else
      let is_solved_row (row : row) = 
        if Map.keys row |> List.equal equal_int (List.range 0 9) |> not then (* check col keys are 0-8*) 
          false
        else
          let row_data = Map.data row in
          let filled, seen = List.fold row_data ~init:(true, []) ~f:fold_row in
          if filled then 
            seen |> List.sort ~compare:compare_int |> List.equal equal_int (List.range 0 9)
        else 
          false
      in 
      let rec loop_rows (x : int) (acc : bool) = 
        if x >= 9 then acc else (* iterate rows 1 through *)
        Map.find_exn board x (* we already checked keys so find_exn should be fine *)
        |> is_solved_row
        |> (fun valid_row -> loop_rows (x + 1) (acc && valid_row))
      in
        loop_rows 0 true
        

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

  let generate_hint (board: Sudoku_board.t): hint = failwith "Not implented"
end
