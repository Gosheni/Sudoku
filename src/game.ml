open Board
open Hint
open Core

module Sudoku_game = struct
  (** Fixed cell is used when the user attempts to change a cell that is fixed. Already present is used when the user's move would make a row/column/3x3 square have a duplicate entry *)
  type error_states = Fixed_cell | Already_present | Invalid_position

  type move = { x : int; y : int; value : int option }

  type hint =
    | Incorrect_cell
    | Suggest_guess
    | Suggested_move of (move * string)
    | Already_solved

  let do_move (board : Sudoku_board.t) (move : move) :
      (Sudoku_board.t, error_states) result =
    let open Sudoku_board in
    match (get board move.x move.y, move.value) with
    | None, _ ->
        assert false
        (* Either the board is not the expected 9 x 9 grid or an invalid position was used *)
    | Some (Fixed _), _ -> Error Fixed_cell
    | Some Empty, None -> Error Already_present
    | Some (Volatile element), Some move_value when element = move_value ->
        Error Already_present
    | Some (Volatile _), None ->
        Ok (set board move.x move.y @@ Empty)
        (* Removing something from a valid board cannot make it invalid *)
    | Some (Volatile _ | Empty), Some move_value ->
        let new_board = set board move.x move.y @@ Volatile move_value in
        if is_valid new_board then Ok new_board else Error Invalid_position

  let generate_hint ?(use_crooks : bool option) (board : Sudoku_board.t) : hint =
    if Sudoku_board.is_solved board then Already_solved else
      let possibile_moves = Hint_system.make_possibility_sets board in
      let forced_moves : (int * int * int * string) list = Hint_system.get_forced_moves possibile_moves in
      if List.length forced_moves = 0 then
        match use_crooks with
        | None | Some false -> Suggest_guess
        | Some true -> 
        let updated_possibs = Hint_system.crooks possibile_moves in
        let new_forced_moves = Hint_system.get_forced_moves updated_possibs in
        if List.length new_forced_moves = 0 then 
          match Sudoku_board.solve_with_unique_solution board with
            | None -> Incorrect_cell
            | Some _ -> Suggest_guess (* if still no forced moves after using crooks suggest guess *)
        else
          let x, y, elem, desc = List.nth_exn new_forced_moves (List.length new_forced_moves |> Random.int) in
          let next_move : move = {x=x; y=y; value=Some elem} in
          let original_moves = Hint_system.get possibile_moves x y |> Option.value_exn in 
          let after_removal = Hint_system.get updated_possibs x y |> Option.value_exn in 
          let removed = List.filter original_moves ~f:(fun x -> not (List.mem after_removal x ~equal:Int.equal)) in
          let full_desc = (List.to_string removed ~f:(fun x -> string_of_int x)) ^ 
                          " can't appear in this cell because they were required in other cells."^
                          "Therefore, this cell must be " ^ (string_of_int elem) ^ 
                          " because if we exclude the others, it is now the only possible appearence in its " ^ desc in
          Suggested_move (next_move, full_desc)
      else
      let x, y, elem, desc = List.nth_exn forced_moves (List.length forced_moves |> Random.int) in
      (* let _ = print_endline ("x: " ^ (string_of_int x) ^ " y: " ^ (string_of_int y) ^ " elem: " ^ (string_of_int elem)) in *)
      let next_move : move = {x=x; y=y; value=Some elem} in
      let full_desc = "This cell must be " ^ (string_of_int elem) ^ " because it is the only possible appearence in its " ^ desc in
      Suggested_move (next_move, full_desc)

end
