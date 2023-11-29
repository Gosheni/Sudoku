open Board
open Hint
open Core

module Sudoku_game = struct
  (** Fixed cell is used when the user attempts to change a cell that is fixed. Already present is used when the user's move would make a row/column/3x3 square have a duplicate entry *)
  type error_states = Fixed_cell | Already_present | Invalid_position

  type move = { x : int; y : int; value : int option }

  type hint =
    | Incorrect_cell of (int * int)
    | Suggested_move of move
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

  let generate_hint (board : Sudoku_board.t) : hint =
    if Sudoku_board.is_solved board then Already_solved else
      let possibile_moves = Hint_system.make_possibility_sets board in
      let forced_moves : (int * int * int) list = Hint_system.get_forced_moves possibile_moves in
      (* let _ = List.to_string ~f:(fun (x, y, z) -> ("x: " ^ (string_of_int x) ^ " y: " ^ (string_of_int y) ^ " elem: " ^ (string_of_int z))) forced_moves 
              |> print_endline in *)
      if List.length forced_moves = 0 then failwith "Not yet implemented" (* Either need to guess or mistake was made (or preemptive sets) *)
      else
      let x, y, elem = List.nth_exn forced_moves (List.length forced_moves |> Random.int) in
      (* let _ = print_endline ("x: " ^ (string_of_int x) ^ " y: " ^ (string_of_int y) ^ " elem: " ^ (string_of_int elem)) in *)
      let next_move : move = {x=x; y=y; value=Some elem} in
      Suggested_move next_move

end
