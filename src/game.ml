open Board
open Hint
open Core

module Sudoku_game = struct
  (** Fixed cell is used when the user attempts to change a cell that is fixed. Already present is used when the user's move would make a row/column/3x3 square have a duplicate entry *)
  type error_states = Fixed_cell | Already_present | Invalid_position

  type move = { x : int; y : int; value : int option }

  type hint =
    | Incorrect_cell
    | Suggest_guess of (move * string)
    | Suggested_move of (move * Hint_system.forced_source)
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

  let make_guess_suggestion possibs : hint =
    let make_guess_desc (possib_guesses : int list) : string =
      "This cell can contain any of the following values: "
      ^ List.to_string possib_guesses ~f:Int.to_string
      ^ ". Try one and if you find a mistake later, come back and try again. "
    in
    let best_row, best_col = Hint_system.get_best_guess possibs in
    let possib_guesses =
      Hint_system.get possibs best_row best_col |> Option.value_exn
    in
    let desc = make_guess_desc possib_guesses in
    let new_move = { x = best_row; y = best_col; value = None } in
    Suggest_guess (new_move, desc)

  (* probably can delete this function, no longer used 
  
  let make_full_desc (desc: string ) (elem : int) (x : int) (y : int)
       (removed : int list) : string =
     let removed =
       if List.length removed = 0 then "This "
       else
         List.to_string removed ~f:Int.to_string
         ^ " can't appear in this cell because they were required in other \
             cells. Therefore, this"
     in
     let intro = "cell must be " ^ Int.to_string elem ^ " because " in
     let desc =
       match desc with
       | "singleton" -> "it is the only possible value for this cell"
       | "row" ->
           "it is the only possible appearance of " ^ Int.to_string elem
           ^ " in row "
           ^ Int.to_string (x + 1)
       | "col" ->
           "it is the only possible appearance of " ^ Int.to_string elem
           ^ " in column "
           ^ Int.to_string (y + 1)
       | "block" ->
           "it is the only possible appearance of " ^ Int.to_string elem
           ^ " in its 3x3 block"
       | other -> other
     in
     removed ^ intro ^ desc *)

  let describe_hint (generated_hint : hint) : string =
    match generated_hint with
    | Incorrect_cell ->
        "Puzzle no longer has a unique solution. There is an incorrect cell \
         somewhere"
    | Already_solved -> "The puzzle is already solved!"
    | Suggest_guess (move, desc) ->
        let x = move.x + 1 |> Int.to_string in
        let y = move.y + 1 |> Int.to_string in
        "No suggested move is present. Try to guess at row " ^ x ^ " col " ^ y
        ^ "\n\n" ^ desc
    | Suggested_move (move, forced_by) ->
        let elem = move.value |> Option.value_exn |> Int.to_string in
        let x = move.x + 1 |> Int.to_string in
        let y = move.y + 1 |> Int.to_string in
        let intro =
          if equal_string elem "-1" then
            "Mistake made at row " ^ x ^ " col " ^ y ^ " because "
          else
            "Suggested move: Add value " ^ elem ^ " to row " ^ x ^ " col " ^ y
            ^ " because "
        in
        let desc =
          match forced_by with
          | Hint_system.Single -> "it is the only possible value for this cell"
          | Hint_system.Row ->
              "it is the only possible appearance of " ^ elem ^ " in row "
              ^ Int.to_string (move.x + 1)
          | Hint_system.Col ->
              "it is the only possible appearance of " ^ elem ^ " in column "
              ^ Int.to_string (move.y + 1)
          | Hint_system.Block ->
              "it is the only possible appearance of " ^ elem
              ^ " in its 3x3 block"
          | Hint_system.Incorrect ->
              "this cell is forced to contain two different values. There is \
               an incorrect cell somewhere"
        in
        intro ^ desc

  let generate_hint ?(use_crooks : bool option) (board : Sudoku_board.t) : hint
      =
    match Sudoku_board.solve_with_unique_solution board with
    | None ->
        Incorrect_cell
        (* if no solution, the user must have made a mistake, assuming the initial board was solvable *)
    | Some _ ->
        if Sudoku_board.is_solved board then Already_solved
        else
          let possibile_moves = Hint_system.make_possibility_sets board in
          let forced_moves : (int * int * int * Hint_system.forced_source) list
              =
            Hint_system.get_forced_moves possibile_moves
          in
          if List.length forced_moves = 0 then
            match use_crooks with
            | None | Some false -> make_guess_suggestion possibile_moves
            | Some true ->
                let updated_possibs = Hint_system.crooks possibile_moves in
                let new_forced_moves =
                  Hint_system.get_forced_moves updated_possibs
                in
                (* let _ = print_endline ("new forced moves: " ^ (List.to_string new_forced_moves ~f:(fun (x, y, elem, _) ->
                    "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ", " ^ (string_of_int elem) ^ ")"))) in *)
                if List.length new_forced_moves = 0 then
                  match Sudoku_board.solve_with_unique_solution board with
                  | None -> Incorrect_cell
                  | Some _ -> make_guess_suggestion possibile_moves
                  (* if still no forced moves after using crooks suggest guess *)
                else
                  let x, y, elem, forced_by =
                    List.nth_exn new_forced_moves
                      (List.length new_forced_moves |> Random.int)
                  in
                  if elem = -1 then Incorrect_cell
                  else
                    let next_move : move = { x; y; value = Some elem } in
                    let original_moves =
                      Hint_system.get possibile_moves x y |> Option.value_exn
                    in
                    let after_removal =
                      Hint_system.get updated_possibs x y |> Option.value_exn
                    in
                    let _ =
                      List.filter original_moves ~f:(fun x ->
                          not (List.mem after_removal x ~equal:Int.equal))
                    in
                    (* let full_desc = make_full_desc desc elem x y removed in *)
                    Suggested_move (next_move, forced_by)
          else
            let x, y, elem, forced_by =
              List.nth_exn forced_moves (List.length forced_moves |> Random.int)
            in
            let next_move : move = { x; y; value = Some elem } in
            Suggested_move (next_move, forced_by)
end