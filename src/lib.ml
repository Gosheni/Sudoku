[@@@ocaml.warning "-27"]

open Core

module Sudoku_board = struct
  type element = Empty | Fixed of int | Volatile of int
  [@@deriving yojson, equal]

  type row = (int, element, Int.comparator_witness) Map.t
  type t = (int, row, Int.comparator_witness) Map.t

  let element_is_valid (element : element) : bool =
    match element with
    | Volatile a | Fixed a -> 1 <= a && a <= 9
    | Empty -> true

  let equal (b1 : t) (b2 : t) : bool =
    let equal_row = Map.equal equal_element in
    Map.equal equal_row b1 b2

  let get (board : t) (x : int) (y : int) : element option =
    let open Option.Let_syntax in
    Map.find board x >>= Fn.flip Map.find y

  let is_valid (board : t) : bool =
    let is_valid_lst (lst : element list) : bool =
      let seen =
        List.filter_map lst ~f:(function
          | Empty -> None
          | Fixed a | Volatile a -> Some a)
      in
      let no_dups = List.contains_dup seen ~compare:compare_int |> not in
      let no_invalid = List.for_all seen ~f:(fun x -> x >= 1 && x <= 9) in
      no_dups && no_invalid
    in

    let check_keys (board : t) : bool =
      (* check row keys are 0-8 *)
      if Map.keys board |> List.equal equal_int (List.range 0 9) |> not then
        false (* if not, return false (invalid board) *)
      else
        (* check col keys are 0-8*)
        Map.for_all board ~f:(fun row ->
            row |> Map.keys |> List.equal equal_int (List.range 0 9))
    in
    let check_rows (board : t) (func_to_check : element list -> bool) : bool =
      Map.for_all board ~f:(fun row -> row |> Map.data |> func_to_check)
    in
    let check_blocks (board : t) (func_to_check : element list -> bool) : bool =
      let check_block (block_num : int) =
        let row_lower = block_num / 3 * 3 in
        let row_upper = row_lower + 3 in
        let col_lower = block_num mod 3 * 3 in
        let col_upper = col_lower + 3 in
        let coordinates_to_check =
          List.cartesian_product
            (List.range row_lower row_upper)
            (List.range col_lower col_upper)
        in
        let curr_block =
          List.map coordinates_to_check ~f:(fun (x, y) ->
              match get board x y with
              | None -> assert false
              | Some element -> element)
        in
        func_to_check curr_block
      in
      List.range 0 9 |> List.for_all ~f:check_block
    in

    check_keys board
    && check_rows board is_valid_lst
    && check_rows (Map.transpose_keys (module Int) board) is_valid_lst
    && check_blocks board is_valid_lst

  let is_solved (board : t) : bool =
    if is_valid board then
      Map.exists board ~f:(fun row ->
          Map.exists row ~f:(equal_element Empty) |> not)
    else false

  let set (board : t) (x : int) (y : int) (element : element) : t =
    assert (
      0 <= x && x <= 8 && 0 <= y && y <= 8 && element_is_valid element
      && is_valid board);
    Map.update board x ~f:(fun row ->
        match row with
        | None -> assert false
        | Some row -> Map.update row y ~f:(fun _ -> element))

  (* doesn't check if is_valid before setting, useful for creating boards for debugging *)
  let set_forced (board : t) (x : int) (y : int) (element : element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8 && element_is_valid element);
    Map.update board x ~f:(fun row ->
        match row with
        | None -> assert false
        | Some row -> Map.update row y ~f:(fun _ -> element))

  let empty : t =
    let a = Map.empty (module Int) in
    let empty_row =
      List.init 9 ~f:(fun _ -> Empty)
      |> List.foldi ~init:a ~f:(fun index map element ->
             Map.add_exn map ~key:index ~data:element)
    in
    List.init 9 ~f:(fun _ -> empty_row)
    |> List.foldi ~init:a ~f:(fun index map element ->
           Map.add_exn map ~key:index ~data:element)

  let seed_to_list (seed : int) : int list =
    let rec aux (state : int list) (seed : int)
        (number_of_element_to_find : int) =
      if number_of_element_to_find = 0 then state
      else
        let new_num = abs (seed mod number_of_element_to_find) + 1 in
        let adjusted_new_num =
          List.fold (List.sort state ~compare:Int.compare) ~init:new_num
            ~f:(fun acc element -> if acc = element then acc + 1 else acc)
        in
        aux
          (adjusted_new_num :: state)
          (seed / number_of_element_to_find)
          (number_of_element_to_find - 1)
    in
    aux [] seed 9 |> List.rev

  let solve_with_backtracking (board : t) (seed : int) (validator : t -> bool) :
      t option =
    (* Optimizations Only check the validity of the block, row, and column that has been changed. *)
    let all_empty : (int * int) list =
      Map.to_alist board
      |> List.map ~f:(Tuple2.map_snd ~f:Map.to_alist)
      |> List.map ~f:(fun (row_num, row) ->
             List.filter_map row ~f:(fun (col_num, element) ->
                 if equal_element Empty element then Some (row_num, col_num)
                 else None))
      |> List.join
    in

    let order = seed_to_list seed in

    let first_guess = List.hd_exn order in

    let order_array : int option array =
      List.range 1 10
      |> List.map ~f:(fun a ->
             let open Option.Let_syntax in
             List.findi order ~f:(fun _ element -> element = a)
             >>| Tuple2.get1 >>| ( + ) 1 >>= List.nth order)
      |> List.to_array
    in
    let next (a : int) : element =
      match order_array.(a - 1) with None -> Empty | Some a -> Volatile a
    in

    let rec backtrack (board : t) (empty : (int * int) list)
        (added_to : (int * int) list) :
        (t * (int * int) list * (int * int) list) option =
      match added_to with
      | [] ->
          None (* Unable to backtrack anymore, i.e. the sudoku is unsolvable *)
      | (x, y) :: tl -> (
          let current = get board x y in
          match current with
          | Some (Volatile a) ->
              let n = next a in
              let next_board = set board x y n in
              if equal_element Empty n then
                backtrack next_board ((x, y) :: empty) tl
              else if validator next_board then
                Some (next_board, empty, added_to)
              else backtrack next_board empty added_to
          | _ -> assert false)
    in

    let rec aux (board : t) (empty : (int * int) list)
        (added_to : (int * int) list) : t option =
      match empty with
      | [] -> Some board
      | (x, y) :: tl -> (
          let new_board = set board x y @@ Volatile first_guess in
          if validator new_board then aux new_board tl ((x, y) :: added_to)
          else
            match backtrack new_board tl ((x, y) :: added_to) with
            | None -> None
            | Some (backtracked_board, new_empty, new_added_to) ->
                aux backtracked_board new_empty new_added_to)
    in

    aux board all_empty []

  let solve_with_unique_solution (board : t) : t option =
    match solve_with_backtracking board 0 is_valid with
    | None -> None
    | Some solution -> (
        let other_solution =
          solve_with_backtracking board 0 (fun board ->
              is_valid board && equal solution board |> not)
        in
        match other_solution with None -> Some solution | _ -> None)

  let solve (_ : t) : t option = None

  let generate_random _ : t =
    let seed = Random.int Int.max_value in
    match solve_with_backtracking empty seed is_valid with
    | None -> assert false (* Solving an empty sudoku always succeeds *)
    | Some board -> board

  let generate_degenerate (board : t) (difficulty : int) : t =
    assert (is_solved board && difficulty > 0);

    let rec aux (board : t) (to_remove : int) : t =
      if to_remove = 0 then board
      else
        let row = Random.int 9 in
        let col = Random.int 9 in
        let new_board = set board row col Empty in
        if Option.is_some @@ solve_with_unique_solution new_board then
          aux new_board (to_remove - 1)
        else board
    in
    aux board difficulty

  type json = Yojson.Safe.t

  let serialize (board : t) : json option =
    let convert_map_content_to_json value_map data =
      let open Option.Let_syntax in
      (if Map.is_empty data then None else Some data)
      >>| Map.to_alist
      >>| List.map ~f:(Tuple2.map_both ~f1:string_of_int ~f2:value_map)
      >>| fun a -> `Assoc a
    in

    board
    |> Map.map ~f:(convert_map_content_to_json element_to_yojson)
    |> Map.filter_map ~f:Fn.id
    |> convert_map_content_to_json Fn.id

  let deserialize (obj : json) : t option =
    let convert_to_map_if_possible obj ~f:filter_map =
      match obj with
      | `Assoc assoc ->
          List.filter_map assoc ~f:filter_map |> Map.of_alist_exn (module Int)
      | _ -> Map.empty (module Int)
    in
    let yojson_to_row obj =
      convert_to_map_if_possible obj ~f:(fun (key, value) ->
          match (int_of_string_opt key, element_of_yojson value) with
          | Some key_int, Ok element -> Some (key_int, element)
          | _ -> None)
    in
    try
      let board =
        convert_to_map_if_possible obj ~f:(fun (key, value) ->
            match (int_of_string_opt key, yojson_to_row value) with
            | Some key_int, row -> Some (key_int, row)
            | _ -> None)
      in
      Option.some_if (is_valid board) board
    with exn -> None

  let pretty_print (board : t) : string =
    let left_spacing : string = "  " in
    let element_to_string = function
      | Empty -> " "
      | Fixed a | Volatile a -> Int.to_string a
    in
    let pretty_print_row (row : row) : string =
      Map.fold row ~init:"" ~f:(fun ~key:col_num ~data:value accum ->
          let block = element_to_string value ^ " " in
          if col_num mod 3 = 0 then accum ^ "| " ^ block else accum ^ block)
      ^ "|"
    in

    let top_ruler : string = left_spacing ^ "  1 2 3   4 5 6   7 8 9\n" in
    let divider_line : string =
      left_spacing ^ String.init (4 + ((3 + 4) * 3)) ~f:(fun _ -> '-') ^ "\n"
    in

    top_ruler
    ^ Map.fold board ~init:"" ~f:(fun ~key:row_num ~data:row_data accum ->
          let row =
            string_of_int (row_num + 1) ^ " " ^ pretty_print_row row_data ^ "\n"
          in
          if row_num mod 3 = 0 then accum ^ divider_line ^ row else accum ^ row)
    ^ divider_line
end

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

  let generate_hint (board : Sudoku_board.t) : hint = failwith "Not implented"
end
