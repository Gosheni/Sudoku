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

  let get_all (board : t) : element list =
    Map.data board |> List.map ~f:Map.data |> List.join

  let check_keys (board : t) : bool =
    let map_has_keys_one_through_nine map =
      Map.keys map |> List.equal equal_int (List.range 0 9)
    in
    if map_has_keys_one_through_nine board |> not then
      (* check row keys are 0-8 *)
      false (* if not, return false (invalid board) *)
    else
      (* check col keys are 0-8*)
      List.range 0 9
      |> List.map ~f:(Map.find_exn board)
      |> List.for_all ~f:map_has_keys_one_through_nine

  let get_row (board : t) (x : int) : element list =
    assert (0 <= x && x <= 8 && check_keys board);
    Map.find_exn board x |> Map.data

  let get_col (board : t) (x : int) : element list =
    assert (0 <= x && x <= 8 && check_keys board);
    Map.map board ~f:(fun row -> Map.find_exn row x) |> Map.data

  (* assumes that sub-blocks correspond to ints in the following manner:
         1 2 3   4 5 6   7 8 9
       -------------------------
     1 |       |       |       |
     2 |   0   |   1   |   2   |
     3 |       |       |       |
       -------------------------
     4 |       |       |       |
     5 |   3   |   4   |   5   |
     6 |       |       |       |
       -------------------------
     7 |       |       |       |
     8 |   6   |   7   |   8   |
     9 |       |       |       |
       -------------------------
  *)
  let get_block (board : t) (x : int) : element list =
    assert (0 <= x && x <= 8 && check_keys board);
    let row_lower = x / 3 * 3 in
    let row_upper = row_lower + 2 in
    (* we will use inclusive bounds so plus 2 *)
    let col_lower = x mod 3 * 3 in
    let col_upper = col_lower + 2 in
    (* we will use inclusive bounds so plus 2 *)
    Map.fold_range_inclusive board ~min:row_lower ~max:row_upper ~init:[]
      ~f:(fun ~key:_ ~data:row acc ->
        acc
        @ (Map.subrange row ~lower_bound:(Incl col_lower)
             ~upper_bound:(Incl col_upper)
          |> Map.data))

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
    let check_section (get_section : int -> element list) : bool =
      List.range 0 9 |> List.map ~f:get_section |> List.for_all ~f:is_valid_lst
    in
    check_keys board
    && (check_section @@ get_row board)
    && (check_section @@ get_col board)
    && (check_section @@ get_block board)

  let is_solved (board : t) : bool =
    is_valid board
    && Map.exists board ~f:(fun row -> Map.exists row ~f:(equal_element Empty))
       |> not

  let update (board : t) (x : int) (y : int)
      (element : element option -> element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8);
    Map.update board x ~f:(fun row ->
        match row with
        | None -> assert false
        | Some row -> Map.update row y ~f:element)

  let set (board : t) (x : int) (y : int) (element : element) : t =
    assert (
      0 <= x && x <= 8 && 0 <= y && y <= 8 && element_is_valid element
      && is_valid board);
    update board x y (fun _ -> element)

  (* doesn't check if is_valid before setting, useful for creating boards for debugging *)
  let set_forced (board : t) (x : int) (y : int) (element : element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8 && element_is_valid element);
    update board x y (fun _ -> element)

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
              let next_board = set_forced board x y n in
              (* set_forced due to the incomming board being invalid *)
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
    if is_valid board then aux board all_empty [] else None

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

    let sample_from_list (ls : 'a list) : ('a * 'a list) option =
      let length = List.length ls in
      if length = 0 then None
      else
        let n = Random.int length in
        let element = List.nth_exn ls n in
        let updated_list = List.filteri ls ~f:(fun i _ -> i <> n) in
        Some (element, updated_list)
    in
    let coordinates =
      List.cartesian_product (List.range 0 9) (List.range 0 9)
    in

    let rec aux (board : t) (to_remove : int)
        (possible_coordinates : (int * int) list) : t =
      if to_remove = 0 then board
      else
        match sample_from_list possible_coordinates with
        | None -> board
        | Some ((row, col), remaining_coordinates) ->
            let new_board = set board row col Empty in
            if Option.is_some @@ solve_with_unique_solution new_board then
              aux new_board (to_remove - 1) remaining_coordinates
            else board
    in
    aux board difficulty coordinates

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

  let generate_hint (board : Sudoku_board.t) : hint = assert false
end
