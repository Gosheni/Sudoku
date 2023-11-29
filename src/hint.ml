open Core

module Hint_system = struct
  module S_element = struct
    type element = int list
    [@@deriving yojson, equal]

    let element_is_valid (element : element) : bool =
      List.for_all element ~f:(fun x -> x >= 1 && x <= 9)
      && (List.contains_dup element ~compare:Int.compare |> not)

    let empty_element = []

    let element_to_string = List.to_string ~f:Int.to_string 
  end

  include Grid.Make_sudoku_grid (S_element)

  let is_valid (possibs : t) : bool =
    let is_valid_lst (lst : element list) : bool =
      List.for_all lst ~f:(S_element.element_is_valid)
    in
    let check_section (get_section : int -> element list) : bool =
      List.range 0 9
      |> List.for_all ~f:(fun i -> i |> get_section |> is_valid_lst) 
    in
    check_keys possibs
    && (check_section @@ get_row possibs)
    && (check_section @@ get_col possibs)
    && (check_section @@ get_block possibs)

  let make_possibility_sets (board : Board.Sudoku_board.t) : t = 
    assert (Board.Sudoku_board.is_valid board);
    let filter_by_section (section : Board.Sudoku_board.element list) (possibilities : int list) : int list = 
      let seen = List.filter_map section ~f:(function
        | Empty -> None
        | Fixed a | Volatile a -> Some a) in
      List.filter possibilities ~f:(fun x -> not (List.mem seen x ~equal:Int.equal))
    in
    let check_all_sections (row_idx : int) (col_idx : int) = 
      let block_idx = (row_idx / 3) * 3 + (col_idx / 3) in
      List.init 9 ~f:(fun x -> x + 1)
      |> filter_by_section (Board.Sudoku_board.get_row board row_idx)
      |> filter_by_section (Board.Sudoku_board.get_col board col_idx)
      |> filter_by_section (Board.Sudoku_board.get_block board block_idx)
    in
    let rec make_possib_board (possibs : t) (row_idx : int) (col_idx : int) : t = 
      if col_idx > 8 then possibs else
      if row_idx > 8 then make_possib_board possibs 0 (col_idx + 1) else
      let possib_list = check_all_sections row_idx col_idx in
      let new_possibs = set possibs row_idx col_idx possib_list in
      make_possib_board new_possibs (row_idx + 1) col_idx
    in
    make_possib_board (empty) 0 0

  let get_forced_moves (possib : t) : (int * int * int) list =
    assert ((is_valid possib));
    Map.fold possib ~init:[] ~f:(fun ~key:row_idx ~data:row acc ->
      Map.fold row ~init:acc ~f:(fun ~key:col_idx ~data:elem acc ->
        match elem with
        | [] -> acc
        | lst -> 
            (* check if more than one element in the section could possibly be x *)
            let already_present (x : int)  (section : element list): bool = 
              List.count section ~f:(fun ls -> List.mem ls x ~equal:Int.equal) > 1 in
            (* check if possibilities are unique in the given row, col and block *)
            let unique_in_row = List.filter lst 
                            ~f:(fun x -> already_present x (get_row possib row_idx) |> not) in
            let unique_in_col = List.filter lst 
                            ~f:(fun x -> already_present x (get_col possib col_idx) |> not) in
            let block_idx = (row_idx / 3) * 3 + (col_idx / 3) in
            let unique_in_block = List.filter lst 
                            ~f:(fun x -> already_present x (get_block possib block_idx) |> not) in
            let all_unique = unique_in_row @ unique_in_col @ unique_in_block 
                             |> List.fold ~init:[] ~f:(fun acc x -> if List.mem acc x ~equal:Int.equal 
                                                                    then acc else x::acc) 
                                (* fold keeps only unique elements, since two sections could tell us same info *)
            in
            if List.length all_unique = 1 then 
              ((row_idx, col_idx, List.hd_exn all_unique)::acc)
            else if List.length all_unique > 1 then (* multiple unique elements in same cell which is impossible *)
              (row_idx, col_idx, -1)::acc (* -1 means an error exists in the current block, which is still a kind of hint*)
            else acc
        )
    )
  end