open Core

module Hint_system = struct
  module S_element = struct
    type element = int list [@@deriving yojson, equal]

    let element_is_valid (element : element) : bool =
      List.for_all element ~f:(fun x -> x >= 1 && x <= 9)
      && List.contains_dup element ~compare:Int.compare |> not

    let empty_element = []
    let element_to_string = List.to_string ~f:Int.to_string
  end

  include Grid.Make_sudoku_grid (S_element)

  let is_valid (possibs : t) : bool =
    let is_valid_lst (lst : element list) : bool =
      List.for_all lst ~f:S_element.element_is_valid
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
    let filter_by_section (section : Board.Sudoku_board.element list)
        (possibilities : int list) : int list =
      let seen =
        List.filter_map section ~f:(function
          | Empty -> None
          | Fixed a | Volatile a -> Some a)
      in
      List.filter possibilities ~f:(fun x ->
          not (List.mem seen x ~equal:Int.equal))
    in
    let check_all_sections (row_idx : int) (col_idx : int) =
      let block_idx = (row_idx / 3 * 3) + (col_idx / 3) in
      List.init 9 ~f:(fun x -> x + 1)
      |> filter_by_section (Board.Sudoku_board.get_row board row_idx)
      |> filter_by_section (Board.Sudoku_board.get_col board col_idx)
      |> filter_by_section (Board.Sudoku_board.get_block board block_idx)
    in
    let rec make_possib_board (possibs : t) (row_idx : int) (col_idx : int) : t
        =
      if col_idx > 8 then possibs
      else if row_idx > 8 then make_possib_board possibs 0 (col_idx + 1)
      else
        match Board.Sudoku_board.get board row_idx col_idx with
        | Some (Volatile _) | Some (Fixed _) ->
            let new_possibs = set possibs row_idx col_idx [] in
            make_possib_board new_possibs (row_idx + 1) col_idx
        | Some Empty ->
            let new_possibs =
              set possibs row_idx col_idx (check_all_sections row_idx col_idx)
            in
            make_possib_board new_possibs (row_idx + 1) col_idx
        | _ -> assert false
    in
    make_possib_board empty 0 0

  let union l1 l2 =
    List.fold l2 ~init:l1 ~f:(fun acc x ->
        if List.mem acc x ~equal:Int.equal then acc else x :: acc)

  type forced_source = Row | Col | Block | Single | Incorrect

  let get_forced_moves (possib : t) : (int * int * int * forced_source) list =
    assert (is_valid possib);
    Map.fold possib ~init:[] ~f:(fun ~key:row_idx ~data:row acc ->
        Map.fold row ~init:acc ~f:(fun ~key:col_idx ~data:elem acc ->
            match elem with
            | [] -> acc
            | [ single_move ] -> (row_idx, col_idx, single_move, Single) :: acc
            | lst ->
                (* check if more than one element in the section could possibly be x *)
                let already_present (x : int) (section : element list) : bool =
                  List.count section ~f:(fun ls ->
                      List.mem ls x ~equal:Int.equal)
                  > 1
                in
                (* check if possibilities are unique in the given row, col and block *)
                let unique_in_row =
                  List.filter lst ~f:(fun x ->
                      already_present x (get_row possib row_idx) |> not)
                in
                let unique_in_col =
                  List.filter lst ~f:(fun x ->
                      already_present x (get_col possib col_idx) |> not)
                in
                let block_idx = (row_idx / 3 * 3) + (col_idx / 3) in
                let unique_in_block =
                  List.filter lst ~f:(fun x ->
                      already_present x (get_block possib block_idx) |> not)
                in
                let all_unique =
                  unique_in_row |> union unique_in_col |> union unique_in_block
                  (* keeps only unique elements, since two sections could tell us same info *)
                in
                if List.length all_unique = 1 then
                  let forced_elem = List.hd_exn all_unique in
                  let forced_by =
                    if List.mem unique_in_row forced_elem ~equal:Int.equal then
                      Row
                    else if List.mem unique_in_col forced_elem ~equal:Int.equal
                    then Col
                    else Block
                  in
                  (* TODO: handle case where forced by multiple sections *)
                  (row_idx, col_idx, forced_elem, forced_by) :: acc
                else if List.length all_unique > 1 then
                  (* multiple unique elements in same cell which is impossible *)
                  (row_idx, col_idx, -1, Incorrect) :: acc
                  (* -1 means an error exists in the current block, which is still a kind of hint*)
                else acc))

  type preemptive = { possibs : element; members : int list }

  let find_preemptive_sets (section : element list) : preemptive list =
    assert (List.length section = 9);
    let preemptive_condition num_cells ls = List.length ls = num_cells in
    let rec get_combination_indices (size : int) (lst : int list) :
        int list list =
      if size = 0 then [ [] ]
      else
        match lst with
        | [] -> []
        | hd :: tl ->
            List.map
              ~f:(fun x -> hd :: x)
              (get_combination_indices (size - 1) tl)
            @ get_combination_indices size tl
      (* either keep curr element and decrease size_limit or keep going *)
    in
    let check_preemptive_of_size (size : int) =
      List.init 9 ~f:Fn.id
      |> List.filter ~f:(fun x -> List.nth_exn section x |> List.length > 1)
         (* only non-empty, non-forced cells*)
      |> get_combination_indices size
      |> List.filter_map ~f:(fun idxs ->
             let elem_set =
               List.map idxs ~f:(fun idx -> List.nth_exn section idx)
               |> List.fold ~init:[] ~f:(fun acc x -> union acc x)
             in
             (* check union of selected cells *)
             if preemptive_condition size elem_set then
               (* check preemptive condition *)
               Some { possibs = elem_set; members = idxs }
             else None)
    in
    let rec loop_sizes min max acc =
      if min > max then acc
      else loop_sizes (min + 1) max (acc @ check_preemptive_of_size min)
    in
    loop_sizes 2 8 []
  (* only check certain sizes of preemptive sets to save time, for now checking all of them *)

  let rec use_preemptive_sets (section : element list)
      (preSet : preemptive list) =
    match preSet with
    | [] -> section
    | curr :: tl ->
        let idxs = curr.members in
        let elems = curr.possibs in
        let new_section =
          List.mapi section ~f:(fun idx elem ->
              if List.mem idxs idx ~equal:Int.equal then elem
              else
                (* don't care if current item is in preSet *)
                List.filter elem ~f:(fun x ->
                    List.mem elems x ~equal:Int.equal |> not)
                (* remove all elements in preSet from current cell *))
        in
        use_preemptive_sets new_section tl

  let update_row (possibs : t) (new_section : element list) (row_idx : int) : t
      =
    Map.mapi possibs ~f:(fun ~key:curr_row ~data:row ->
      if row_idx <> curr_row then row else
      Map.mapi row ~f:(fun ~key:curr_col ~data:_ ->
        List.nth_exn new_section curr_col))

  let update_col (possibs : t) (new_section : element list) (col_idx : int) : t
      =
    Map.mapi possibs ~f:(fun ~key:curr_row ~data:row ->
      Map.mapi row ~f:(fun ~key:curr_col ~data:elem ->
        if col_idx <> curr_col then elem
        else
        List.nth_exn new_section curr_row))

  let update_block (possibs : t) (new_section : element list) (block_idx : int)
      : t =
    let row_idx = block_idx / 3 * 3 in
    let col_idx = block_idx % 3 * 3 in
    Map.mapi possibs ~f:(fun ~key:curr_row ~data:row ->
      Map.mapi row ~f:(fun ~key:curr_col ~data:elem ->
        if curr_row < row_idx || curr_row >= row_idx + 3 then elem
        else if curr_col < col_idx || curr_col >= col_idx + 3 then elem
        else
        List.nth_exn new_section (curr_col - col_idx + (curr_row - row_idx) * 3)))

  let crooks_on_section (possibs : t) (get_section : int -> element list)
      (update_section : t -> element list -> int -> t) : t =
    let rec crooks_helper (idx : int) acc =
      if idx > 8 then acc
      else
        let section = get_section idx in
        let preSet = find_preemptive_sets section in
        let new_section = use_preemptive_sets section preSet in
        let new_possibs = update_section acc new_section idx in
        crooks_helper (idx + 1) new_possibs
    in
    crooks_helper 0 possibs

  let crooks (possibs : t) : t =
    let crooks_one_round curr =
      ( crooks_on_section curr (get_row curr) update_row |> fun curr ->
        crooks_on_section curr (get_col curr) update_col )
      |> fun curr -> crooks_on_section curr (get_block curr) update_block
    in
    let rec crooks_till_unchanged curr =
      let new_possibs = crooks_one_round curr in
      if equal curr new_possibs then curr else crooks_till_unchanged new_possibs
    in
    crooks_till_unchanged possibs

  (* best guess is just shortest length of possibilities *)
  let get_best_guess (possib : t) : int * int =
    let best_row, best_col, _ =
      (* initial length is just set to 10, which is guaranteed to be larger than any list of possibilities *)
      Map.fold possib ~init:(-1, -1, 10) ~f:(fun ~key:row_idx ~data:row acc ->
          Map.fold row ~init:acc
            ~f:(fun ~key:col_idx ~data:elem (i, j, best_len) ->
              match elem with
              | [] -> (i, j, best_len)
              | lst ->
                  if List.length lst < best_len then
                    (row_idx, col_idx, List.length lst)
                  else (i, j, best_len)))
    in
    (best_row, best_col)
end