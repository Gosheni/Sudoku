[@@@ocaml.warning "-27"]

open Core

module Sudoku_board = struct
  type element =
    | Empty
    | Fixed of int
    | Volatile of int  (** Contains the board state including which *)
  [@@deriving yojson, equal]

  type row = (int, element, Int.comparator_witness) Map.t
  type t = (int, row, Int.comparator_witness) Map.t
  type difficulty = int

  let element_to_string = function
    | Empty -> " "
    | Fixed a | Volatile a -> Int.to_string a

  let get (board : t) (x : int) (y : int) : element option =
    let open Option.Let_syntax in
    Map.find board x >>= Fn.flip Map.find y

  let check_keys (board : t) : bool =
    if Map.keys board |> List.equal equal_int (List.range 0 9) |> not then
      (* check row keys are 0-8 *)
      false (* if not, return false (invalid board) *)
    else
      let check_rows (row : row) =
        Map.keys row |> List.equal equal_int (List.range 0 9)
      in
      (* check col keys are 0-8*)
      let rec loop_rows (row_idx : int) acc =
        if row_idx >= 9 then acc
        else
          Map.find_exn board
            row_idx (* we already checked keys so find_exn should be fine *)
          |> check_rows
          |> fun valid_row -> loop_rows (row_idx + 1) (acc && valid_row)
      in
      loop_rows 0 true

  let check_rows (board : t) (func_to_check : element list -> bool) : bool =
    let check_row (row : row) = row |> Map.data |> func_to_check in
    let rec loop_rows (x : int) (acc : bool) =
      if x >= 9 then acc
      else
        (* iterate rows 1 through *)
        Map.find_exn board
          x (* we already checked keys so find_exn should be fine *)
        |> check_row
        |> fun valid_row -> loop_rows (x + 1) (acc && valid_row)
    in
    loop_rows 0 true

  let check_block (board : t) (func_to_check : element list -> bool) : bool =
    let check_block (block_num : int) =
      let row_lower = block_num / 3 * 3 in
      let row_upper = row_lower + 3 in
      let col_lower = block_num mod 3 * 3 in
      let col_upper = col_lower + 3 in
      let rec loop_block x y acc =
        if y >= col_upper then acc
        else if x >= row_upper then loop_block row_lower (y + 1) acc
        else
          match get board x y with
          | None -> loop_block (x + 1) y acc
          | Some elem -> loop_block (x + 1) y (elem :: acc)
      in
      let curr_block = loop_block row_lower col_lower [] in
      func_to_check curr_block
    in
    let rec loop_blocks (block_num : int) (acc : bool) =
      if block_num >= 9 then acc
      else
        check_block block_num |> fun valid_block ->
        loop_blocks (block_num + 1) (acc && valid_block)
    in
    loop_blocks 0 true

  let check_cols (board : t) (func_to_check : element list -> bool) : bool =
    let check_col (col_num : int) =
      let rec loop_rows (row_idx : int) acc =
        if row_idx >= 9 then acc
        else
          match get board row_idx col_num with
          | None ->
              failwith
                "tried to access invalid element or board incorrectly \
                 structured"
          | Some elem -> loop_rows (row_idx + 1) (elem :: acc)
      in
      let curr_col = loop_rows 0 [] in
      func_to_check curr_col
    in
    let rec loop_cols (col_num : int) (acc : bool) =
      if col_num >= 9 then acc
      else
        check_col col_num |> fun valid_col ->
        loop_cols (col_num + 1) (acc && valid_col)
    in
    loop_cols 0 true

  let fold_check_non_empty ((acc, seen) : bool * int list) (elem : element) =
    match elem with
    | Empty -> (false, seen)
    | Fixed a | Volatile a -> (acc, a :: seen)

  let is_solved_list (lst : element list) : bool =
    let filled, seen = List.fold lst ~init:(true, []) ~f:fold_check_non_empty in
    if filled then
      seen
      |> List.sort ~compare:compare_int
      |> List.equal equal_int (List.range 1 10)
    else false

  let is_solved (board : t) : bool =
    check_keys board
    && check_rows board is_solved_list
    && check_cols board is_solved_list
    && check_block board is_solved_list

  let is_valid_lst (lst : element list) : bool =
    let _, seen = List.fold lst ~init:(true, []) ~f:fold_check_non_empty in
    let unique_seen = List.dedup_and_sort seen ~compare:compare_int in
    let no_dups = List.length seen = List.length unique_seen in
    let no_invalid = List.for_all seen ~f:(fun x -> x >= 1 && x <= 9) in
    no_dups && no_invalid

  let is_valid (board : t) : bool =
    check_keys board
    && check_rows board is_valid_lst
    && check_cols board is_valid_lst
    && check_block board is_valid_lst

  let set (board : t) (x : int) (y : int) (element : element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8 && is_valid board);
    Map.update board x ~f:(fun row ->
        match row with
        | None -> assert false
        | Some row -> Map.update row y ~f:(fun _ -> element))

  (* doesn't check if is_valid before setting, useful for creating boards for debugging *)
  let set_forced (board : t) (x : int) (y : int) (element : element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8);
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

  (** Takes a fully solved sudoko. This method expects a fully solved sudoku *)
  let generate_random _ = failwith "Not implemented"

  let generate_degenerate (board : t) (difficulty : difficulty) : t =
    failwith "Not implemented"

  (** *)

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

  let solve (_ : t) : t option = None

  type json = Yojson.Safe.t
  (** Generates a solved sudoko with all the cells filled *)

  let serialize (map : t) : json option =
    let convert_map_content_to_json value_map data =
      let open Option.Let_syntax in
      (if Map.is_empty data then None else Some data)
      >>| Map.to_alist
      >>| List.map ~f:(Tuple2.map_both ~f1:string_of_int ~f2:value_map)
      >>| fun a -> `Assoc a
    in

    map
    |> Map.map ~f:(convert_map_content_to_json element_to_yojson)
    |> Map.filter_map ~f:Fn.id
    |> convert_map_content_to_json Fn.id

  let de_serialize (obj : json) : t option =
    (* TODO: Add error handling *)
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
      let new_map = convert_to_map_if_possible obj ~f:(fun (key, value) ->
        match (int_of_string_opt key, yojson_to_row value) with
        | Some key_int, row -> Some (key_int, row)
        | _ -> None) in
      Some new_map
    with
      | exn -> None 

let equal_test (board1 : t) (board2 : t) =
  let equal_row (row1 : row) (row2 : row) =
    let alist1 = Map.to_alist row1 in
    let alist2 = Map.to_alist row2 in
    List.for_all2_exn alist1 alist2 ~f:(fun (col1, elem1) (col2, elem2) ->
      col1 = col2 && equal_element elem1 elem2
    )
  in
      
  let alist1 = Map.to_alist board1 in
  let alist2 = Map.to_alist board2 in
  List.length alist1 = List.length alist2 &&
  List.for_all2_exn alist1 alist2 ~f:(fun (_, row_elem1) (_, row_elem2) ->
    equal_row row_elem1 row_elem2
  )
      
      

  let pretty_print (board : t) : string =
    let left_spacing : string = "  " in
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