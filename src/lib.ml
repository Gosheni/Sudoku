[@@@ocaml.warning "-27"]

open Core

module Sudoku_board = struct
  type element =
    | Empty
    | Fixed of int
    | Volatile of int  (** Contains the board state including which *)
  [@@deriving yojson]

  type row = (int, element, Int.comparator_witness) Map.t
  type t = (int, row, Int.comparator_witness) Map.t
  type difficulty = int

  let element_to_string = function
    | Empty -> " "
    | Fixed a | Volatile a -> Int.to_string a

  let get (board : t) (x : int) (y : int) : element option =
    let open Option.Let_syntax in
    Map.find board x >>= Fn.flip Map.find y

  let is_valid (board : t) : bool = false
  let is_solved (board : t) : bool = false

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

  let set (board : t) (x : int) (y : int) (element : element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8 && is_valid board);
    Map.update board x ~f:(fun row ->
        match row with
        | None -> assert false
        | Some row -> Map.update row y ~f:(fun _ -> element))

  (** Takes a fully solved sudoko. This method expects a fully solved sudoku *)
  let generate_random _ = failwith "Not implemented"

  let generate_degenerate (board : t) (difficulty : difficulty) : t =
    failwith "Not implemented"

  (** *)
  let solve (_ : t) : t option = None

  type json = Yojson.Safe.t
  (** Generates a solved sudoko with all the cells filled *)

  let row_to_yojson (row : row) : json option =
    if Map.is_empty row then None
    else
      Some
        (`Assoc
          (Map.to_alist row
          |> List.map ~f:(fun (k, v) -> (string_of_int k, element_to_yojson v))
          ))

  let yojson_to_row (obj : json) : row =
    match obj with
    | `Assoc assoc ->
        let elements =
          List.filter_map assoc ~f:(fun (key, value) ->
              match (int_of_string_opt key, element_of_yojson value) with
              | Some key_int, Ok element -> Some (key_int, element)
              | _ -> None)
        in
        Map.of_alist_exn (module Int) elements
    | _ -> Map.empty (module Int)

  let de_serialize (map : t) : json option =
    let json_rows = Map.map map ~f:row_to_yojson in
    let filtered_json_rows = Map.filter_map json_rows ~f:Fn.id in

    if Map.is_empty filtered_json_rows then None
    else
      Some
        (`Assoc
          (Map.to_alist filtered_json_rows
          |> List.map ~f:(fun (k, v) -> (string_of_int k, v))))

  let serialize (obj : json) : t =
    match obj with
    | `Assoc assoc ->
        let rows =
          List.filter_map assoc ~f:(fun (key, value) ->
              match (int_of_string_opt key, yojson_to_row value) with
              | Some key_int, row -> Some (key_int, row)
              | _ -> None)
        in
        Map.of_alist_exn (module Int) rows
    | _ -> Map.empty (module Int)

  let pretty_print (board : t) : string =
    let pretty_print_row (row : row) : string =
      Map.fold row ~init:"" ~f:(fun ~key:col_num ~data:value accum ->
          let block = element_to_string value ^ " " in
          if col_num mod 3 = 0 then accum ^ "| " ^ block else accum ^ block)
      ^ "|"
    in

    let divider_line : string =
      String.init (4 + ((3 + 4) * 3)) ~f:(fun _ -> '-') ^ "\n"
    in

    Map.fold board ~init:"" ~f:(fun ~key:row_num ~data:row_data accum ->
        let row = pretty_print_row row_data ^ "\n" in
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
    | Alread_solved

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
