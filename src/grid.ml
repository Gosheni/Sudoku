open Core

module type Element = sig
  type element [@@deriving equal, yojson]

  val element_is_valid : element -> bool
  val empty_element : element
  val element_to_string : element -> string
end

module type Sudoku_grid = sig
  include Element

  type row = (int, element, Core.Int.comparator_witness) Core.Map.t
  type t = (int, row, Core.Int.comparator_witness) Core.Map.t
  type coordinate = int * int

  val equal : t -> t -> bool
  val empty : t
  val get : t -> int -> int -> element option
  val set : t -> int -> int -> element -> t
  val get_all : t -> element list
  val get_row : t -> int -> element list
  val get_col : t -> int -> element list
  val get_block : t -> int -> element list
  val check_keys : t -> bool
  val is_valid_grid : t -> bool

  type json = Yojson.Safe.t

  val serialize : t -> json
  val deserialize : json -> t option
end

module Make_sudoku_grid (E : Element) = struct
  include E

  type row = (int, element, Int.comparator_witness) Map.t
  type t = (int, row, Int.comparator_witness) Map.t
  type coordinate = int * int

  let grid_size : int = 9

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
      Map.keys map |> List.equal equal_int (List.range 0 grid_size)
    in
    (* check row keys are 0-8 *)
    let check_row_keys = map_has_keys_one_through_nine board in

    (* check col keys are 0-8. This is lazily evalutated as the find_exn assumes that check_row_keys succeeds *)
    let check_col_keys _ =
      List.range 0 grid_size
      |> List.map ~f:(Map.find_exn board)
      |> List.for_all ~f:map_has_keys_one_through_nine
    in

    check_row_keys && check_col_keys ()

  let get_row (board : t) (row : int) : element list =
    (* Assumes the board satisfies `check_keys` and the row is within the range 0 ... 8  *)
    Map.find_exn board row |> Map.data

  let get_col (board : t) (col : int) : element list =
    (* Assumes the board satisfies `check_keys` and the col is within the range 0 ... 8  *)
    Map.map board ~f:(Fn.flip Map.find_exn col) |> Map.data

  (* assumes that sub-blocks correspond to ints in the following manner:
         0 1 2   3 4 5   6 7 8
       -------------------------
     0 |       |       |       |
     1 |   0   |   1   |   2   |
     2 |       |       |       |
       -------------------------
     3 |       |       |       |
     4 |   3   |   4   |   5   |
     5 |       |       |       |
       -------------------------
     6 |       |       |       |
     7 |   6   |   7   |   8   |
     8 |       |       |       |
       -------------------------
  *)
  let get_block (board : t) (x : int) : element list =
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

  let update (board : t) (x : int) (y : int)
      (element : element option -> element) : t =
    (* Assumes the board satisfies check_keys and the coordinates are valid, i.e within the range 0 ... 8 *)
    Map.update board x ~f:(fun row ->
        match row with
        | None ->
            assert false
            (* It is assumed that board is valid, thus the row will always exist *)
        | Some row -> Map.update row y ~f:element)

  let set (board : t) (x : int) (y : int) (element : element) : t =
    (* Assumes the board satisfies check_keys and the coordinates are valid, i.e within the range 0 ... 8 *)
    update board x y (fun _ -> element)

  let empty : t =
    let a = Map.empty (module Int) in
    let empty_row =
      List.init grid_size ~f:(fun _ -> empty_element)
      |> List.foldi ~init:a ~f:(fun index map element ->
             Map.add_exn map ~key:index ~data:element)
    in
    List.init grid_size ~f:(fun _ -> empty_row)
    |> List.foldi ~init:a ~f:(fun index map element ->
           Map.add_exn map ~key:index ~data:element)

  let is_valid_grid (board : t) : bool =
    check_keys board && Map.for_all board ~f:(Map.for_all ~f:element_is_valid)

  type json = Yojson.Safe.t

  let serialize (grid : t) : json =
    if is_valid_grid grid then
      let convert_map_content_to_json value_map data =
        data |> Map.to_alist
        |> List.map ~f:(Tuple2.map_both ~f1:string_of_int ~f2:value_map)
        |> fun a -> `Assoc a
      in

      grid
      |> Map.map ~f:(convert_map_content_to_json element_to_yojson)
      |> convert_map_content_to_json Fn.id
    else failwith "Tried to serialize an invalid grid"

  let deserialize (obj : json) : t option =
    let convert_to_map_if_possible (obj : json) ~f:filter_map =
      match obj with
      | `Assoc assoc -> (
          List.filter_map assoc ~f:filter_map |> Map.of_alist (module Int)
          |> function
          | `Duplicate_key _ -> None
          | `Ok a -> Some a)
      | _ -> None
    in
    let yojson_to_row obj =
      convert_to_map_if_possible obj ~f:(fun (key, value) ->
          match (int_of_string_opt key, element_of_yojson value) with
          | Some key_int, Ok element -> Some (key_int, element)
          | _ -> None)
    in

    let board =
      convert_to_map_if_possible obj ~f:(fun (key, value) ->
          match (int_of_string_opt key, yojson_to_row value) with
          | Some key_int, Some row -> Some (key_int, row)
          | _ -> None)
    in
    match board with
    | Some a -> Option.some_if (is_valid_grid a) a
    | None -> None
end
