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

  val equal : t -> t -> bool
  val empty : t
  val get : t -> int -> int -> element option
  val set : t -> int -> int -> element -> t
  val set_forced : t -> int -> int -> element -> t
  val get_all : t -> element list
  val get_row : t -> int -> element list
  val get_col : t -> int -> element list
  val get_block : t -> int -> element list
  val check_keys : t -> bool
end

module Make_sudoku_grid (E : Element) = struct
  include E

  type row = (int, element, Int.comparator_witness) Map.t
  type t = (int, row, Int.comparator_witness) Map.t

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
    (* check row keys are 0-8 *)
    let check_row_keys = map_has_keys_one_through_nine board in

    (* check col keys are 0-8*)
    let check_col_keys _ =
      List.range 0 9
      |> List.map ~f:(Map.find_exn board)
      |> List.for_all ~f:map_has_keys_one_through_nine
    in

    check_row_keys && check_col_keys ()

  let get_row (board : t) (x : int) : element list =
    assert (0 <= x && x <= 8 && check_keys board);
    Map.find_exn board x |> Map.data

  let get_col (board : t) (x : int) : element list =
    assert (0 <= x && x <= 8 && check_keys board);
    Map.map board ~f:(fun row -> Map.find_exn row x) |> Map.data

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

  let update (board : t) (x : int) (y : int)
      (element : element option -> element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8);
    Map.update board x ~f:(fun row ->
        match row with
        | None -> assert false
        | Some row -> Map.update row y ~f:element)

  let set (board : t) (x : int) (y : int) (element : element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8 && element_is_valid element);
    update board x y (fun _ -> element)

  (* doesn't check if is_valid before setting, useful for creating boards for debugging *)
  let set_forced (board : t) (x : int) (y : int) (element : element) : t =
    assert (0 <= x && x <= 8 && 0 <= y && y <= 8 && element_is_valid element);
    update board x y (fun _ -> element)

  let empty : t =
    let a = Map.empty (module Int) in
    let empty_row =
      List.init 9 ~f:(fun _ -> empty_element)
      |> List.foldi ~init:a ~f:(fun index map element ->
             Map.add_exn map ~key:index ~data:element)
    in
    List.init 9 ~f:(fun _ -> empty_row)
    |> List.foldi ~init:a ~f:(fun index map element ->
           Map.add_exn map ~key:index ~data:element)
end
