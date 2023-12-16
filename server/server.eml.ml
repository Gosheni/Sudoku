open Board
open Core 
open Iolib
open Game

let single_cell id = 
    <td class="cell" id="<%s id %>" onclick="changeSelectedCell(this)"/>

let number_button number =
  <td class="numberButton" id="numberButton<%s number %>" onclick="numberButtonWasTapped(this)"><%s (number)%></td>

let hint_area _ = 
  <table>
    <tr>
      <td>
      <button class="hint-button" onclick="hintButtonWasTapped(this)">Give me a hint</button>
      </td>
    </tr>
    <tr>
      <td class="hint-area" id="hint">Press the button above if you need help</td>
    </tr>
  </table>

let new_game_area _ =
  <div>
    <button class="new-game-button" onclick="newGame(40)">New Easy</button>
    <button class="new-game-button" onclick="newGame(50)">New Normal</button>
    <button class="new-game-button" onclick="newGame(60)">New Hard</button>
  </div>

let table_row id = 
  <tr id="<%s id %>">
    <%s! 
    List.range 0 9
    |> List.map ~f: string_of_int 
    |> List.map ~f: ((^) id)
    |> List.map ~f: single_cell 
    |> List.fold ~init: "" ~f: (^) %>
  </tr>
let render _ =
  <html>
    <head>
        <style>
        body {
          font-family: Arial, sans-serif;
        }
        table, th, td {
          border: 1px solid black;
          border-collapse: collapse;
          font-size: 50px;
        }
        table { table-layout: fixed; }
        .sudoku-table tr:nth-of-type(3n) td {
            border-bottom: 5px solid #f00;
        }       
        .sudoku-table tr:nth-of-type(3n - 2) td {
          border-top: 5px solid #f00;
      }       
        .sudoku-table tr td:nth-of-type(3n) {
            border-right: 5px solid #f00;
        }
        .sudoku-table tr td:nth-of-type(3n-2) {
            border-left: 5px solid #f00;
        }       
        td {
           height: 9vh;
           width: 9vh;
           text-align: center;
        }
        .selected {
           background-color: brown;
          color: #FFF;
        }
        .bold {
          font-weight: 600;
        }
        .container {
          display: flex;
        }
        .darkened {
          background-color: #3d3d3d;
          color: black;
        }
        .secondary-hint {
          background-color: #aeaeae;
        }
        .highlight-elem {
          color: #FF0000;
        }
        .outline-top {
          border-top: 5px solid #0000ff !important;
        }
        .outline-bottom {
          border-bottom: 5px solid #0000ff !important;
        }
        .outline-left {
          border-left: 5px solid #0000ff !important;
        }
        .outline-right {
          border-right: 5px solid #0000ff !important;
        }

        .main-hint {
          background-color: #ffffff;
        }
        .playarea {
          display: flex;
          flex-direction: column;
        }
        .hint-container {
          margin-left: 30px;
        }  
        .hint-area {
          font-size: 20px;
          width: 800px;
          height: 100px;
          padding: 10px;
          position: relative;
        }
        .hint-key-blue {
          margin-top: 17px;
          border: 3px solid #0000ff;
          background-color: #ffffff;
          width: 20px;
          height: 20px;
        }
        .hint-key-black {
          margin-top: 17px;
          background-color: #3d3d3d;
          width: 20px;
          height: 20px;
        }
        .hint-key-red {
          margin-top: 17px;
          background-color: #cecece;
          color: #FF0000;
          width: 20px;
          height: 20px;
          padding-left: 5px;
        }
        .hint-key {
          margin-top: 10px;
          display: none;
          flex-direction: row;
        }
        .hint-key-subsection {
          display: flex;
          flex-direction: row;
          margin-left: 10px;
        }
        .hint-button{
          align: center;
          display: inline-block;
          background-color: #d3d3d3;
          border-radius: 7px;
          width: 200px;
          font-size: 15px;
        }
        .new-game-container {
          margin-top: 30px;
          margin-left: 140px;
        }  
        .new-game-button {
          display: inline-block;
          background-color: #d3d3d3;
          border-radius: 7px;
          width: 150px; /* Adjust the width as needed */
          font-size: 15px;
          margin-right: 20px; /* Optional: Add some margin between buttons */
        }
        #timer {
          text-align: center;
        }
        </style>
        <script>
      let hintCalled = false;
      let gameFinished = false;
      function changeSelectedCell(cell) {
        unhighlightCells(); // included whereever an action is taken, to cancel hint highlights
        isHighlighted = cell.classList.contains('selected');
        if (!isHighlighted) {
          var cells = document.getElementsByClassName("cell");
          for (var i = 0; i < cells.length; i++) {
            cells.item(i).classList.remove('selected');
          }
            cell.classList.add('selected');
        }
      }

      function hintButtonWasTapped(button) {
        fetch("/api/v1/hint")
          .then((response) => {
            hintCalled = true;
            if (response.ok) {
              return response.json();
            } else {
              throw new Error(`${response.status} ${response.statusText}`);
            }
          })
          .then((json) => makeHint(json));
      }

      function newGame(difficulty) {
        // reset hints
        unhighlightCells();
        hintCalled = false;
        var hintText = document.getElementById("hint");
        hintText.textContent = "Press the button above if you need help";

        let reqUrl = "/api/v1/initialize?difficulty=" + difficulty;
        fetch(reqUrl)
          .then((response) => {
            if (response.ok) {
              return response.json();
            } else {
              throw new Error(`${response.status} ${response.statusText}`);
            }
          })
          .then((json) => {
            startTime = new Date().getTime();
            populateBoard(json);
            gameFinished = false; // Reset the gameFinished flag
          });
      }

      function highlightHint(squares, secondary, target, elem, forced_by) {
        unhighlightCells(); // in case two hints are asked for in a row
        hintCalled = true;
        var key = document.getElementsByClassName("hint-key");
        key[0].style.display = "flex";
        var cells = document.getElementsByClassName("cell");
        console.log(secondary[0]);
        console.log(target[0]);
        for (var i = 0; i < cells.length; i++) {
          cells.item(i).classList.add("darkened");
        }
        for (var i = 0; i < squares.length; i++) {
          var square = squares[i];
          var x = square[0];
          var y = square[1];
          var cell = document.getElementById(x.toString() + y.toString());
          console.log(forced_by);
          if (forced_by === "row") {
            if (i == 0) {
              cell.classList.add("outline-right");
            } else if (i == squares.length - 1) {
              cell.classList.add("outline-left");
            }
            cell.classList.add("outline-top");
            cell.classList.add("outline-bottom");
          }
          if (forced_by === "col") {
            if (i == 0) {
              cell.classList.add("outline-bottom");
            } else if (i == squares.length - 1) {
              cell.classList.add("outline-top");
            }
            cell.classList.add("outline-left");
            cell.classList.add("outline-right");
          }
          if (forced_by === "block") {
            if (i < 3) {
              cell.classList.add("outline-bottom");
            } else if (i >= 6) {
              cell.classList.add("outline-top");
            } 
            if (i % 3 == 0) {
              cell.classList.add("outline-right");
            } else if (i % 3 == 2) {
              cell.classList.add("outline-left");
            }
          }
          if (forced_by === "single") {
            cell.classList.add("outline-top");
            cell.classList.add("outline-bottom");
            cell.classList.add("outline-left");
            cell.classList.add("outline-right");
          }
        }
        for (var i = 0; i < secondary.length; i++) {
          var square = secondary[i];
          var x = square[0];
          var y = square[1];
          var cell = document.getElementById(x.toString() + y.toString());
          cell.classList.add("secondary-hint");
          if (cell.textContent === elem.toString()) {
            cell.classList.add("highlight-elem");
          }
        }
        var targetCell = document.getElementById(target[0].toString() + target[1].toString());
        targetCell.classList.add("main-hint");
        targetCell.classList.remove("secondary-hint");
      }

      function unhighlightCells() {
        if (hintCalled) {
          var key = document.getElementsByClassName("hint-key");
          key[0].style.display = "none";
          var cells = document.getElementsByClassName("cell");
          for (var i = 0; i < cells.length; i++) {
            var cell = cells.item(i);
            cell.classList.remove("darkened");
            if (cell.classList.contains("main-hint")) {
              cell.classList.remove("main-hint");
            }
            if (cell.classList.contains("secondary-hint")) {
              cell.classList.remove("secondary-hint");
            }
            if (cell.classList.contains("highlight-elem")) {
              cell.classList.remove("highlight-elem");
            }
            if (cell.classList.contains("outline-top")) {
              cell.classList.remove("outline-top");
            }
            if (cell.classList.contains("outline-bottom")) {
              cell.classList.remove("outline-bottom");
            }
            if (cell.classList.contains("outline-left")) {
              cell.classList.remove("outline-left");
            }
            if (cell.classList.contains("outline-right")) {
              cell.classList.remove("outline-right");
            }
          }
          hintCalled = false;
        }
      }

      function makeHint(json) {
        console.log(json);
        var hint = json["hint"];
        if (json["squares"] !== undefined){
          var squares = json["squares"];
          var secondary = json["secondary_squares"];
          var target = json["target_coord"];
          var elem = json["target_elem"];
          var forced_by = json["forced_by"];
          highlightHint(squares, secondary, target, elem, forced_by);
        }
        var hintText = document.getElementById("hint");
        hintText.textContent = hint;
      }

      function numberButtonWasTapped(button) {
        unhighlightCells(); // included whereever an action is taken, to cancel hint highlights
        let move = button.textContent == "X" ? null : button.textContent; 
        doMove(move)
      }

      function getSelectedCellCoords() {
        console.log("here");
        let selected = document.getElementsByClassName('selected')[0];
        console.log(selected.id.split(''));
        let coords = selected.id.split('');
        return coords
      }

        function doMove(move) {
          if (gameFinished) { return }
          let coords = getSelectedCellCoords();
          console.log("now here");
          console.log(coords);
          let x = coords[0];
          let y = coords[1];
          if (!x || !y) { return }
          let baseURL = "/api/v1/move?x=" + x + "&y=" + y;
          let reqUrl = (move ==  null) ? baseURL : baseURL  + "&move="+ move ;
  
          fetch(reqUrl)
            .then((response) => {
              if (response.ok) {
                return response.json()
              } else {
                throw new Error(`${response.status} ${response.statusText}`);
              }
            })
            .then((json) => populateBoard(json));

        }

        function populateBoard(json) {
          currentBoard = json;
          var hasSeenEmpty = false
          for (var row = 0; row < 9; row++) {
            for (var col = 0; col < 9; col++) {
              let cell = document.getElementById(row.toString() + col.toString());
              let element = currentBoard[row][col];
              
              if (cell.classList.contains('bold')) {
                cell.classList.remove('bold');
              }
              
              if (element[0] === "Empty") {
                hasSeenEmpty = true;
                cell.textContent = "";  
              } else if (element[0] === "Fixed") {
                  cell.textContent = element[1];
              } else if (element[0] === "Volatile") {
                  cell.classList.add('bold');
                  cell.textContent = element[1];
              } else {
                  console.log("Received unknown sudoku cell");
              }
            }
          }
          if (!hasSeenEmpty) {
            console.log("You have won");
            setTimeout(function() {
              clearInterval(timer);
              gameFinished = true;
              alert("You have won!!");
            }, 500);
          }

        }

        function updateTimer() {
          // Inspired by https://www.w3schools.com/howto/howto_js_countdown.asp
          let now = new Date().getTime();
          var distance = now - startTime;

          let days = Math.floor(distance / (1000 * 60 * 60 * 24));
          let hours = Math.floor((distance % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
          let minutes = Math.floor((distance % (1000 * 60 * 60)) / (1000 * 60));
          let seconds = Math.floor((distance % (1000 * 60)) / 1000);

          document.getElementById("timer").innerHTML = (days > 0 ? days + "d " : "") + hours + "h "
          + minutes + "m " + seconds + "s ";
        }
        var timer = setInterval(updateTimer, 100);

        var currentBoard = "";
        var startTime;
        fetch("/api/v1/initialize")
          .then((response) => {
            if (response.ok) {
              return response.json()
            } else {
              throw new Error(`${response.status} ${response.statusText}`);
            }
          })
          .then((json) => {
            startTime = new Date().getTime();  
            populateBoard(json)
          });

        document.addEventListener('keydown', function(event) {
            unhighlightCells(); // included whereever an action is taken, to cancel hint highlights

            let coords = getSelectedCellCoords();
            let row = parseInt(coords[0]);
            let col = parseInt(coords[1]);

            if(event.keyCode >= 49 && event.keyCode <= 59) { // 1-9
              doMove(event.keyCode - 48 )
            }
            else if(event.keyCode == 8) { // Backspace
              doMove(null)
            } else if (event.keyCode == 38 && row > 0) { // Arrow up 
                let cell = document.getElementById((row - 1).toString() + col.toString());
                changeSelectedCell(cell);
                event.preventDefault();
            } else if (event.keyCode == 40 && row < 8) { // Arrow down 
              let cell = document.getElementById((row + 1).toString() + col.toString());
              changeSelectedCell(cell);
              event.preventDefault();
            } else if (event.keyCode == 37 && col > 0) { // Arrow left 
              let cell = document.getElementById(row.toString() + (col - 1).toString());
              changeSelectedCell(cell);
              event.preventDefault();
            } else if (event.keyCode == 39 && col < 8) { // Arrow right 
              let cell = document.getElementById(row.toString() + (col + 1).toString());
              changeSelectedCell(cell);
              event.preventDefault();
            }
            
        });
        </script>
    </head>
    <body>
    <div class="container">
      <div class="playarea">
        <table class="sudoku-table">
          <%s! 
          List.range 0 9
          |> List.map ~f: string_of_int 
          |> List.map ~f: table_row 
          |> List.fold ~init: "" ~f: (^) %>
        </table>
        <div class="numberbuttons" style="margin-top: 10px">
        <table>
          <tr> 
          <%s! 
            List.range 1 10 
            |> List.map ~f: string_of_int 
            |> List.map ~f: number_button 
            |> List.fold ~init: "" ~f: (^) %>
          <%s! number_button "X" %>
          </tr>
        </table>
        </div>
      </div>
      <div class="right-container">
        <h1 id="timer"></h1>
        <div class="hint-container">
          <%s! hint_area () %>
          <div class="hint-key">
            <div class="hint-key-subsection">
            <div class="hint-key-blue"></div>
            <p>&nbsp Relevant section   </p></div>
            <div class="hint-key-subsection">
            <div class="hint-key-black"></div>
            <p>&nbsp    Not Important  </p></div>
            <div class="hint-key-subsection">
            <div class="hint-key-red">2</div>
            <p>&nbsp    Can't be here because of conflict  </p></div>
        </div>
        <div class="new-game-container">
          <%s! new_game_area () %>
        </div> 

      </div>
      
    </div>
    </body>
  </html>

let get_section_as_coordinate_list (make_coord : int -> Sudoku_board.coordinate) =
  List.range 0 9 |> List.map ~f:make_coord
  |> List.fold ~init:[] ~f:(fun acc x -> x :: acc)

let get_primary_squares (move : move)
    (forced_by : Hint.Hint_system.forced_source) : Sudoku_board.coordinate list =
  let open Hint.Hint_system in
  match forced_by with
  | Single | Incorrect -> [ (move.x, move.y) ]
  | Row -> get_section_as_coordinate_list (fun y -> (move.x, y))
  | Col -> get_section_as_coordinate_list (fun x -> (x, move.y))
  | Block ->
      let make_coord_board x =
        let y = x mod 3 in
        let x = x / 3 in
        ((move.x / 3 * 3) + x, (move.y / 3 * 3) + y)
      in
      get_section_as_coordinate_list make_coord_board

let get_secondary_squares (board : Sudoku_board.t) (move : move)
    (forced_by : Hint.Hint_system.forced_source) : Sudoku_board.coordinate list =
  let open Hint.Hint_system in
  let row_idx = move.x in
  let col_idx = move.y in
  let target = move.value |> Option.value_exn in
  let block_idx = (row_idx / 3 * 3) + (col_idx / 3) in
  let make_coord_block x =
    let y = x mod 3 in
    let x = x / 3 in
    ((row_idx / 3 * 3) + x, (col_idx / 3 * 3) + y)
  in
  let section, make_coord =
    match forced_by with
    | Single | Incorrect -> ([], fun _ -> (0, 0))
    | Row -> (Sudoku_board.get_row board row_idx, fun y -> (row_idx, y))
    | Col -> (Sudoku_board.get_col board col_idx, fun x -> (x, col_idx))
    | Block -> (Sudoku_board.get_block board block_idx, make_coord_block)
  in
  let get_secondary_squares_of_cell row_idx col_idx =
    let check_section (section : Sudoku_board.element list)
        (make_coords : int -> Sudoku_board.coordinate) =
      if
        List.filter section ~f:(fun c ->
            match c with Fixed a | Volatile a -> a = target | _ -> false)
        |> List.length > 0
      then get_section_as_coordinate_list make_coords
      else []
    in
    let block_idx = (row_idx / 3 * 3) + (col_idx / 3) in
    let make_coord_block x =
      let y = x mod 3 in
      let x = x / 3 in
      ((row_idx / 3 * 3) + x, (col_idx / 3 * 3) + y)
    in
    let block =
      check_section (Sudoku_board.get_block board block_idx) make_coord_block
    in
    if List.length block > 0 then block
    else
      let row =
        check_section (Sudoku_board.get_row board row_idx) (fun y ->
            (row_idx, y))
      in
      if List.length row > 0 then row
      else
        check_section (Sudoku_board.get_col board col_idx) (fun x ->
            (x, col_idx))
  in
  (* only look for secondary squares if main square is empty *)
  List.filter_mapi section ~f:(fun i c ->
      let x, y = make_coord i in
      match c with
      | Empty -> get_secondary_squares_of_cell x y |> Some
      | _ -> None)
  |> List.concat

let get_board request =
  match Dream.cookie request "current.game" with
  | None -> None
  | Some game_title -> Configuration.get_game game_title

let hint_to_json board (hint : hint) : Yojson.Safe.t =
  match hint with
  | Incorrect_cell | Already_solved | Suggest_guess _ ->
      let desc = Game.describe_hint hint in
      `Assoc [ ("hint", `String desc) ]
  | Suggested_move (move, forced_by) ->
      let squares_to_highlight = get_primary_squares move forced_by in
      let _ = print_endline (move.value |> Option.value_exn |> Int.to_string) in
      let secondary_squares = get_secondary_squares board move forced_by in
      let coordinates_to_json_str ls =
        `List (List.map ~f:(fun (x, y) -> `List [ `Int x; `Int y ]) ls)
      in
      let desc = Game.describe_hint hint in
      let target = `List [ `Int move.x; `Int move.y ] in
      `Assoc
        [
          ("hint", `String desc);
          ("squares", coordinates_to_json_str squares_to_highlight);
          ("secondary_squares", coordinates_to_json_str secondary_squares);
          ("target_coord", target);
          ("target_elem", `Int (move.value |> Option.value_exn));
          ( "forced_by",
            `String (Hint.Hint_system.forced_source_to_string forced_by) );
        ]

let parse_initialize request =
  let open Sudoku_board in
  let difficulty =
    Dream.query request "difficulty"
    |> Option.map ~f:(fun x -> int_of_string x)
    |> Option.value ~default:50
  in
  let board = generate_random () |> Fn.flip generate_degenerate difficulty in
  let board_json: string = serialize board |> Yojson.Safe.to_string in
  let title = List.init 20 ~f:(fun _ -> Random.int 10) |> List.map ~f: Int.to_string
      |> List.map ~f: Char.of_string
      |> String.of_list in
  let _ = Configuration.add_game title difficulty board in
  let response = Dream.response board_json in
  Dream.add_header response "Content-Type" "application/json";
  Dream.set_cookie response request "current.game" title;
  Lwt.return response

let parse_hint request =
  match get_board request with
  | None -> Dream.respond "error"
  | Some (_, board) ->
      let hint = Game.generate_hint ~use_crooks:true board in
      let json = hint_to_json board hint |> Yojson.Safe.to_string in
      Dream.json json

let parse_move request =
  let apply_move move =
    match get_board request with
    | None -> Dream.json ~code:405 "{\"error\":\"some error\"}"
    | Some (game, board) -> (
        match Game.do_move board move with
        | Ok new_board ->
            if Sudoku_board.is_solved new_board then
              let _ = Configuration.finish_game game in
              ()
            else Configuration.update_game game new_board;
            let json =
              Sudoku_board.serialize new_board |> Yojson.Safe.to_string
            in
            Dream.json json
        | _ -> Dream.json ~code:405 "{\"error\":\"some error\"}")
  in
  match
    ( Dream.query request "x",
      Dream.query request "y",
      Dream.query request "move" )
  with
  | Some x, Some y, Some move ->
      apply_move
        {
          x = Int.of_string x;
          y = Int.of_string y;
          value = Some (Int.of_string move);
        }
  | Some x, Some y, None ->
      apply_move { x = Int.of_string x; y = Int.of_string y; value = None }
  | _ -> Dream.json ~code:405 "{\"error\":\"some error\"}"

let get_api path = Dream.get ("/api/v1/" ^ path)

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
        [
          Dream.get "/" (fun _ -> Dream.html (render ()));
          get_api "initialize" parse_initialize;
          get_api "move" parse_move;
          get_api "hint" parse_hint;
        ]