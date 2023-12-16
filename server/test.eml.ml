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
      <td class="hint-area" id="hint">Hint text appears here</td>
    </tr>
  </table>

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
        }
        .main-hint {
          background-color: #d3d3d3;
          border: 5px solid grey;
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
        .hint-button{
          align: center;
          display: inline-block;
          background-color: #d3d3d3;
          border-radius: 7px;
          width: 200px;
          font-size: 15px;
        }
        </style>
        <script>
      let hintCalled = false;
      
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

      function highlightHint(squares, target) {
        unhighlightCells(); // in case two hints are asked for in a row
        hintCalled = true;
        var cells = document.getElementsByClassName("cell");
        for (var i = 0; i < cells.length; i++) {
          cells.item(i).classList.add("darkened");
        }
        for (var i = 0; i < squares.length; i++) {
          var square = squares[i];
          var x = square[0];
          var y = square[1];
          var cell = document.getElementById(x.toString() + y.toString());
          cell.classList.remove("darkened");
        }
        var targetCell = document.getElementById(target[0].toString() + target[1].toString());
        targetCell.classList.add("main-hint");
      }

      function unhighlightCells() {
        if (hintCalled) {
          var cells = document.getElementsByClassName("cell");
          for (var i = 0; i < cells.length; i++) {
            cells.item(i).classList.remove("darkened");
            if (cells.item(i).classList.contains("main-hint")) {
              cells.item(i).classList.remove("main-hint");
            }
          }
          hintCalled = false;
        }
      }

      function makeHint(json) {
        var hint = json["hint"];
        var squares = json["squares"];
        var target = json["target"];
        highlightHint(squares, target);
        var hintText = document.getElementById("hint");
        hintText.textContent = hint;
      }

      function numberButtonWasTapped(button) {
        unhighlightCells(); // included whereever an action is taken, to cancel hint highlights
        let move = button.textContent == "X" ? null : button.textContent; 
        doMove(move)
      }

      function getSelectedCellCoords() {
        let selected = document.getElementsByClassName('selected')[0];
        let coords = selected.id.split('');
        return coords
      }

        function doMove(move) {
          if (gameFinished) { return }
          let coords = getSelectedCellCoords();
          let x = coords[0];
          let y = coords[1];
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
            console.log(event.keyCode);
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
        <h1 id="timer"></h1>
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
      <div class="hint-container">
        <%s! hint_area () %>
      </div>
    </div>
    </body>
  </html>

let get_squares_to_highlight (move : Sudoku_game.move)
  (forced_by : Hint.Hint_system.forced_source) : (int * int) list =
  let open Hint.Hint_system in
  let get_section_as_coordinate_list (make_coord : int -> int * int) =
    List.range 0 9 |> List.map ~f:make_coord
    |> List.fold ~init:[] ~f:(fun acc x -> x :: acc)
  in
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

let get_board request =
  match Dream.cookie request "current.game" with
  | None -> None
  | Some game_title -> (
      match Configuration.get_game game_title with
      | None -> None
      | Some game -> Some (game, game_title))

let hint_to_json (hint : Sudoku_game.hint) : Yojson.Safe.t =
  match hint with
  | Incorrect_cell | Already_solved | Suggest_guess _ ->
      let desc = Sudoku_game.describe_hint hint in
      `Assoc [ ("hint", `String desc) ]
  | Suggested_move (move, forced_by) ->
      let squares_to_highlight = get_squares_to_highlight move forced_by in
      let squares_as_string =
        `List
          (List.map
            ~f:(fun (x, y) -> `List [ `Int x; `Int y ])
            squares_to_highlight)
      in
      let desc = Sudoku_game.describe_hint hint in
      let target = `List [ `Int move.x; `Int move.y ] in
      `Assoc
        [
          ("hint", `String desc);
          ("squares", squares_as_string);
          ("target", target);
        ]

let parse_hint request =
  match get_board request with
  | None -> Dream.respond "error"
  | Some (game, _) ->
      let hint = Sudoku_game.generate_hint ~use_crooks:true game in
      let json = hint_to_json hint |> Yojson.Safe.to_string in
      Dream.json json
        
      
let parse_move request = 
  let apply_move move = 
      match get_board request with 
      | None -> Dream.json ~code: 405 "{\"error\":\"some error\"}" 
      | Some (game, game_title) -> 
          match Sudoku_game.do_move game move with 
          | Ok new_board ->  
              (Configuration.update_game game_title new_board;
              let json = Sudoku_board.serialize new_board |> Yojson.Safe.to_string in 
              Dream.json json )
          | _ -> Dream.json ~code: 405 "{\"error\":\"some error\"}" 
  in 
  (match (Dream.query request "x", Dream.query request "y", Dream.query request "move") with 
        | Some x, Some y, Some move -> 
          apply_move { x=Int.of_string x; y =Int.of_string y; value= Some (Int.of_string move) }
        | Some x, Some y, None -> 
          apply_move { x=Int.of_string x; y =Int.of_string y; value= None }
        | _ -> 
          Dream.json ~code: 405 "{\"error\":\"some error\"}" 
        )  



let () =
Dream.run
@@ Dream.logger
@@ Dream.router [
  Dream.get "/api/v1/initialize" (fun request ->
    let open Sudoku_board in 

    let difficulty = 50 in 
    let board = generate_random  () |> Fn.flip generate_degenerate difficulty in 
    let board_json: string = serialize board |> Yojson.Safe.to_string in 
    let title = List.init 20 ~f:(fun _ -> Random.int 10) |> List.map ~f: Int.to_string 
        |> List.map ~f: Char.of_string 
        |> String.of_list in 
    Configuration.add_game title difficulty board; 
    let response = Dream.response board_json in
    Dream.add_header response "Content-Type" "application/json";
    Dream.set_cookie response request "current.game" title;
    Lwt.return response
  );

  Dream.get "/" (fun _ ->
    Dream.html (render ()));

    Dream.get "/api/v1/move" parse_move;

    Dream.get "/api/v1/hint" parse_hint
]
