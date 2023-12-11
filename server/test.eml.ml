open Board
open Core 
open Iolib
open Game

let single_cell id = 
    <td class="cell" id="<%s id %>" onclick="changeSelectedCell(this)"/>

let number_button number =
  <td class="numberButton" id="numberButton<%s number %>" onclick="numberButtonWasTapped(this)"><%s (number)%></td>
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
        </style>
        <script>
        
      function changeSelectedCell(cell) {
        isHighlighted = cell.classList.contains('selected');
        if (!isHighlighted) {
          var cells = document.getElementsByClassName("cell");
          for (var i = 0; i < cells.length; i++) {
            cells.item(i).classList.remove('selected');
          }
            cell.classList.add('selected');
        }
      }

      function numberButtonWasTapped(button) {
        let move = button.textContent == "X" ? null : button.textContent; 
        doMove(move)
      }

      function getSelectedCellCoords() {
        let selected = document.getElementsByClassName('selected')[0];
        let coords = selected.id.split('');
        return coords
      }

        function doMove(move) {
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
              console.log(currentBoard[row][col]);
              if (cell.classList.contains('bold')) {
                cell.classList.remove('bold');
              }
              
              if (currentBoard[row][col][0] === "Empty") {
                hasSeenEmpty = true;
                cell.textContent = "";  
              } else if (currentBoard[row][col][0] === "Fixed") {
                  cell.classList.add('bold')
                  cell.textContent = currentBoard[row][col][1]
              } else {
                  cell.textContent = currentBoard[row][col][1]
              }
            }
          }
          if (!hasSeenEmpty) {
            console.log("You have won");
            setTimeout(function() {
              alert("You have won!!");
            }, 500);
          }

        }
        var currentBoard = ""
        fetch("/api/v1/initialize")
          .then((response) => {
            if (response.ok) {
              return response.json()
            } else {
              throw new Error(`${response.status} ${response.statusText}`);
            }
          })
          .then((json) => populateBoard(json));

        document.addEventListener('keydown', function(event) {
            console.log(event.keyCode);

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
        <table class="sudoku-table">
          <%s! 
          List.range 0 9
          |> List.map ~f: string_of_int 
          |> List.map ~f: table_row 
          |> List.fold ~init: "" ~f: (^) %>
        </table>
        <br/>
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
    </body>
  </html>

let get_board request =
  match Dream.cookie request "current.game" with
  | None -> None
  | Some game_title -> (
      match Configuration.get_game game_title with
      | None -> None
      | Some game -> Some (game, game_title))

let parse_move request =
  let apply_move move =
    match get_board request with
    | None -> Dream.json ~code:405 "{\"error\":\"some error\"}"
    | Some (game, game_title) -> (
        match Sudoku_game.do_move game move with
        | Ok new_board ->
            Configuration.update_game game_title new_board;
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

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/api/v1/initialize" (fun request ->
             let open Sudoku_board in
             let difficulty = 20 in
             let board =
               generate_random () |> Fn.flip generate_degenerate difficulty
             in
             let board_json : string =
               serialize board |> Yojson.Safe.to_string
             in
             let title =
               List.init 20 ~f:(fun _ -> Random.int 10)
               |> List.map ~f:Int.to_string |> List.map ~f:Char.of_string
               |> String.of_list
             in
             Configuration.add_game title difficulty board;
             let response = Dream.response board_json in
             Dream.add_header response "Content-Type" "application/json";
             Dream.set_cookie response request "current.game" title;
             Lwt.return response);
         Dream.get "/" (fun _ -> Dream.html (render ()));
         Dream.get "/api/v1/move" parse_move;
       ]
