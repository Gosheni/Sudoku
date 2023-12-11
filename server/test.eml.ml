open Board
open Core 
open Iolib
open Game

let single_cell id = 
    <td class="cell" id="<%s id %>" onclick="changeSelectedCell(this)">
    <!--<input type="number" id="name" pattern="\d?" minlength="0" maxlength="1" width="80%" />-->
    </td>

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
        table, th, td {
          border: 1px solid black;
          border-collapse: collapse;
        }
        table { table-layout: fixed; }
        td {
           height: 10vh;
           width: 10vh;
           text-align: center;
        }
        .selected {
           background-color: brown;
          color: #FFF;
        }
        .selected {
          background-color: brown;
          color: #DDD;
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
        var move =  button.textContent; 
        var selected = document.getElementsByClassName('selected')[0];
        var coords = selected.id.split('');
        var x = coords[0];
        var y = coords[1];
        var baseURL = "/api/v1/move?x=" + x + "&y=" + y;
        var reqUrl = (move == "X") ? baseURL : baseURL  + "&move="+ move ;
        
        
        
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
          for (var row = 0; row < 9; row++) {
            for (var col = 0; col < 9; col++) {
              let cell = document.getElementById(row.toString() + col.toString());
              console.log(currentBoard[row][col]);
              if (cell.classList.contains('bold')) {
                cell.classList.remove('bold');
              }
              
              if (currentBoard[row][col][0] === "Empty") {
                cell.textContent = "";  
              } else if (currentBoard[row][col][0] === "Fixed") {
                  cell.classList.add('bold')
                  cell.textContent = currentBoard[row][col][1]
              } else {
                  cell.textContent = currentBoard[row][col][1]
              }
            }
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

        </script>
    </head>
    <body>
        <table>
          <%s! 
          List.range 0 9
          |> List.map ~f: string_of_int 
          |> List.map ~f: table_row 
          |> List.fold ~init: "" ~f: (^) %>
        </table>
        <br/>
        <table>
          <tr> 
          <%s! number_button "X" %>
          <%s! 
            List.range 1 10 
            |> List.map ~f: string_of_int 
            |> List.map ~f: number_button 
            |> List.fold ~init: "" ~f: (^) %>
          </tr>
        </table>
    </body>
  </html>







let get_board request =
  match Dream.cookie request "current.game" with 
  | None -> None 
  | Some game_title -> 
      match Configuration.get_game game_title with 
      | None -> None 
      | Some game -> Some (game, game_title)


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

    let difficulty = 20 in 
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

    Dream.get "/api/v1/move" parse_move
]