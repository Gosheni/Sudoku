open Board
open Core 
let single_cell id = 
    <td id="<%s id %>">9</td>
let table_row id = 
  <tr id="<%s id %>">
    <%s! 
    List.range 1 10 
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
        </style>
    </head>
    <body>
        <table>
          <%s! 
          List.range 1 10 
          |> List.map ~f: string_of_int 
          |> List.map ~f: table_row 
          |> List.fold ~init: "" ~f: (^) %>
        </table>
    </body>
  </html>

let () =
Dream.run
@@ Dream.logger
@@ Dream.router [
  Dream.get "/emptyboard" (fun _ ->
    Dream.json Sudoku_board.(empty |> serialize |> Yojson.Safe.to_string));
  Dream.get "/" (fun _ ->
    Dream.html (render ()))
]