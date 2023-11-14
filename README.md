1. **An overview of the purpose of the project**  
We implement the game Sudoko including:
    - A solver
    - A board generator
    - A command-line interface client to play the game
        - A hint system 
            - Hint system will use preemptive sets ([see linked paper](https://www.ams.org/notices/200904/rtx090400460p.pdf)) to ensure the hint generated is something the user should have been able to arrive at without guessing.
        - Functionality to save and load boards  

    If time permits we might also include more sudoko variants, a highscore system, and a web client. These variants might include "killer sudoko", "baby-sudoko (4x4)" etc.



2. **A list of libraries you plan on using**
    - **Additionally if any of the libraries are either not listed above or are in the data processing category above, you will also be required to have successfully installed the library on all team member computers and have a small demo app working to verify the library really works. We require this because OCaml libraries can be flakey. You will need to submit this demo/ as part of the code submission for your design.**  
    
    - _yojson_ to serialize/deserialize our sudoku board. 
    - _stdio_ for doing the command-line interface. 




3. **Commented module type declarations (.mli files) which will provide you with an initial specification to code to**
    - **You can obviously change this later and don’t need every single detail filled out**
    - **But, do include an initial pass at key types and functions needed and a brief comment if the meaning of a function is not clear.**

    See _Lib.mli_. 


4. **Include a mock of a use of your application, along the lines of the Minesweeper example above but showing the complete protocol.**

    ``` 
    $ ./sudoku.exe init  # initialize a new game
        1 2 3   4 5 6   7 8 9
      -------------------------
   1 |     6 |       |     1 |
   2 |   7   |   6   |   5   |
   3 | 8     | 1   3 | 2     |
      -------------------------
   4 |     5 |   4   | 8     |
   5 |   4   | 7   2 |   9   |
   6 |     8 |   1   | 7     |
      -------------------------
   7 |     1 | 2   5 |     3 |
   8 |   6   |   7   |   8   |
   9 | 2     |       | 4     |
      -------------------------

    $ ./sudoku.exe move 8 9 2  # example move, add value 2 to row 8 col 9
        1 2 3   4 5 6   7 8 9
      -------------------------
   1 |     6 |       |     1 |
   2 |   7   |   6   |   5   |
   3 | 8     | 1   3 | 2     |
      -------------------------
   4 |     5 |   4   | 8     |
   5 |   4   | 7   2 |   9   |
   6 |     8 |   1   | 7     |
      -------------------------
   7 |     1 | 2   5 |     3 |
   8 |   6   |   7   |   8 2 |
   9 | 2     |       | 4     |
      -------------------------
    
    $ ./sudoku.exe move 1 1 6  # example invalid move
    
    Can't place there because of conflicting value
        1 2 3   4 5 6   7 8 9
      -------------------------
   1 |     6 |       |     1 |
   2 |   7   |   6   |   5   |
   3 | 8     | 1   3 | 2     |
      -------------------------
   4 |     5 |   4   | 8     |
   5 |   4   | 7   2 |   9   |
   6 |     8 |   1   | 7     |
      -------------------------
   7 |     1 | 2   5 |     3 |
   8 |   6   |   7   |   8 2 |
   9 | 2     |       | 4     |
      -------------------------

    $ ./sudoku.exe hint

    Possible move is 8 at 2, 9
        1 2 3   4 5 6   7 8 9
      -------------------------
   1 |     6 |       |     1 |
   2 |   7   |   6   |   5   |
   3 | 8     | 1   3 | 2     |
      -------------------------
   4 |     5 |   4   | 8     |
   5 |   4   | 7   2 |   9   |
   6 |     8 |   1   | 7     |
      -------------------------
   7 |     1 | 2   5 |     3 |
   8 |   6   |   7   |   8 2 |
   9 | 2     |       | 4     |
      -------------------------

      $ ./sudoku.exe solve
        1 2 3   4 5 6   7 8 9
      -------------------------
    1 | 5 3 6 | 8 2 7 | 9 4 1 |
    2 | 1 7 2 | 9 6 4 | 3 5 8 |
    3 | 8 9 4 | 1 5 3 | 2 6 7 |
      -------------------------
    4 | 7 1 5 | 3 4 9 | 8 2 6 |
    5 | 6 4 3 | 7 8 2 | 1 9 5 |
    6 | 9 2 8 | 5 1 6 | 7 3 4 |
      -------------------------
    7 | 4 8 1 | 2 9 5 | 6 7 3 |
    8 | 3 6 9 | 4 7 1 | 5 8 2 |
    9 | 2 5 7 | 6 3 8 | 4 1 9 |
      -------------------------

      Please init a new board to keep playing

    $ ./sudoku.exe init path_to_existing_board.json # user can also create board from existing json
        1 2 3   4 5 6   7 8 9
       -------------------------
    1 | 3   6 | 5   8 | 4     |
    2 | 5 2   |       |       |
    3 |   8 7 |       |   3 1 |
      -------------------------
    4 |     3 |   1   |   8   |
    5 | 9     | 8 6 3 |     5 |
    6 |   5   |   9   | 6     |
      -------------------------
    7 | 1 3   |       | 2 5   |
    8 |       |       |   7 4 |
    9 |     5 | 2   6 | 3     |
      -------------------------

    $ ./sudoku.exe save new_path.json

    Current board saved to new_path.json

    ```

board
move
invalid move
hint
solve


5. **Also include a brief list of what order you will implement features.**
    1. Initially we will work on our Sududo library that is the interface for updating and generating sudoku boards.
    2. Then we'll add tests to ensure everything works as expected. 
    3. Then we will work on out command-line utility
    4. See **1.** above for additional 

6. **If your project is an OCaml version of some other app in another language or a projust you did in another course etc please cite this other project. In general any code that inspired your code needs to be cited in your submissions.**  
    _None_.

7. **You may also include any other information which will make it easier to understand your project.**  
    See [Wikipedia](https://en.wikipedia.org/wiki/Sudoku).