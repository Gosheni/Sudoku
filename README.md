# Overview

We implement the game Sudoku including:

   - A solver. 

   - A board generator.  

   - A high score system based on time taken to complete a game.  

   - A hint system    
      - Hint system uses non-backtracking sudoku solvers to guarantee the hint is something a perfect player could come up with, with the info already on the board  
      - Hint system also explain what information a player would look at to arrive at the move being hinted.  
      - If no determinate moves can be found, hint system recommends guesses with the least variability. 
      - For details of the algorithm used by our hint system ([see linked paper](https://www.ams.org/notices/200904/rtx090400460p.pdf)).  

   - Functionality to save and load boards, of the users desired difficulty.

   - We offer two different ways to play the game:
		- through a command line interface (CLI).
		- through a web server built in Dream. Has a few additional features:
			- visual representations of the information given in each hint
			- the ability to interact with the game through both clicking and keyboard input
			- the visible timer and the ability to pause the game when needed.


# Installation and Usage

   - Given opam and ocaml version 5.0.0, running the following installs all required packages:
	```opam install . --deps-only --working-dir```

   - How to run (web server):
	```dune exec server/server.exe```

   - How to play on web server:
		- Click on a square on the board to highlight it, then click on the number board below the main game to add a number
		- Alternatively, move the highlighted square with arrow keys and input numbers with 1-9 on the keyboard (or try to delete with backspace)
		- Upon completing a game, enter the game name using the text prompt to record the score.
		- Use the buttons on the side to get hints, create a new game or pause the game.
	
   - How to run (CLI):
	```dune exec cli/sudoku.exe [COMMAND] [OPTIONS]```
	
   - List of CLI commands with options:
		- init [DIFFICULTY] NAME
			- creates a new game with the given NAME, optionally takes an integer 1-70 determining how difficult the game will be (higher means more difficult)
			- prints the newly created board
		- move VALUE ROW COL
			- tries to place value at the specified row and column
			- if move is valid, updates the board and prints the new one
			- if move is invalid, tells the user why
		- remove ROW COL
			- tries to remove the value at the specified row and column
			- if cell is not fixed, updates the board and prints the new one
			- otherwise, tells the user that is invalid
		- hint
			- gives the user a hint. This can be either:
				- telling the user there is a mistake somewhere (board is no longer solvable)
				- telling the user to make a guess
				- telling the user a suggested move
					- suggested moves are always logically reachable and don't require guessing to reach them. 
					- suggested moves also include a short explanation of how that move can be arrived at logically
		- solve
			- solves the board and prints the solution for the user
		- scores
			- prints a list of high scores (name of the game and the time taken)
			- also prints the most recent score
		- print
			- prints the current board
		- list
			- prints the list of currently available games
		- load NAME
			- loads the game with NAME, so any moves, hints etc are now done on the loaded board.
			- to go back to the previous game, need to call load again

# Further resources
   See [Wikipedia](https://en.wikipedia.org/wiki/Sudoku) for a description of the game.