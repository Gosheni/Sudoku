
# Game API

## Start Game
- **Endpoint**: `GET /api/v1/initialize?difficulty=[Int]&username=[String]`
- **Returns**: 
  - 200: A board in JSON format.

## Do Move
- **Endpoint**: `GET /api/v1/move?x=[Int]&y=[Int]&move=[Int]`
- **Returns**: 
  - 200: An updated board in JSON format.
  - 405: An error message to be displayed.
- **Notes**: If this move solves the board, a high score will be added.

## Get Hint
- **Endpoint**: `GET /api/v1/hint`
- **Returns**: 
  - 200: A JSON containing hint details.
  - 405: An error message to be displayed.

## Get Solution
- **Endpoint**: `GET /api/v1/solve`
- **Returns**: 
  - 200: A board in JSON format.

# Highscores API

## Get Highscores
- **Endpoint**: `GET /api/v1/highscores?difficulty=[Int]`
- **Returns**: A JSON-encoded list where each element contains a username and the number of seconds it took for the player.

# Other API

## Submit Board
- **Endpoint**: `POST /api/v1/submit?username=[String]`
- **Payload**: A dictionary containing a JSON-encoded board and a username.
- **Returns**: 
  - 200: An updated board in JSON format.
  - 405: An error message to be displayed.
