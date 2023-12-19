
# Game API

## Start Game
- **Endpoint**: `GET /api/v1/initialize?difficulty=[Int]`
- **Returns**: 
  - 200: A board in JSON format.
- **Notes**: 
  - Difficulty is optional, default is 50

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

# Highscores API
## Get Highscores
- **Endpoint**: `GET /api/v1/highscores?difficulty=[Int]`
- **Returns**: 
  - 200 : A JSON object with two elements, `recent` which contains the most recent highscore, and `top10` which constains up to 10 highscores. Each highscore constains a username and the number of seconds it took for the player.

## Submit username
- **Endpoint**: `Get /api/v1/submit?username=[String]`
- **Returns**:
  - 200: See `highscores`
