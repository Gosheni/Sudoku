
## Game 
### Start game
- GET: /api/v1/initialize?difficulty=[Int]&username=[String]
- Returns: 200: A board in JSON format. 
### Do move
- GET: /api/v1/move?x=[Int]&y=[Int]&move=[Int]  
    `move`is optional. If it is not included the server will interpret this as a request to remove an element. 
- Returns: 
    - 200: An updated board in JSON format.
    - 405: An error message to be displayed
- Notes: If this solves the board, a highscore will be added.
### Get a hint 
- GET: /api/v1/hint
- Returns: 
    - 200: A json containing hint details
    - 405: An error message to be displayed
### Get solution
- GET: /api/v1/solve
- Returns: 200: A board in JSON format

## Highscores
### Get highscores 
- GET: /api/v1/highscores?difficulty=[Int]
- Returns a JSON encoded list where each element contains a username and the number of seconds it took for the player.

## Other 
### Submit board 
- POST: /api/v1/submit
    The payload has to be a dictionary containing a JSON encoded board and a username. 
- Returns: 
    - 200: An updated board in JSON format.
    - 405: An error message to be displayed