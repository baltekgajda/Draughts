# Draughts with Minimax algorithm
<img align="right" src="../master/src/main/resources/images/DraughtsBoard.png" width="400">
Program that allows you to play draughts against the computer, containing implementation 
of minimax algorithm with alpha-beta pruning.

## How to play
Before starting the game player can choose between three difficulty levels (easy/medium/hard). Each difficulty level means different 
maximum depth of the tree generated by minimax algorithm.
To move a piece, click on a chosen one and drag to another grid cell. The program will automatically align the piece inside the cell. If 
the move is not allowed, the piece will be returned to it's previous cell.
## Rules
Game is played by two opponents - the player and the computer, on opposite sides of the gameboard. 
The player has red pieces; computer has black pieces. Players alternate turns. A player may not move an opponent's piece.
A piece may move only diagonally into an unoccupied square. The player, who has no remaining pieces or cannot move due to being blocked, 
loses the game.
<!--Capturing containing maximum number of pieces is mandatory.-->
## Documentation
You can check out project's full documentation [here](https://baltekgajda.github.io/Draughts/)
