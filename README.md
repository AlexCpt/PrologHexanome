# PrologHexanome AI
This project goal was to create an artificial intelligence able to play the ["Connect Four"](https://en.wikipedia.org/wiki/Connect_Four) game.

### Game Rules
It is an really easy game. The Board is made of 7 columns and 6 lines. Each player, one after the other, throw one of his token in the column of his choice. The token will go at the bottom by gravity. 
The first player to get 4 tokens in a vertical, horizontal or diagonal line wins.

### AI N°1
The AI n°1 plays randomly where it is allowed to play.

### AI N°2
In order of importance :

1. When AI n°2 sees that it has already 3 tokens in a line, and that it could put the fourth to win, it will do it.
2. When AI n°2 sees that its opposant has already 3 tokens in a line, and that it could put the fourth to win, it will do it to counter the other player.
3. If there is nothing special, The AI n°2 plays randomly where it is allowed to play.

### AI N°3
The AI n°3 builds the tree of possibility at a depth given in parameters to the `ai` function.

```
ia(Board, Player, Depth, IndexCol, Mode),
```

It will then use an heuristic to determine which situation is the best for it. Then it will decide which is the best play thanks to a **Min-Max algorithm**.

#### Heuristic

The heuristic we used is the following : 
* 200 pts if the game is won
* 10 for each 3 token in line (with a maximum of 20)

###### How to improve Heuristic
* These numbers was determined pretty randomly and could be improved with machine learning. 
* Other heuristics could be find easily.

## How to play

Define the level of depth you want in the ia call in the main loop

```
play(Player,VictoryPlayer,Mode) :-
board(Board),
displayBoard,
writeln(''),
write(Player), writeln(', its your turn ! <3'), writeln(''), writeln(''), 
ia(Board, Player, Depth, IndexCol, Mode),
isMoveValid(IndexCol, IndexRow, Board),
playMove(Board, IndexCol, IndexRow, NewBoard, Player),
applyIt(Board, NewBoard),
changePlayer(Player, NextPlayer),
play(NextPlayer, VictoryPlayer, Mode).
```

Call `init(X, Mode).` X will be unified with the winner player.
1. Mode 1 is IA1(o) vs IA1(x)
2. Mode 2 is IA1(o) vs IA2(x)
3. Mode 3 is IA2(o) vs IA3(x)

## How to do statistics

To do a lot of games and extract statistics we created the `statistic` function. It will do *x* number of games and display how many times the player 'x' won. You have to precise which mode you want to be played too.

Call `statistic(0,x,0,Mode).`



## Results of IA
Feel free to contribute 
