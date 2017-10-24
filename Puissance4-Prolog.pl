:- dynamic board/1. 

play(_) :- gameover(Winner), !, displayBoard, writeln(''), write('Game is over. Winner is : '), writeln(Winner).

play(Player) :-
board(Board),
displayBoard,
writeln(''),
write(Player), writeln(', its your turn ! <3'), writeln(''), writeln(''), 
ia(Board, IndexCol, IndexRow, Player,NextPlayer, 0),
playMove(Board, IndexCol, IndexRow, NewBoard, Player),
applyIt(Board, NewBoard),
changePlayer(Player, NextPlayer),
play(NextPlayer).


% Retourne l'élément à la position [IndCol, IndRow]
elemBoard(IndCol, IndRow, Board, Elem) :- nonvar(IndCol), nonvar(IndRow), Index is IndCol * 6 + IndRow, var(Elem), !, nth0(Index, Board, Elem).
% Retourne IndCol et IndRow pour un élément Elem
elemBoard(IndCol, IndRow, Board, Elem) :- nth0(Index, Board, Elem), IndCol is div(Index, 6), IndRow is mod(Index, 6).

playMove(Board,Col,Row,NewBoard,Player) :- Board=NewBoard, elemBoard(Col,Row,NewBoard,Player).
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).

%%%% Recursive predicate that checks if all the elements of the List (a board) 
%%%% are instanciated: true e.g. for [x,x,o,o,x,o,x,x,o] false for [x,x,o,o,_G125,o,x,x,o]
isBoardFull([]).
isBoardFull([H|T]):- nonvar(H), isBoardFull(T).

%%%% Test is the game is finished %%%
gameover(Winner) :- board(Board), winner(Board,Winner), !.  % There exists a winning configuration: We cut!
gameover('Draw') :- board(Board), isBoardFull(Board). % the Board is fully instanciated (no free variable): Draw.

winner(Board, P) :- elemBoard(Col1, Row, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    				Col1 =< 3,
                    Col2 is Col1+1,
                    elemBoard(Col2, Row, Board, Z2),
    Z2 == P,
                    Col3 is Col2+1,
                    elemBoard(Col3, Row, Board, Z3),
    Z3 == P,
                    Col4 is Col3+1,
                    elemBoard(Col4, Row, Board, Z4),
    Z4 == P, !. % row

winner(Board, P) :- elemBoard(Col, Row1, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    				Row1 =< 2,
                    Row2 is Row1+1,
                    elemBoard(Col, Row2, Board, Z2),
    Z2 == P,
                    Row3 is Row2+1,
                    elemBoard(Col, Row3, Board, Z3),
    Z3 == P,
                    Row4 is Row3+1,
                    elemBoard(Col, Row4, Board, Z4),
    Z4 == P, !. % col

winner(Board, P) :- elemBoard(Col1, Row1, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    Col1 =< 3,
    Row1 =< 2,
                    Col2 is Col1+1,
    
                    Row2 is Row1+1,
    
                    elemBoard(Col2, Row2, Board, Z2),
    Z2 == P,
                    Col3 is Col2+1,
                    Row3 is Row2+1,
                    elemBoard(Col3, Row3, Board, Z3),
    Z3 == P,
                    Col4 is Col3+1,
                    Row4 is Row3+1,
                    elemBoard(Col4, Row4, Board, Z4),
    Z4 == P, !. % diag1

winner(Board, P) :- elemBoard(Col1, Row1, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
      				Col1 =< 3,
    				Row1 >= 3,
                    Col2 is Col1+1,
                    Row2 is Row1-1,
    
                    elemBoard(Col2, Row2, Board, Z2),
    Z2 == P,
                    Col3 is Col2+1,
                    Row3 is Row2-1,
                    elemBoard(Col3, Row3, Board, Z3),
    Z3 == P,
                    Col4 is Col3+1,
                    Row4 is Row3-1,
                    elemBoard(Col4, Row4, Board, Z4),
    Z4 == P, !. % diag2

%%%% Predicate to get the next player
changePlayer('x','o').
changePlayer('o','x').

%ia %%Rajouter score à remonter dans la récursion pour faire min max
%ia(_,_,_,_,2):- 
%
%%Soucis stop juste quand winner alros qu'on veut faire toutes les sommes

%Victoire immédiate
%ia(Board, IndexCol, IndexRow,'x',_,_):-isMoveValid(IndexCol,IndexRow,Board), copy_term(Board, NewBoardIA),playMove(NewBoardIA,IndexCol,IndexRow,NewBoardTest,Player), winner(NewBoardTest,Player),!.

%ia victoire de l'adversaire immédiate :
%ia(Board, IndexCol, IndexRow,'x',NextPlayer,_):-isMoveValid(IndexCol,IndexRow,Board),copy_term(Board, NewBoardIA), changePlayer(Player,NextPlayer),playMove(NewBoardIA,IndexCol,IndexRow,NewBoardTest,NextPlayer),winner(NewBoardTest,NextPlayer),!.

%IA Heuristique
ia(Board, IndexCol, IndexRow,'o',_,1, Score):-isMoveValid(IndexCol,IndexRow,Board),
    														copy_term(Board, NewBoardIA),playMove(NewBoardIA,IndexCol,IndexRow,NewBoardTest,'o'),
    														heuristic(NewBoardTest, 'o', OtherScore),
															Score is -OtherScore.
ia(Board, IndexCol, IndexRow,'o',_,_):-repeat, IndexCol is random(7), IndexRow is random(6), isMoveValid(IndexCol,IndexRow,Board),!.

ia(Board, IndexCol, IndexRow,'x',_,0):-	
%    														scoreMax(-1000),
    
 %   														repeat,
  %  															isMoveValid(IndexColCurrent,IndexRowCurrent,Board), 
   % 															copy_term(Board, NewBoardIA),
    %															playMove(NewBoardIA,IndexColCurrent, IndexRowCurrent,NewBoardTest,'x'),
    %															changePlayer('x', NextPlayer),
   	%															ia(NewBoardTest, _, _,NextPlayer,'x',1,Score),
    %															Score > scoreMax,
    %															scoreMax(Score),
    %															IndexCol = IndexColCurrent,
     %         												IndexRow = IndexRowCurrent.
              											
    
	bestMove(Board,IndexCol,IndexRow,_,-1000,[]).
              												
getRandomValidMove(Board, IndexCol, IndexRow) :- repeat, IndexCol is random(7), IndexRow is random(6), isMoveValid(IndexCol,IndexRow,Board),!.


%Best Move
bestMove(Board, BestCol, BestRow, NewScoreMax, OldScoreMax, CoupsEffectues) :-
  getRandomValidMove(Board, Col, Row),
  not(member(Col, CoupsEffectues)),

    append(CoupsEffectues, [Col], NewCoupsEffectues),

  bestMove(Board, BestCol, BestRow, InterScoreMax, OldScoreMax, NewCoupsEffectues),

  copy_term(Board, NewBoard),
  playMove(NewBoard, Col, Row, NewBoardAfterMove, 'x'),
  %changePlayer('x', NextPlayer),
  heuristic(NewBoardAfterMove, 'x', Score),

  NewScoreMax is max(Score, InterScoreMax),
  Score > InterScoreMax,
  BestCol = Col,
  BestRow = Row.

bestMove(_, _, _, NewScoreMax, _, _) :-
  NewScoreMax = (-1000).



%Niv intermédiaire
%ia(Board, IndexCol, IndexRow,'x',NextPlayer,Profondeur,Score):-isMoveValid(IndexCol,IndexRow,Board),NewProfondeur is Profondeur+1, 
 %   														copy_term(Board, NewBoardIA),playMove(NewBoardIA,IndexCol,IndexRow,NewBoardTest,Player),
  %  														ia(NewBoardTest, IndexCol, IndexRow,Player,NextPlayer,NewProfondeur,Score).
    

%%IA random
                                   

isElemVar(Col,Row,Board):-elemBoard(Col, Row, Board, Elem),var(Elem).
isColFullBelow(Col,Row,Board):-elemBoard(Col, Row-1, Board, ElemOver),nonvar(ElemOver).
isGroundCol(_,0,_).

isMoveValid(Col,Row,Board):-isElemVar(Col,Row,Board),isColFullBelow(Col,Row,Board).
isMoveValid(Col,Row,Board):-isElemVar(Col,Row,Board),isGroundCol(Col,Row,Board).
                                       
%rowInColValidMove([],0,Board).
%rowInColValidMove([HCol|T],R,Board):-nonvar(HCol),rowInColValidMove(T,A,Board,Res), R is A+1.

printVal(N) :- board(B), nth0(N,B,Val), var(Val), write('_ '), !.
printVal(N) :- board(B), nth0(N,B,Val), write(Val), write(' ').

displayBoard:-
    printVal(5), printVal(11), printVal(17),printVal(23),printVal(29),printVal(35),printVal(41), writeln(''),
    printVal(4), printVal(10), printVal(16),printVal(22),printVal(28),printVal(34),printVal(40), writeln(''),
    printVal(3), printVal(9), printVal(15),printVal(21),printVal(27),printVal(33),printVal(39), writeln(''),
    printVal(2), printVal(8), printVal(14),printVal(20),printVal(26),printVal(32),printVal(38), writeln(''),
    printVal(1), printVal(7), printVal(13),printVal(19),printVal(25),printVal(31),printVal(37), writeln(''),
    printVal(0), printVal(6), printVal(12),printVal(18),printVal(24),printVal(30),printVal(36), writeln('').

heuristic(Board, Player, Score) :- winner(Board, Player), !, Score = 100.
heuristic(_, _, Score) :- Score = 1.

%%%%% Start the game! 
init :- length(Board,42), assert(board(Board)), play('x').