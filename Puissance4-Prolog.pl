:- dynamic board/1. 

play(_) :- gameover(Winner), !, displayBoard, writeln(''), write('Game is over. Winner is : '), writeln(Winner).

play(Player) :-
board(Board),
displayBoard,
writeln(''),
write(Player), writeln(', its your turn ! <3'), writeln(''), writeln(''), 
ia(Board, Player, 2, IndexCol),
isMoveValid(IndexCol, IndexRow, Board),
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

%Victoire immédiate
%ia(Board, IndexCol, IndexRow,'x',_,_):-isMoveValid(IndexCol,IndexRow,Board), copy_term(Board, NewBoardIA),playMove(NewBoardIA,IndexCol,IndexRow,NewBoardTest,Player), winner(NewBoardTest,Player),!.

%ia victoire de l'adversaire immédiate :
%ia(Board, IndexCol, IndexRow,'x',NextPlayer,_):-isMoveValid(IndexCol,IndexRow,Board),copy_term(Board, NewBoardIA), changePlayer(Player,NextPlayer),playMove(NewBoardIA,IndexCol,IndexRow,NewBoardTest,NextPlayer),winner(NewBoardTest,NextPlayer),!.

%IA Heuristique

heuristique_signe(Board, JoueurActuel, Profondeur, ReturnScore) :-
    Res is Profondeur mod 2,
    Res == 1,
    heuristique(Board, JoueurActuel, ReturnScore).

heuristique_signe(Board, JoueurActuel, Profondeur, ReturnScore) :-
    Res is Profondeur mod 2,
    Res == 0,
    changePlayer(JoueurActuel, NextJoueur),
    heuristique(Board, NextJoueur, ScoreNegatif),
    ReturnScore is -ScoreNegatif.
                         
ia_interne(Board, JoueurActuel, Profondeur, ProfondeurMax, ReturnScore) :-
    Profondeur >= ProfondeurMax,
    heuristique_signe(Board, JoueurActuel, Profondeur, ReturnScore).

ia_interne(Board, JoueurActuel, Profondeur, _, ReturnScore) :-
    winner(Board, _),
    heuristique_signe(Board, JoueurActuel, Profondeur, ReturnScore).

ia_interne(Board, JoueurActuel, Profondeur, ProfondeurMax, ReturnScore) :-
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 0, Score0),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 1, Score1),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 2, Score2),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 3, Score3),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 4, Score4),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 5, Score5),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 6, Score6),

    
    ListeScore = [Score0, Score1, Score2, Score3, Score4, Score5, Score6],
    min_list(ListeScore, ReturnScore).    
    
jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, Col, ReturnScore) :-
    isMoveValid(Col, Row, Board), 
    copy_term(Board, NewBoard),
    playMove(NewBoard, Col, Row, BoardAfterMove, JoueurActuel),
    NewProfondeur is Profondeur + 1,
    changePlayer(JoueurActuel, NextJoueur),
   	ia_interne(BoardAfterMove, NextJoueur, NewProfondeur, ProfondeurMax, ReturnScore).

jouerCoupIA(_, _, 0, _, _, ReturnScore) :-
    ReturnScore = (-1000).

jouerCoupIA(_, _, _, _, _, ReturnScore) :-
    ReturnScore = 1000.

%%IA random
ia(Board, 'o', _, ReturnCol):-repeat, ReturnCol is random(7), isMoveValid(ReturnCol, _, Board),!.

ia(Board, JoueurActuel, ProfondeurMax, ReturnCol) :-
    jouerCoupIA(Board, JoueurActuel, 0, ProfondeurMax, 0, Score0),
    jouerCoupIA(Board, JoueurActuel, 0, ProfondeurMax, 1, Score1),
    jouerCoupIA(Board, JoueurActuel, 0, ProfondeurMax, 2, Score2),
    jouerCoupIA(Board, JoueurActuel, 0, ProfondeurMax, 3, Score3),
    jouerCoupIA(Board, JoueurActuel, 0, ProfondeurMax, 4, Score4),
    jouerCoupIA(Board, JoueurActuel, 0, ProfondeurMax, 5, Score5),
    jouerCoupIA(Board, JoueurActuel, 0, ProfondeurMax, 6, Score6),
    
    ListeScore = [Score0, Score1, Score2, Score3, Score4, Score5, Score6],
    max_list(ListeScore, ReturnScore),
    nth0(ReturnCol, ListeScore, ReturnScore).

                                   

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

heuristique(Board, Player, Score) :- winner(Board, Player), !,
    Score = 100.
heuristique(_, _, Score) :- Score = 1.

%%%%% Start the game! 
init :- length(Board,42), assert(board(Board)), play('x').