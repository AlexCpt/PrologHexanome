:- dynamic board/1. 

play(_,VictoryPlayer,_) :- gameover(Winner), !, displayBoard, writeln(''), write('Game is over. Winner is : '), writeln(Winner), VictoryPlayer = Winner.

play(Player,VictoryPlayer,Mode) :-
board(Board),
displayBoard,
writeln(''),
write(Player), writeln(', its your turn ! <3'), writeln(''), writeln(''), 
ia(Board, Player, 4, IndexCol, Mode),
isMoveValid(IndexCol, IndexRow, Board),
playMove(Board, IndexCol, IndexRow, NewBoard, Player),
applyIt(Board, NewBoard),
changePlayer(Player, NextPlayer),
play(NextPlayer, VictoryPlayer, Mode).


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

%IA Heuristique
heuristique_signe(Board, JoueurActuel, Profondeur, ReturnScore) :-
    Res is Profondeur mod 2,
    Res == 0,
    heuristique(Board, JoueurActuel, Score),
    changePlayer(JoueurActuel, NextJoueur),
    heuristique(Board, NextJoueur, ScoreAdv),
    ReturnScore is (Score - ScoreAdv).

heuristique_signe(Board, Adversaire, Profondeur, ReturnScore) :-
    Res is Profondeur mod 2,
    Res == 1,
    heuristique(Board, Adversaire, ScoreAdv),
    changePlayer(Adversaire, NextJoueur),
    heuristique(Board, NextJoueur, Score),
    ReturnScore is (Score - ScoreAdv).
                         
ia_interne(Board, JoueurActuel, Profondeur, ProfondeurMax, ReturnScore) :-
    Profondeur >= ProfondeurMax,
    heuristique_signe(Board, JoueurActuel, Profondeur, ReturnScore).

ia_interne(Board, JoueurActuel, Profondeur, _, ReturnScore) :-
    winner(Board, _),
    heuristique_signe(Board, JoueurActuel, Profondeur, ReturnScore).

ia_interne(Board, JoueurActuel, Profondeur, ProfondeurMax, ReturnScore) :-
    Res is Profondeur mod 2,
    Res == 0,
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 0, Score0),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 1, Score1),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 2, Score2),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 3, Score3),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 4, Score4),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 5, Score5),
    jouerCoupIA(Board, JoueurActuel, Profondeur, ProfondeurMax, 6, Score6),

    
    ListeScore = [Score0, Score1, Score2, Score3, Score4, Score5, Score6],
    max_list(ListeScore, ReturnScore).    

ia_interne(Board, JoueurActuel, Profondeur, ProfondeurMax, ReturnScore) :-
    Res is Profondeur mod 2,
    Res == 1,
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

jouerCoupIA(_, _, Profondeur, _, _, ReturnScore) :-
    Res is Profondeur mod 2,
    Res == 0,
    ReturnScore = (-1000).

jouerCoupIA(_, _, Profondeur, _, _, ReturnScore) :-
    Res is Profondeur mod 2,
    Res == 1,
    ReturnScore = 1000.

%-------------------------------
%IA TOUT RANDOM -> MODE 1
ia(Board, _, _, ReturnCol,1):-repeat, ReturnCol is random(7), isMoveValid(ReturnCol, _, Board),!.
%--------------------------------


%--------------------------------
% IA Victoire Immédiate / Contre et 'o' random-> MODE 2
% 
% Victoire immédiate
ia(Board,'x',_,ReturnCol,2):-isMoveValid(ReturnCol,IndexRow,Board), copy_term(Board, NewBoardIA),playMove(NewBoardIA,ReturnCol,IndexRow,NewBoardTest,'o'), winner(NewBoardTest,'o'),!.

%Contrer la victoire de l'adversaire
ia(Board,'x',_,ReturnCol,2):-isMoveValid(ReturnCol,IndexRow,Board),copy_term(Board, NewBoardIA), changePlayer('o',NextPlayer), playMove(NewBoardIA,ReturnCol,IndexRow,NewBoardTest,NextPlayer), winner(NewBoardTest,NextPlayer),!.

%O en Random
ia(Board, _, _, ReturnCol,2):-repeat, ReturnCol is random(7), isMoveValid(ReturnCol, _, Board),!.
%%--------------------------------

%%--------------------------------
%IA Heuristique arbre MinMax
%
% Victoire immédiate -> O
ia(Board,'o',_,ReturnCol,3):-isMoveValid(ReturnCol,IndexRow,Board), copy_term(Board, NewBoardIA),playMove(NewBoardIA,ReturnCol,IndexRow,NewBoardTest,'o'), winner(NewBoardTest,'o'),!.

%Contrer la victoire de l'adversaire -> O
ia(Board,'o',_,ReturnCol,3):-isMoveValid(ReturnCol,IndexRow,Board),copy_term(Board, NewBoardIA), changePlayer('o',NextPlayer), playMove(NewBoardIA,ReturnCol,IndexRow,NewBoardTest,NextPlayer), winner(NewBoardTest,NextPlayer),!.

% Random -> O
ia(Board, 'o', _, ReturnCol,3):-repeat, ReturnCol is random(7), isMoveValid(ReturnCol, _, Board),!.

%X en heuristique
ia(Board, 'x', ProfondeurMax, ReturnCol,3) :-
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
%%%--------------------------------
                                   

isElemVar(Col,Row,Board):-elemBoard(Col, Row, Board, Elem),var(Elem).
isColFullBelow(Col,Row,Board):-elemBoard(Col, Row-1, Board, ElemOver),nonvar(ElemOver).
isGroundCol(_,0,_).

isMoveValid(Col,Row,Board):-isElemVar(Col,Row,Board),isColFullBelow(Col,Row,Board).
isMoveValid(Col,Row,Board):-isElemVar(Col,Row,Board),isGroundCol(Col,Row,Board).
                                       
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
    Score = 200.

heuristique(Board,Player,Score):-
    troisPionsAligneCol1(Board,Player, Score1),
    troisPionsAligneCol2(Board,Player, Score2),
    troisPionsAligneRow1(Board,Player, Score3),
    troisPionsAligneRow2(Board,Player, Score4),
    troisPionsAligneDiag11(Board,Player, Score5),
    troisPionsAligneDiag12(Board,Player, Score6),
    troisPionsAligneDiag21(Board,Player, Score7),
    troisPionsAligneDiag22(Board,Player, Score8),
    Score is min(Score1 + Score2 + Score3 + Score4 + Score5 + Score6 + Score7 + Score8, 20).
heuristique(_, _, Score) :- Score = 1.


troisPionsAligneCol1(Board, P, Score) :- elemBoard(Col1, Row, Board, Z1),
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
                    isMoveValid(Col4, Row, Board),
    				!, Score = 10. % row

troisPionsAligneCol1(_, _, Score) :- Score = 0.

troisPionsAligneCol2(Board, P, Score) :- elemBoard(Col1, Row, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    				Col1 =< 4,
    				Col1 >= 1,
                    Col2 is Col1+1,
                    elemBoard(Col2, Row, Board, Z2),
    Z2 == P,
                    Col3 is Col2+1,
                    elemBoard(Col3, Row, Board, Z3),
    Z3 == P,
                    Col4 is Col1-1,
                    isMoveValid(Col4, Row, Board),
    				!, Score = 10. % row

troisPionsAligneCol2(_, _, Score) :- Score = 0.

troisPionsAligneRow1(Board, P, Score) :- elemBoard(Col, Row1, Board, Z1),
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
                    isMoveValid(Col, Row4, Board),
    				!, Score = 10. % row

troisPionsAligneRow1(_, _, Score) :- Score = 0.

troisPionsAligneRow2(Board, P, Score) :- elemBoard(Col, Row1, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    				Row1 =< 3,
                    Row2 is Row1+1,
                    elemBoard(Col, Row2, Board, Z2),
    Z2 == P,
                    Row3 is Row2+1,
                    elemBoard(Col, Row3, Board, Z3),
    Z3 == P,
                    Row4 is Row3-1,
                    isMoveValid(Col, Row4, Board),
    				!, Score = 10. % row

troisPionsAligneRow2(_, _, Score) :- Score = 0.

troisPionsAligneDiag11(Board, P, Score) :- elemBoard(Col1, Row1, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    				Row1 =< 2,
    				Col1 =< 3,
                    Row2 is Row1+1,
    				Col2 is Col1+1,
                    elemBoard(Col2, Row2, Board, Z2),
    Z2 == P,
                    Row3 is Row2+1,
    				Col3 is Col2+1,
                    elemBoard(Col3, Row3, Board, Z3),
    Z3 == P,
                    Row4 is Row3+1,
    				Col4 is Col3+1,
                    isMoveValid(Col4, Row4, Board),
    				!, Score = 10. % row

troisPionsAligneDiag11(_, _, Score) :- Score = 0.

troisPionsAligneDiag12(Board, P, Score) :- elemBoard(Col1, Row1, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    				Row1 =< 3,
    				Col1 =< 4,
                    Row2 is Row1+1,
    				Col2 is Col1+1,
                    elemBoard(Col2, Row2, Board, Z2),
    Z2 == P,
                    Row3 is Row2+1,
    				Col3 is Col2+1,
                    elemBoard(Col3, Row3, Board, Z3),
    Z3 == P,
                    Row4 is Row3-1,
    				Col4 is Col3-1,
                    isMoveValid(Col4, Row4, Board),
    				!, Score = 10. % row

troisPionsAligneDiag12(_, _, Score) :- Score = 0.

troisPionsAligneDiag21(Board, P, Score) :- elemBoard(Col1, Row1, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    				Row1 =< 2,
    				Col1 >= 3,
                    Row2 is Row1+1,
    				Col2 is Col1-1,
                    elemBoard(Col2, Row2, Board, Z2),
    Z2 == P,
                    Row3 is Row2+1,
    				Col3 is Col2-1,
                    elemBoard(Col3, Row3, Board, Z3),
    Z3 == P,
                    Row4 is Row3+1,
    				Col4 is Col3-1,
                    isMoveValid(Col4, Row4, Board),
    				!, Score = 10. % row

troisPionsAligneDiag21(_, _, Score) :- Score = 0.

troisPionsAligneDiag22(Board, P, Score) :- elemBoard(Col1, Row1, Board, Z1),
                    nonvar(Z1),
    				P = Z1,
    				Row1 =< 3,
    				Row1 >= 1,
    				Col1 >= 2,
    				Col1 =< 5,
                    Row2 is Row1+1,
    				Col2 is Col1-1,
                    elemBoard(Col2, Row2, Board, Z2),
    Z2 == P,
                    Row3 is Row2+1,
    				Col3 is Col2-1,
                    elemBoard(Col3, Row3, Board, Z3),
    Z3 == P,
                    Row4 is Row3-1,
    				Col4 is Col3+1,
                    isMoveValid(Col4, Row4, Board),
    				!, Score = 10. % row

troisPionsAligneDiag22(_, _, Score) :- Score = 0.

xisWinner('x',VictoireX, NewVictoire):-
	NewVictoire is VictoireX + 1.
xisWinner('o',VictoireX, NewVictoire):-
	NewVictoire is VictoireX.


%Mode 1 -> TOUT RANDOM
%Mode 2 -> X repère les alignements de 3 pour gagner ou contrer mais sinon random / O random
%Mode 3 -> X MinMax et O repère les alignements de 3 pour gagner ou contrer mais sinon random

%Fait NbTot parties en mode Mode et affiche le nombre de victoires de 'x'
%Appel : statistic(0,NbTot,0,Mode).
statistic(NbActu,NbTot,VictoireX,Mode):-
    NbActu<NbTot,
    NouvNbActu is NbActu + 1,
    init(X,Mode),
    xisWinner(X,VictoireX,NewVictoire),
    write(NewVictoire),
    write('sur'),
    write(NouvNbActu),

    statistic(NouvNbActu,NbTot,NewVictoire, Mode).


%%%%% Start the game ! 
init(VictoryPlayer, Mode):- length(Board,42), assert(board(Board)), play('x', VictoryPlayer,Mode), retract(board(Board)).