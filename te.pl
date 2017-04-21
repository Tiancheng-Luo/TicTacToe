:- use_module(library(clpfd)).
%
% Simple demo that constrains a list to be the pairwise sum of two other
% lists
%




play:- chance([y, y, y, y, y, y, y, y, y], x).


chance(Board, x):- 
    displayTicTaToe(Board), 
    (
     checkIfWon(Board, x), !, write('THE PLAYER HAS WON AGAINST THE COMPUTER\n'), !;
     checkIfWon(Board, o), !, write('THE COMPUTER HAS WON AGAINST THE PLAYER\n'), !;
     neg(checkIfMovesLeft(Board)), !, write('THE GAME IS A DRAW.\n'), !;
     write('Make Your Move : '), nl, read(Input), nl,replace(Board, Input, x, NewBoard),
     (
        (count(NewBoard, y, NoOfY), NoOfY = 8, Input = 4, replace(NewBoard, 0, o, NewBoardFirstChance), chance(NewBoardFirstChance, x)), !;
        (count(NewBoard, y, NoOfY), NoOfY = 8, Input =\= 4, replace(NewBoard, 4, o, NewBoardFirstChance), chance(NewBoardFirstChance, x)), !;
        chance(NewBoard, o)
    )
    ).

chance(Board, o):- 
    displayTicTaToe(Board), 
    (
     checkIfWon(Board, x), write('THE PLAYER HAS WON AGAINST THE COMPUTER\n'),!;
     checkIfWon(Board, o),  write('THE COMPUTER HAS WON AGAINST THE PLAYER\n'),!;
     neg(checkIfMovesLeft(Board)),  write('THE GAME IS A DRAW.\n'),!;
     findBestMove(Board, Position), replace(Board, Position, o, NewBoard), chance(NewBoard, x)
    ).


% Minmax Algo.

evaluate(Board, Answer):-
    (
     checkRow(Board, o), !, Answer = 10;
     checkRow(Board, x), !, Answer = -10;
     checkCol(Board, o), !, Answer = 10;
     checkCol(Board, x), !, Answer = -10;
     checkDiagonal(Board, o), !, Answer = 10;
     checkDiagonal(Board, x), !, Answer = -10;
     Answer = 0
    ).


minimax([P0, P1, P2, P3, P4, P5, P6, P7, P8], Depth, IsMax, Score) :-
    Board = [P0, P1, P2, P3, P4, P5, P6, P7, P8],
    NextDepth is Depth+1,
    (
     evaluate(Board, 10), Score = 10, !;
     evaluate(Board, -10), Score = -10, !;
     neg(checkIfMovesLeft(Board)), Score = 0, !;
     (IsMax == true,
        ((PO == y, replace(Board, 0, o, NewBoard0), minimax(NewBoard0, NextDepth, false, TempScore0));TempScore0 = -1000),
        ((P1 == y, replace(Board, 1, o, NewBoard1), minimax(NewBoard1, NextDepth, false, TempScore1));TempScore1 = -1000),
        ((P2 == y, replace(Board, 2, o, NewBoard2), minimax(NewBoard2, NextDepth, false, TempScore2));TempScore2 = -1000),
        ((P3 == y, replace(Board, 3, o, NewBoard3), minimax(NewBoard3, NextDepth, false, TempScore3));TempScore3 = -1000),
        ((P4 == y, replace(Board, 4, o, NewBoard4), minimax(NewBoard4, NextDepth, false, TempScore4));TempScore4 = -1000),
        ((P5 == y, replace(Board, 5, o, NewBoard5), minimax(NewBoard5, NextDepth, false, TempScore5));TempScore5 = -1000),
        ((P6 == y, replace(Board, 6, o, NewBoard6), minimax(NewBoard6, NextDepth, false, TempScore6));TempScore6 = -1000),
        ((P7 == y, replace(Board, 7, o, NewBoard7), minimax(NewBoard7, NextDepth, false, TempScore7));TempScore7 = -1000),
        ((P8 == y, replace(Board, 8, o, NewBoard8), minimax(NewBoard8, NextDepth, false, TempScore8));TempScore8 = -1000),
        max_list([TempScore0, TempScore1, TempScore2, TempScore3, TempScore4, TempScore5, TempScore6, TempScore7, TempScore8], BestVal),
        Score = BestVal
     ), !;
     (IsMax = false,
        ((PO == y, replace(Board, 0, x, NewBoard0), minimax(NewBoard0, NextDepth, true, TempScore0));TempScore0 = 1000),
        ((P1 == y, replace(Board, 1, x, NewBoard1), minimax(NewBoard1, NextDepth, true, TempScore1));TempScore1 = 1000),
        ((P2 == y, replace(Board, 2, x, NewBoard2), minimax(NewBoard2, NextDepth, true, TempScore2));TempScore2 = 1000),
        ((P3 == y, replace(Board, 3, x, NewBoard3), minimax(NewBoard3, NextDepth, true, TempScore3));TempScore3 = 1000),
        ((P4 == y, replace(Board, 4, x, NewBoard4), minimax(NewBoard4, NextDepth, true, TempScore4));TempScore4 = 1000),
        ((P5 == y, replace(Board, 5, x, NewBoard5), minimax(NewBoard5, NextDepth, true, TempScore5));TempScore5 = 1000),
        ((P6 == y, replace(Board, 6, x, NewBoard6), minimax(NewBoard6, NextDepth, true, TempScore6));TempScore6 = 1000),
        ((P7 == y, replace(Board, 7, x, NewBoard7), minimax(NewBoard7, NextDepth, true, TempScore7));TempScore7 = 1000),
        ((P8 == y, replace(Board, 8, x, NewBoard8), minimax(NewBoard8, NextDepth, true, TempScore8));TempScore8 = 1000),
        min_list([TempScore0, TempScore1, TempScore2, TempScore3, TempScore4, TempScore5, TempScore6, TempScore7, TempScore8], BestVal),
        Score = BestVal
     )
    ).


functionToFindBestMove([P0, P1, P2, P3, P4, P5, P6, P7, P8], Chance):-
   Board = [P0, P1, P2, P3, P4, P5, P6, P7, P8],
   ((PO == y, replace(Board, 0, o, NewBoard0), minimax(NewBoard0, 0, false, TempScore0));TempScore0 = -1000),
   ((P1 == y, replace(Board, 1, o, NewBoard1), minimax(NewBoard1, 0, false, TempScore1));TempScore1 = -1000),
   ((P2 == y, replace(Board, 2, o, NewBoard2), minimax(NewBoard2, 0, false, TempScore2));TempScore2 = -1000),
   ((P3 == y, replace(Board, 3, o, NewBoard3), minimax(NewBoard3, 0, false, TempScore3));TempScore3 = -1000),
   ((P4 == y, replace(Board, 4, o, NewBoard4), minimax(NewBoard4, 0, false, TempScore4));TempScore4 = -1000),
   ((P5 == y, replace(Board, 5, o, NewBoard5), minimax(NewBoard5, 0, false, TempScore5));TempScore5 = -1000),
   ((P6 == y, replace(Board, 6, o, NewBoard6), minimax(NewBoard6, 0, false, TempScore6));TempScore6 = -1000),
   ((P7 == y, replace(Board, 7, o, NewBoard7), minimax(NewBoard7, 0, false, TempScore7));TempScore7 = -1000),
   ((P8 == y, replace(Board, 8, o, NewBoard8), minimax(NewBoard8, 0, false, TempScore8));TempScore8 = -1000),
   ListOfMinMax = [TempScore0, TempScore1, TempScore2, TempScore3, TempScore4, TempScore5, TempScore6, TempScore7, TempScore8],
   max_list(ListOfMinMax, BestVal),
   nth(Index, ListOfMinMax, BestVal),
   Diff is Index - 1,
   Chance = Diff.
   

findBestMove(Board , Answer):- functionToFindBestMove(Board, Chance), Answer = Chance .


% Predicates for checking winning


checkIfWon(List, X):- checkRow(List, X); checkCol(List, X); checkDiagonal(List, X). 
checkIfMovesLeft(List):- member(y, List).

checkRow([X, X, X|_], X).
checkRow([_, _, _, X, X, X|_], X).
checkRow([_, _, _, _, _, _, X, X, X], X).

   
checkCol([X, _, _, X, _, _, X, _, _], X).
checkCol([_, X, _, _, X, _, _, X, _], X).
checkCol([_, _, X, _, _, X, _, _, X], X).

checkDiagonal([X, _, _, _, X, _, _, _, X], X).
checkDiagonal([_, _, X, _, X, _, X, _, _], X).



% Utility Functions

neg(Goal):-  Goal,!,fail. 
neg(Goal).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).













% Drawing the Tic-Tac-Board


    displayTicTaToe([X1, X2, X3, X4, X5, X6, X7, X8, X9]):- 
displayEmptyLine, write(' '), write(X1), write(' |'), write(' '), write(X2), write(' |'), write(' '), write(X3), displayBorderLine, 
displayEmptyLine, write(' '), write(X4), write(' |'), write(' '), write(X5), write(' |'), write(' '), write(X6), displayBorderLine, 
displayEmptyLine, write(' '), write(X7), write(' |'), write(' '), write(X8), write(' |'), write(' '), write(X9), write('\n'), displayEmptyLine.


    
    displayEmptyLine:- write('   |   |   \n').
    displayBorderLine:- write('\n___|___|___\n').
    displayCharacterLine(X, Pos):- write(' '), write(X), write(' ').
    
:- initialization(play).
