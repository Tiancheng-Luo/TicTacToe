:- use_module(library(clpfd)).
%
% Simple demo that constrains a list to be the pairwise sum of two other
% lists
%




startGame:- chance(['y', 'y', 'y', 'y', 'y', 'y', 'y', 'y', 'y'], 'x').


chance(Board, 'x'):- 
    displayTicTaToe(Board), 
    (
     checkIfWon(Board, 'x'), !, write("THE PLAYER HAS WON AGAINST THE COMPUTER\n");
     checkIfWon(Board, 'o'), !, write("THE COMPUTER HAS WON AGAINST THE PLAYER\n");
     neg(checkIfMovesLeft(Board)), !, write("THE GAME IS A DRAW.\n");
     write("Make Your Move : "), nl, read(Input), nl, replace(Board, Input, 'x', NewBoard), chance(NewBoard, 'o')
    ).
    
chance(Board, 'o'):- 
    displayTicTaToe(Board), 
    (
     checkIfWon(Board, 'x'), !, write("THE PLAYER HAS WON AGAINST THE COMPUTER\n");
     checkIfWon(Board, 'o'), !, write("THE COMPUTER HAS WON AGAINST THE PLAYER\n");
     neg(checkIfMovesLeft(Board)), !, write("THE GAME IS A DRAW.\n");
     write("Make Your Goddamn Move : "), nl, read(Input), nl, replace(Board, Input, 'o', NewBoard), chance(NewBoard, 'x')
    ).
    
     % findBestMove(Board, Position), replace(Board, Position, 'o', NewBoard), chance(NewBoard, 'x')


% Minmax Algo.

evaluate(Board, Answer):-
    (
     checkRow(Board, 'o'), !, Answer = 10;
     checkRow(Board, 'x'), !, Answer = -10;
     checkCol(Board, 'o'), !, Answer = 10;
     checkCol(Board, 'x'), !, Answer = -10;
     checkDiagonal(Board, 'o'), !, Answer = 10;
     checkDiagonal(Board, 'x'), !, Answer = -10;
     Answer = 0
    ).


minimax(Board, false, Score):-
    (
     evaluate(Board, 10), !, Score = 10;
     evaluate(Board, -10), !, Score = -10;
     neg(checkIfMovesLeft(Board)), !, Score = 0;
     Best = -1000, 
     
    )
    

% Predicates for checking winning


checkIfWon(List, X):- checkRow(List, X); checkCol(List, X); checkDiagonal(List, X). 
checkIfMovesLeft(List):- member('y', List).

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















% Drawing the Tic-Tac-Board


    displayTicTaToe([X1, X2, X3, X4, X5, X6, X7, X8, X9]):- 
displayEmptyLine, write(" "), write(X1), write(" |"), write(" "), write(X2), write(" |"), write(" "), write(X3), displayBorderLine, 
displayEmptyLine, write(" "), write(X4), write(" |"), write(" "), write(X5), write(" |"), write(" "), write(X6), displayBorderLine, 
displayEmptyLine, write(" "), write(X7), write(" |"), write(" "), write(X8), write(" |"), write(" "), write(X9), write("\n"), displayEmptyLine.


    
    displayEmptyLine:- write("   |   |   \n").
    displayBorderLine:- write("\n___|___|___\n").
    displayCharacterLine(X, Pos):- write(" "), write(X), write(" ").
    
    
