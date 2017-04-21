

/* In This TIC-TAC-GAME
        The Board is represented as a list.
        0 signifies empty slot,
        1 signifies player move slot,
        2 signifies computer move slot,
    
    I used MiniMax Algorithm for the AI.
*/

  
/* The First Predicate That Will Be Called */
play:- chance([0, 0, 0, 0, 0, 0, 0, 0, 0], 1).


/* The chance predicate will detect which player's chance and call the appropriate function */
chance(Board, X):- 
    displayTicTaToe(Board), 
    (
     checkIfWon(Board, 1), write('You Win!\n'), !;  %Checking if player has won
     checkIfWon(Board, 2), write('I Win!\n'), !;    %Checking if computer has won
     neg(checkIfMovesLeft(Board)), write('Draw!\n'), !;     %Checking if game is a draw
     ( X =:= 1, write('Enter row number : '), nl, read_integer(Row), nl, write('Enter column number : '), nl, read_integer(Column), nl, Input is (3*(Row-1)+(Column-1)),
                replace(Board, Input, 1, NewBoard),
        (
            /* On first chance they are 8! possibilities . Due to too many recursions , we get a stackoverflow , Hence we are hard-coding the computer position */
            (count(NewBoard, 0, NoOfY), NoOfY == 8,     %Checking Whether It is First chance
                (
                    ( Input == 4, replace(NewBoard, 0, 2, NewBoardFirstChance), chance(NewBoardFirstChance, 1), !);     %If User Enters In The Middle 
                    ( Input =\= 4, replace(NewBoard, 4, 2, NewBoardFirstChance), chance(NewBoardFirstChance, 1), !)     %If User enters other than the middle 
                ) 
            ), !;
            chance(NewBoard, 2)     %Other chances
        )
     ), !;
     /* Computer Chance */
     ( X =:= 2, findBestMove(Board, Position), replace(Board, Position, 2, NewBoard), chance(NewBoard, 1))      %Finding best move and replacing Tic-Tac-Toe 
    ).


/* MiniMax Algorithm */

/* evaluation function of minimax 
    If computer has won evaluation returns 10
    If player has won evaluation returns -10
    else 0
*/
evaluate(Board, Answer):-
    (
     checkRow(Board, 2), Answer = 10, !;
     checkRow(Board, 1), Answer = -10, !;
     checkCol(Board, 2), Answer = 10, !;
     checkCol(Board, 1), Answer = -10, !;
     checkDiagonal(Board, 2), Answer = 10, !;
     checkDiagonal(Board, 1), Answer = -10, !;
     Answer = 0
    ).

/* minimax function
    If it is maximizer chance , IsMax is true and for all the free slot in the board minimax is recursively called with computer chance at that index.
    If it is minimizer chance , IsMax is false and for all the free slot in the board minimax is recursively called with player chance at that index.
    
    Depth is used for ending the game in the minimum possible steps
*/
minimax([P0, P1, P2, P3, P4, P5, P6, P7, P8], Depth, IsMax, Score) :-
    Board = [P0, P1, P2, P3, P4, P5, P6, P7, P8],
    NextDepth is Depth+1,       %incrementing depth for optimal solution 
    (
     /* Taking care of the edge cases */
     evaluate(Board, 10),  Temp is 10 - Depth , Score = Temp, !;
     evaluate(Board, -10), Temp is (-10) + Depth , Score = Temp, !;
     neg(checkIfMovesLeft(Board)), Score = 0, !;
     (IsMax == true,    %maximizer chance , call recursively for the minimizer chance by changing IsMax to false , If slot is not free return -1000
        ((P0 == 0, replace(Board, 0, 2, NewBoard0), minimax(NewBoard0, NextDepth, false, TempScore0));TempScore0 = -1000),
        ((P1 == 0, replace(Board, 1, 2, NewBoard1), minimax(NewBoard1, NextDepth, false, TempScore1));TempScore1 = -1000),
        ((P2 == 0, replace(Board, 2, 2, NewBoard2), minimax(NewBoard2, NextDepth, false, TempScore2));TempScore2 = -1000),
        ((P3 == 0, replace(Board, 3, 2, NewBoard3), minimax(NewBoard3, NextDepth, false, TempScore3));TempScore3 = -1000),
        ((P4 == 0, replace(Board, 4, 2, NewBoard4), minimax(NewBoard4, NextDepth, false, TempScore4));TempScore4 = -1000),
        ((P5 == 0, replace(Board, 5, 2, NewBoard5), minimax(NewBoard5, NextDepth, false, TempScore5));TempScore5 = -1000),
        ((P6 == 0, replace(Board, 6, 2, NewBoard6), minimax(NewBoard6, NextDepth, false, TempScore6));TempScore6 = -1000),
        ((P7 == 0, replace(Board, 7, 2, NewBoard7), minimax(NewBoard7, NextDepth, false, TempScore7));TempScore7 = -1000),
        ((P8 == 0, replace(Board, 8, 2, NewBoard8), minimax(NewBoard8, NextDepth, false, TempScore8));TempScore8 = -1000),
        max_list([TempScore0, TempScore1, TempScore2, TempScore3, TempScore4, TempScore5, TempScore6, TempScore7, TempScore8], BestVal),    %find maximum score of maximizer and return that
        Score = BestVal
     ), !;
     (IsMax = false,    %minimizer chance , call recursively for the maximizer chance by changing IsMax to true , If slot is not free return 1000
        ((P0 == 0, replace(Board, 0, 1, NewBoard0), minimax(NewBoard0, NextDepth, true, TempScore0));TempScore0 = 1000),
        ((P1 == 0, replace(Board, 1, 1, NewBoard1), minimax(NewBoard1, NextDepth, true, TempScore1));TempScore1 = 1000),
        ((P2 == 0, replace(Board, 2, 1, NewBoard2), minimax(NewBoard2, NextDepth, true, TempScore2));TempScore2 = 1000),
        ((P3 == 0, replace(Board, 3, 1, NewBoard3), minimax(NewBoard3, NextDepth, true, TempScore3));TempScore3 = 1000),
        ((P4 == 0, replace(Board, 4, 1, NewBoard4), minimax(NewBoard4, NextDepth, true, TempScore4));TempScore4 = 1000),
        ((P5 == 0, replace(Board, 5, 1, NewBoard5), minimax(NewBoard5, NextDepth, true, TempScore5));TempScore5 = 1000),
        ((P6 == 0, replace(Board, 6, 1, NewBoard6), minimax(NewBoard6, NextDepth, true, TempScore6));TempScore6 = 1000),
        ((P7 == 0, replace(Board, 7, 1, NewBoard7), minimax(NewBoard7, NextDepth, true, TempScore7));TempScore7 = 1000),
        ((P8 == 0, replace(Board, 8, 1, NewBoard8), minimax(NewBoard8, NextDepth, true, TempScore8));TempScore8 = 1000),
        min_list([TempScore0, TempScore1, TempScore2, TempScore3, TempScore4, TempScore5, TempScore6, TempScore7, TempScore8], BestVal),    %find minimum score of minimizer and return that
        Score = BestVal
     )
    ).

/* functionToFindBestMove function
    For all the free slots call minimax after changing that index with computer chance
*/
functionToFindBestMove([P0, P1, P2, P3, P4, P5, P6, P7, P8], Chance):-
   Board = [P0, P1, P2, P3, P4, P5, P6, P7, P8],
   %If slot is not free return -1000
   ((P0 == 0, replace(Board, 0, 2, NewBoard0), minimax(NewBoard0, 0, false, TempScore0));TempScore0 = -1000),
   ((P1 == 0, replace(Board, 1, 2, NewBoard1), minimax(NewBoard1, 0, false, TempScore1));TempScore1 = -1000),
   ((P2 == 0, replace(Board, 2, 2, NewBoard2), minimax(NewBoard2, 0, false, TempScore2));TempScore2 = -1000),
   ((P3 == 0, replace(Board, 3, 2, NewBoard3), minimax(NewBoard3, 0, false, TempScore3));TempScore3 = -1000),
   ((P4 == 0, replace(Board, 4, 2, NewBoard4), minimax(NewBoard4, 0, false, TempScore4));TempScore4 = -1000),
   ((P5 == 0, replace(Board, 5, 2, NewBoard5), minimax(NewBoard5, 0, false, TempScore5));TempScore5 = -1000),
   ((P6 == 0, replace(Board, 6, 2, NewBoard6), minimax(NewBoard6, 0, false, TempScore6));TempScore6 = -1000),
   ((P7 == 0, replace(Board, 7, 2, NewBoard7), minimax(NewBoard7, 0, false, TempScore7));TempScore7 = -1000),
   ((P8 == 0, replace(Board, 8, 2, NewBoard8), minimax(NewBoard8, 0, false, TempScore8));TempScore8 = -1000),
   ListOfMiniMax = [TempScore0, TempScore1, TempScore2, TempScore3, TempScore4, TempScore5, TempScore6, TempScore7, TempScore8],
   max_list(ListOfMiniMax, BestVal),    %find the maximum score
   nth(Index, ListOfMiniMax, BestVal),  %find index of maximum score in the list
   Diff is Index - 1,                   %index is calculated from 1 , so subtracting it
   Chance = Diff.   
   

%finding the best move for the computer 
findBestMove(Board , Answer):- functionToFindBestMove(Board, Chance), Answer = Chance .


/* Predicates for checking winning */

%checkIfWon function calls row , column , diagonal checking
checkIfWon(List, X):- checkRow(List, X); checkCol(List, X); checkDiagonal(List, X).

%checkIfMovesLeft function checks if any free slot is present in the board.
checkIfMovesLeft(List):- member(0, List).

%checkCol function checks if board has a winning row
checkRow([X, X, X|_], X).
checkRow([_, _, _, X, X, X|_], X).
checkRow([_, _, _, _, _, _, X, X, X], X).

%checkCol function checks if board has a winning column
checkCol([X, _, _, X, _, _, X, _, _], X).
checkCol([_, X, _, _, X, _, _, X, _], X).
checkCol([_, _, X, _, _, X, _, _, X], X).

%checkDiagonal function checks if board has a winning diagonal
checkDiagonal([X, _, _, _, X, _, _, _, X], X).
checkDiagonal([_, _, X, _, X, _, X, _, _], X).



/* Utility Functions */

%neg function negates a boolean value

neg(Goal):-  Goal,!,fail. 
neg(_).

%replace function takes a list , index , value , and puts value at list[index] in the 4th arguement

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

%count function finds the number of occurences of element in a list
count([],_,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).


/* Drawing the Tic-Tac-Board */


    displayTicTaToe([X1, X2, X3, X4, X5, X6, X7, X8, X9]):- 
displayEmptyLine, write(' '), write(X1), write(' |'), write(' '), write(X2), write(' |'), write(' '), write(X3), displayBorderLine, 
displayEmptyLine, write(' '), write(X4), write(' |'), write(' '), write(X5), write(' |'), write(' '), write(X6), displayBorderLine, 
displayEmptyLine, write(' '), write(X7), write(' |'), write(' '), write(X8), write(' |'), write(' '), write(X9), write('\n'), displayEmptyLine.


    
    displayEmptyLine:- write('   |   |   \n').
    displayBorderLine:- write('\n___|___|___\n').

    
:- initialization(play).
