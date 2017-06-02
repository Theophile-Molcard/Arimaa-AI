:- module(bot,
      [  get_moves/3
      ]).

% A few comments but all is explained in README of github

% get_moves signature
% get_moves(Moves, gamestate, board).

% Exemple of variable
% gamestate: [side, [captured pieces] ] (e.g. [silver, [ [0,1,rabbit,silver],[0,2,horse,silver] ])
% board: [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]

% Call exemple:
% get_moves(Moves, [silver, []], [[0,0,rabbit,silver],[0,1,rabbit,silver],[0,2,horse,silver],[0,3,rabbit,silver],[0,4,elephant,silver],[0,5,rabbit,silver],[0,6,rabbit,silver],[0,7,rabbit,silver],[1,0,camel,silver],[1,1,cat,silver],[1,2,rabbit,silver],[1,3,dog,silver],[1,4,rabbit,silver],[1,5,horse,silver],[1,6,dog,silver],[1,7,cat,silver],[2,7,rabbit,gold],[6,0,cat,gold],[6,1,horse,gold],[6,2,camel,gold],[6,3,elephant,gold],[6,4,rabbit,gold],[6,5,dog,gold],[6,6,rabbit,gold],[7,0,rabbit,gold],[7,1,rabbit,gold],[7,2,rabbit,gold],[7,3,cat,gold],[7,4,dog,gold],[7,5,rabbit,gold],[7,6,horse,gold],[7,7,rabbit,gold]]).

% default call
get_moves( Moves, [Color,_], Board) :-
  addMoves( Moves, [Color,_], Board, 4).

get_moves( Moves, [Color,_], Board) :-
  addMoves( Moves, [Color,_], Board, 3).

get_moves( Moves, [Color,_], Board) :-
  addMoves( Moves, [Color,_], Board, 2).

get_moves( Moves, [Color,_], Board) :-
  addMoves( Moves, [Color,_], Board, 1).

addMoves( [Move], [Color, _], Board, 1) :-
  get_move( Move, [Color, _], Board ).

addMoves( [Moves], [Color,_], Board, N) :-
  N_moins_un is N-1,
  get_move( Move, [Color, _], Board ),
  updateBoard( Move, Board, NewBoard),
  addMoves( Moves_retour, [Color,_], NewBoard, N_moins_un),
  concat( [Move], Moves_retour, Moves ).





get_move( Move, [Color,_], Board) :-

  % avance une piece random
  getAlly( Type, silver, Board, Piece_retour),
  avancePiece( Move, Piece_retour, Board).

get_move( Move, [Color,_], Board) :-

  % dernier recours
  getAlly( Type, silver, Board, Piece_retour),
  aleaMovePiece( Move, Piece_retour, Board).

updateBoard([[X1, Y1],[X2, Y2]], Board, [[X2, Y2, Type, Color]|TempBoard]) :-
  element([X1, Y1, Type, Color], Board ),
  retire([X1, Y1, Type, Color], Board, TempBoard).


% retourne toutes les possibilités de déplacement d'une pièce
avancePiece( [[X, Y], [X_plus_un, Y]], [X, Y, Type, Color], Board ) :-
  X_plus_un is X+1,
  inBoard(X_plus_un, Y),
  isFree([X_plus_un, Y], Board),
  canMove([X, Y, Type, Color], Board).

% retourne toutes les possibilités de déplacement d'une pièce
aleaMovePiece( [[X, Y], [X_voisin, Y_voisin]], [X, Y, Type, Color], Board ) :-
  canMove([X, Y, Type, Color], Board),
  neighbour(X, Y, X_voisin, Y_voisin),
  isFree([X_voisin, Y_voisin], Board).

% retourne un alié (on peut choisir le type la couleur)
getAlly( _, _, [], _) :- fail.
getAlly( Type, Color, [T|Q], Piece_retour) :-
  T \= [_, _, Type, Color],
  getAlly( Type, Color, Q, Piece_retour).
getAlly( Type, Color, [Piece_retour|_], Piece_retour) :-
  T = [_, _, Type, Color].
getAlly( Type, Color, [T|Q], Piece_retour) :-
  getAlly( Type, Color, Q, Piece_retour).


% coordonnées sont sur le plateau
inBoard(X, Y) :-
  X >= 0, X =< 7, Y >= 0, Y =< 7 .

% retourne / vérifie que deux cases sont voisines
neighbour(X1, Y, X2, Y) :-
	X2 is X1 - 1, inBoard(X2, Y), inBoard(X2, Y).
neighbour(X1, Y, X2, Y) :-
	X2 is X1 + 1, inBoard(X2, Y), inBoard(X2, Y).
neighbour(X, Y1, X, Y2) :-
	Y2 is Y1 - 1, inBoard(X, Y2), inBoard(X, Y2).
neighbour(X, Y1, X, Y2) :-
	Y2 is Y1 + 1, inBoard(X, Y2), inBoard(X, Y2).


% vérifie qu'une case est vide, prend en parametre coord(X, Y) et Board
isFree([X, Y], Board) :- \+ element([X, Y, _, _], Board).

% element d'une liste.
element(T, [T|_]).
element(E, [_|Q]) :- element(E, Q).


% concatène deux listes
concat([], L2, L2).
concat([T|Q], L2, [T|L3]) :- concat(Q, L2, L3).

% retire un element d'une liste
retire(X, [X | Q], Q).
retire(X, [T | Q], [T | R]) :- X\=T, retire(X, Q, R).

% s'efface si la pièce peut bouger
canMove( Piece, Board ) :-
  getNeighbour(Piece, Board, Voisins),
  closeToFriends(Piece, Voisins), !. % si elle est à coté d'un ami elle peut bouger.

canMove( Piece, Board ) :-
  getNeighbour(Piece, Board, Voisins),
  stronger(Piece, Voisins), !. % pas d'amis à coté, elle peut bouger ssi elle est plus forte que tout ses voisins.


% controle la supériorité d'une pièce sur une autre
isWeaker( rabbit, _ ) :- !.

isWeaker(rabbit, cat):- !.
isWeaker(cat, dog):- !.
isWeaker(dog, horse):- !.
isWeaker(horse, camel):- !.
isWeaker(camel, elephant):- !.

isWeaker(X, Y) :- X\=Y, X\=elephant, isWeaker(X, W), isWeaker(W, Y).


% verifie la proximité d'un ami
closeToFriends( [_, _, _, Color], [ [_, _, _, Color] | _ ] ).
closeToFriends( P, [ _ | Q ]) :-
	closeToFriends( P, Q ).

% verifie sa suppériorité aux autres pièces l'entourant
stronger( _, []).
stronger( [X, Y, Type1, Color], [ [_, _, Type2, _] | Q ]) :-
	isWeaker( Type2, Type1 ), stronger( [X, Y, Type1, Color], Q ).

%retourne les pièces voisines d'une pièce sur un plateau
getNeighbour( [X1, Y1 | Q1], [[X2, Y2 | Q2] | Q], [[X2, Y2 | Q2] | Voisins]) :-
	neighbour(X1, Y1, X2, Y2), getNeighbour( [X1, Y1 | Q1], Q, Voisins), !.
getNeighbour( Piece, [ _ | Q ], Voisins) :-
	getNeighbour( Piece, Q, Voisins).
getNeighbour( _ , [], []).
