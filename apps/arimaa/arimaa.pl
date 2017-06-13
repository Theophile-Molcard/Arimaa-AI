:- module(bot,
			[	get_moves/3
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

% On essait toujours de faire le plus de déplacements possible


get_moves( Moves, [Color, _], Board) :-
	addMoves( Moves, [Color,_], Board, 4).

get_moves( Moves, [Color, _], Board) :-
	addMoves( Moves, [Color, _], Board, 3).

get_moves( Moves, [Color, _], Board) :-
	addMoves( Moves, [Color, _], Board, 2).

get_moves( Moves, [Color, _], Board) :-
	addMoves( Moves, [Color, _], Board, 1).

	
addMoves( Moves, [Color, _], Board, N) :-
	get_move( Move, [Color, _], Board, Cout ),
	N_moins_cout is N - Cout,
	N_moins_cout >= 0,
	multiUpdateBoard( Move, Board, NewBoard, Piece_mortes_retour),
	mortAlly(Piece_mortes_retour, Color),
	addMoves( Moves_retour, [Color, _], NewBoard, N_moins_cout),
	concat( Move, Moves_retour, Moves ).
	
addMoves( [], _, _, 0).

mortAlly([], _).
mortAlly([[]|Q], Color):-
	mortAlly(Q, Color).
mortAlly([[_, _, _, Color_enemie]|Q], Color):-
	Color \= Color_enemie,
	mortAlly(Q, Color).
	


% retournent des déplacement unitaires dans l'ordre de ce que l'on propose



get_move( Move, [Color,_], Board, 1) :-

	% gagne en 1 coup
	getAlly( Type, Color, Board, [6, Y, Type, Color]),
	Type = rabbit,
	gagne( Move, [X, Y, rabbit, Color], Board, 1).
	
get_move( Move, [Color,_], Board, 2) :-

	% gagne en 2 coup
	getAlly( Type, Color, Board, [X, Y, Type, Color]),
	Type = rabbit,
	X >= 5,
	gagne( Move, [X, Y, rabbit, Color], Board, 2).
	
get_move( Move, [Color,_], Board, 3) :-

	% gagne en 3 coup
	getAlly( Type, Color, Board, [X, Y, Type, Color]),
	Type = rabbit,
	X >= 4,
	gagne( Move, [X, Y, rabbit, Color], Board, 3).
	
get_move( Move, [Color,_], Board, 4) :-

	% gagne en 4 coup
	getAlly( Type, Color, Board, [X, Y, Type, Color]),
	Type = rabbit,
	X >= 3,
	gagne( Move, [X, Y, rabbit, Color], Board, 4).

get_move( Move, [Color,_], Board, 2) :-

	% tue une piece adverse
	getAlly( Type, Color, Board, Piece_retour),
	Type \= rabbit,
	tuePiece( Move, Piece_retour, Board).

get_move( [Move], [Color,_], Board, 1) :-

	% avance une piece random
	getAlly( Type, Color, Board, Piece_retour),
	avancePiece( Move, Piece_retour, Board).
	
get_move( Move, [Color,_], Board, 2) :-

	% pousse une piece random
	getAlly( Type, Color, Board, Piece_retour),
	Type \= rabbit,
	poussePiece( Move, Piece_retour, Board).

get_move( [Move], [Color,_], Board, 1) :-

	% dernier recours
	getAlly( Type, Color, Board, Piece_retour),
	droitePiece( Move, Piece_retour, Board).

get_move( [Move], [Color,_], Board, 1) :-

	% dernier recours
	getAlly( Type, Color, Board, Piece_retour),
	gauchePiece( Move, Piece_retour, Board).
	

get_move( [Move], [Color,_], Board, 1) :-

	% dernier recours
	Type \= rabbit,
	getAlly( Type, Color, Board, Piece_retour),
	reculePiece( Move, Piece_retour, Board).

multiUpdateBoard([], Board, Board, []).

multiUpdateBoard([Move| Q], Board, UpdatedBoard, [Piece_morte | Piece_mortes_retour]) :-
	updateBoard(Move, Board, TempBoard, Piece_morte),
	multiUpdateBoard( Q, TempBoard, UpdatedBoard, Piece_mortes_retour).
	

% Mise à jour du plateau entre deux coups
updateBoard([[X1, Y1],[X2, Y2]], Board, UpdatedBoard, Piece_morte) :-
	element([X1, Y1, Type, Color], Board ),
	retire([X1, Y1, Type, Color], Board, TempBoard),
	detruitPieces([[X2, Y2, Type, Color]|TempBoard], [[X2, Y2, Type, Color]|TempBoard], UpdatedBoard, Piece_morte).


% Savoir si une case est une trape pour une pièce donnée
trapForPiece( X_trap, Y_trap, [X, Y, _, Color], Board) :-
	trap( X_trap, Y_trap ),
	getNeighbour( [X_trap, Y_trap ,_ ,_], Board, Voisins ),
	retire([X, Y, _, Color], Voisins, Voisins_sans_piece),
	\+ getAlly(_, Color, Voisins_sans_piece, _).


% détruit toutes les pièces qui doivent l'être sur le plateau
detruitPieces([T|Q], Board, BoardUpdated, T) :-
	isTrapped(T, Board ), !,
	retire(T, Board, BoardUpdated).

detruitPieces([_|Q], Board, BoardUpdated, Y) :-
	detruitPieces( Q, Board, BoardUpdated, Y).

detruitPieces([], Board, Board, []).
	
	
	
	
% savoir si une piece est détruite
isTrapped([X, Y, _, Color], Board) :-
	trap(X, Y),
	getNeighbour([X, Y, _, Color], Board, Voisins),
	\+ getAlly(_, Color, Voisins, _).

trap(2, 2).
trap(2, 5).
trap(5, 2).
trap(5, 5).


abs(X, X) :-
	X >= 0, !.

abs(X, Moins_X) :-
	Moins_X is -X.
	
gagne( [[[6, Y], [7, Y]]], [6, Y, rabbit, Color], Board, Coup_restant) :-
	Coup_restant > 0,
	avancePiece([[6, Y], [7, Y]], [6, Y, rabbit, Color], Board), !.

gagne( [[[X, Y], [X_plus_un, Y]]| Move], [X, Y, rabbit, Color], Board, Coup_restant) :-
	bordureEnemie(X_gagne, Y_gagne, Color),
	isFree([X_gagne, Y_gagne], Board),
	Delta_X is 7 - X,
	Dif_Y is Y - Y_gagne,
	abs(Dif_Y, Delta_Y),
	Somme is Delta_X + Delta_Y,
	Somme =< Coup_restant,
	avancePiece([[X, Y], [X_plus_un, Y]], [X, Y, rabbit, Color], Board),
	updateBoard([[X, Y], [X_plus_un, Y]], Board, UpdatedBoard, _),
	Coup_restant_moins_un is Coup_restant - 1,
	gagne( Move, [X_plus_un, Y, rabbit, Color], UpdatedBoard, Coup_restant_moins_un).	
	
gagne( [[[X, Y], [X, Y_plus_un]]| Move], [X, Y, rabbit, Color], Board, Coup_restant) :-
	bordureEnemie(X_gagne, Y_gagne, Color),
	isFree([X_gagne, Y_gagne], Board),
	Delta_X is 7 - X,
	Dif_Y is Y - Y_gagne,
	abs(Dif_Y, Delta_Y),
	Somme is Delta_X + Delta_Y,
	Somme =< Coup_restant,
	Delta_X < Coup_restant,
	droitePiece([[X, Y], [X, Y_plus_un]], [X, Y, rabbit, Color], Board),
	Coup_restant_moins_un is Coup_restant - 1,
	updateBoard([[X, Y], [X_plus_un, Y]], Board, UpdatedBoard, _),
	gagne( Move, [X, Y_plus_un, rabbit, Color], UpdatedBoard, Coup_restant_moins_un).	
	
gagne( [[[X, Y], [X, Y_moins_un]]| Move], [X, Y, rabbit, Color], Board, Coup_restant) :-
	bordureEnemie(X_gagne, Y_gagne, Color),
	isFree([X_gagne, Y_gagne], Board),
	Delta_X is 7 - X,
	Dif_Y is Y - Y_gagne,
	abs(Dif_Y, Delta_Y),
	Somme is Delta_X + Delta_Y,
	Somme =< Coup_restant,
	Delta_X < Coup_restant,
	gauchePiece([[X, Y], [X, Y_moins_un]], [X, Y, rabbit, Color], Board),
	Coup_restant_moins_un is Coup_restant - 1,
	updateBoard([[X, Y], [X_plus_un, Y]], Board, UpdatedBoard, _),
	gagne( Move, [X, Y_moins_un, rabbit, Color], UpdatedBoard, Coup_restant_moins_un).
	


% fait avancer une piece aléatoire du plateau
avancePiece( [[X, Y], [X_plus_un, Y]], [X, Y, Type, Color], Board ) :-
	X_plus_un is X+1,
	inBoard(X_plus_un, Y),
	isFree([X_plus_un, Y], Board),
	\+ trapForPiece(X_plus_un, Y, [X, Y, Type, Color], Board),
	canMove([X, Y, Type, Color], Board).

% fait avancer une piece aléatoire du plateau
bougePieceEnemie( [[X, Y], [X_voisin, Y_voisin]], [X, Y, _, _], Board ) :-
	neighbour(X, Y, X_voisin, Y_voisin), 
	inBoard(X_voisin, Y_voisin),
	isFree([X_voisin, Y_voisin], Board).



% fait avancer une piece aléatoire du plateau
poussePiece( [Move_enemie, [[X, Y],[X_plus_un, Y]]], [X, Y, Type, Color], Board ) :-
	X_plus_un is X+1,
	inBoard(X_plus_un, Y),
	canMove([X, Y, Type, Color], Board),
	\+ trapForPiece(X_plus_un, Y, [X, Y, Type, Color], Board),
	element([X_plus_un, Y, Type_enemie, Color_enemie], Board),
	Color_enemie \= Color,
	isWeaker(Type_enemie, Type),
	bougePieceEnemie(Move_enemie, [X_plus_un, Y, Type_enemie, Color_enemie], Board).


% fait avancer une piece aléatoire du plateau
tuePiece( [[PosEnemie,PosPiege], [[X, Y],PosEnemie]], [X, Y, Type, Color], Board ) :-
	canMove([X, Y, Type, Color], Board),
	getNeighbour([X, Y, Type, Color], Board, Voisins),
	isVulnerable([X, Y, Type, Color], Voisins, Board, PosEnemie, PosPiege).

isVulnerable([X, Y, Type, Color], [[X_enemie, Y_enemie, Type_enemie, Color_enemie]|_], Board, [X_enemie, Y_enemie], [X_v_e, Y_v_e]) :-
	Color_enemie \= Color,
	isWeaker(Type_enemie, Type),
	neighbour( X_enemie, Y_enemie, X_v_e, Y_v_e ),
	trap(X_v_e, Y_v_e),
	isFree([X_v_e, Y_v_e], Board),
	trapForPiece(X_v_e, Y_v_e, [X_enemie, Y_enemie, Type_enemie, Color_enemie], Board), !.

isVulnerable( Piece, [_|Q], Board, Piece_enemie, Piege) :-
	isVulnerable( Piece, Q, Board, Piece_enemie, Piege).
	

% fait aller à droite une piece aléatoire du plateau
droitePiece( [[X, Y], [X, Y_plus_un]], [X, Y, Type, Color], Board ) :-
	Y_plus_un is Y+1,
	inBoard(X, Y_plus_un),
	isFree([X, Y_plus_un], Board),
	\+ trapForPiece(X, Y_plus_un, [X, Y, Type, Color], Board),
	canMove([X, Y, Type, Color], Board).


% fait aller à gauche une piece aléatoire du plateau
gauchePiece( [[X, Y], [X, Y_moins_un]], [X, Y, Type, Color], Board ) :-
	Y_moins_un is Y-1,
	inBoard(X, Y_moins_un),
	isFree([X, Y_moins_un], Board),
	\+ trapForPiece(X, Y_moins_un, [X, Y, Type, Color], Board),
	canMove([X, Y, Type, Color], Board).


% recule une pièce
reculePiece( [[X, Y], [X_moins_un, Y]], [X, Y, Type, Color], Board ) :-
	X_moins_un is X-1,
	inBoard(X_moins_un, Y),
	isFree([X_moins_un, Y], Board),
	\+ trapForPiece(X_moins_un, Y, [X, Y, Type, Color], Board),
	canMove([X, Y, Type, Color], Board).

% retourne un alié (on peut choisir le type la couleur)

getAlly( Type, Color, [T|Q], Piece_retour) :-
	T \= [_, _, Type, Color],
	getAlly( Type, Color, Q, Piece_retour).
	
getAlly( Type, Color, [[X, Y, Type, Color]|_], [X, Y, Type, Color]).
	
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
getNeighbour( [X1, Y1, _, _], [[X2, Y2, Type, Color] | Q], [[X2, Y2, Type, Color] | Voisins]) :-
	neighbour(X1, Y1, X2, Y2), getNeighbour( [X1, Y1, _, _], Q, Voisins), !.
	
getNeighbour( Piece, [ _ | Q ], Voisins) :-
	getNeighbour( Piece, Q, Voisins).
	
getNeighbour( _ , [], []).


bordureEnemie(7, 0, silver).
bordureEnemie(7, 1, silver).
bordureEnemie(7, 2, silver).
bordureEnemie(7, 3, silver).
bordureEnemie(7, 4, silver).
bordureEnemie(7, 5, silver).
bordureEnemie(7, 6, silver).
bordureEnemie(7, 7, silver).

bordureEnemie(0, 0, gold).
bordureEnemie(0, 1, gold).
bordureEnemie(0, 2, gold).
bordureEnemie(0, 3, gold).
bordureEnemie(0, 4, gold).
bordureEnemie(0, 5, gold).
bordureEnemie(0, 6, gold).
bordureEnemie(0, 7, gold).