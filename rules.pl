:- module('rules',[
    defeated/2,
    victorious/2,
    move/3,
    phase/2
  ]).
:- use_module('configuration.pl').
:- use_module('board.pl').


%% a player is defeated if he can not move
defeated(Cfg,Player):-
	player(Cfg,Player),
	\+ move(Cfg,_,_).

%% a player has won if his oponent is defeated
victorious(Cfg,Player):-
	defeated(Cfg,Oponent),
	oponent(Oponent,Player).

%% A move starts with the current player
% placing, or moving one of his own pieces
% according to the current phase of the game.
%
% If by this he closes a mill
% the player removes one of the oponents pieces
% from the board.
%
% Then the other player is to move.
move(Cfg0,move(From,To,Remove),Cfg3):-
        phase(Cfg0,Phase),
	move_piece(Phase,Cfg0,From,To,Cfg1),
	board(Cfg0,Board0),
	(   in_mill(Board0,To)
        ->  take_piece(Cfg1,Remove,Cfg2)
        ;   Remove=x, Cfg2=Cfg1
        ),
        next_player(Cfg2,Cfg3).


move_piece(placing,Cfg0,x,Field,Cfg1):-
	player(Cfg0,Me),
        place_piece(Cfg0,Me,Field,Cfg1).

move_piece(moving,Cfg0,From,To,Cfg1):-
	player(Cfg0,Me),
	occupied(Cfg0,From,Me),
	adjacent(From,To),
	empty(Cfg0,To),
        swap_fields(Cfg0,From,To,Cfg1).

move_piece(flying,Cfg0,From,To,Cfg1):-
	player(Cfg0,Me),
	occupied(Cfg0,From,Me),
	empty(Cfg0,To),
        swap_fields(Cfg0,From,To,Cfg1).

% As long as the oponent has unprotected
% pieces, only one of those may be taken.
% Only if *all* of the oponents pieces are protected,
% one of them may be taken.
% (implemented using a soft cut (*->)/2
take_piece(Cfg0,Remove,Cfg1):-
	player(Cfg0,Me),
        oponent(Me,Oponent),
        (   unprotected(Cfg0,Oponent,Remove)
        *-> remove_piece(Cfg0,Remove,Oponent,Cfg1)
        ;   occupied(Cfg0,Remove,Oponent),
            remove_piece(Cfg0,Remove,Oponent,Cfg1)
        ).

% A piece is unprotected if it is not part in
% a mill.
unprotected(Cfg,Player,Field):-
	occupied(Cfg,Field,Player),
	board(Cfg,Board),
	\+ in_mill(Board,Field).

phase(Cfg,Phase):-
	player(Cfg,Player),
        status(Cfg,Player,Unused,Lost),
	(   Lost > 6
	->  Phase = not_enough_pieces
	;   Unused > 0
	->  Phase = placing
	;   Lost = 6
	->  Phase = flying
	;   Phase = moving
	).
