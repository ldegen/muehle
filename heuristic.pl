:- module('heuristic',[
    heuristic/3
  ]).
:- use_module('configuration.pl').
:- use_module('board.pl').

pieces_left(Cfg,Count):-
	player(Cfg,Player),
	pieces_left(Cfg,Player,Count).

pieces_left(Cfg,Player,Count):-
        status(Cfg,Player,_,Lost),
	Count is 9 - Lost.

freedom(Cfg,Player,Field):-
	occupied(Cfg,Field,Player),
	adjacent(Field,Neighbour),
	empty(Cfg,Neighbour).

freedoms(Cfg,Player,Count):-
	aggregate_all(count,freedom(Cfg,Player,_Field),Count).

heuristic(Cfg,Me,G):-
	oponent(Me,Them),
	pieces_left(Cfg,Me,MyPieces),
	pieces_left(Cfg,Them,TheirPieces),
	freedoms(Cfg,Me,MyFreedoms),
	freedoms(Cfg,Them,TheirFreedoms),
	G is 7*(MyPieces-TheirPieces) + MyFreedoms-TheirFreedoms.

