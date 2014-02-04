:- module(configuration,[
	      start/1,
	      player/2,
	      oponent/2,
	      board/2,
	      empty/2,
	      occupied/3,
	      remove_piece/4,
	      place_piece/4,
	      swap_fields/4,
	      next_player/2,
	      status/4
	  ]).

:- use_module('bake.pl').
:- use_module('setters.pl').
:- use_module(library(record)).
:- use_module('board.pl').

:- record   player_status(
		   unused:between(0,9)=9,
		   lost:between(0,7)=0
		 ).

:- record board(
	      a7:oneof([black,white,empty])=empty,
	      d7:oneof([black,white,empty])=empty,
	      g7:oneof([black,white,empty])=empty,
	      g4:oneof([black,white,empty])=empty,
	      g1:oneof([black,white,empty])=empty,
	      d1:oneof([black,white,empty])=empty,
	      a1:oneof([black,white,empty])=empty,
	      a4:oneof([black,white,empty])=empty,
	      b6:oneof([black,white,empty])=empty,
	      d6:oneof([black,white,empty])=empty,
	      f6:oneof([black,white,empty])=empty,
	      f4:oneof([black,white,empty])=empty,
	      f2:oneof([black,white,empty])=empty,
	      d2:oneof([black,white,empty])=empty,
	      b2:oneof([black,white,empty])=empty,
	      b4:oneof([black,white,empty])=empty,
	      c5:oneof([black,white,empty])=empty,
	      d5:oneof([black,white,empty])=empty,
	      e5:oneof([black,white,empty])=empty,
	      e4:oneof([black,white,empty])=empty,
	      e3:oneof([black,white,empty])=empty,
	      d3:oneof([black,white,empty])=empty,
	      c3:oneof([black,white,empty])=empty,
	      c4:oneof([black,white,empty])=empty
	  ).

:- record configuration(
	      player:oneof([black,white])=white,
	      board:board=board(empty,empty,empty,empty,empty,empty,
				empty,empty,empty,empty,empty,empty,
				empty,empty,empty,empty,empty,empty,
				empty,empty,empty,empty,empty,empty
			       ),
	      white:player_status=player_status(9,0),
	      black:player_status=player_status(9,0)
	  ).

%%	-----------------------------------------------
record_arg_name_type_(R,A,N,T):-
	current_record(R,Desc),
	arg(A,Desc,DescArg),
	desc_arg_name_type(DescArg,N,T).



desc_arg_name_type(N:T=_,N,T):-!.
desc_arg_name_type(N:T,N,T):-!.
desc_arg_name_type(N=_,N,any):-!.
desc_arg_name_type(N,N,any).

:- bake(record_arg_name_type(R,A,N,T),record_arg_name_type_(R,A,N,T)).
:- bake(record_constructor_name(R,CN),(current_record(R,_),atom_concat('make_',R,CN))).

construct(Type,Options,Record):-
	record_constructor_name(Type,CN),
	call(CN,Options,Record).

json_to_record(Type,json(Props),Record):-
	props_options(Props,Type,Options),
	construct(Type,Options,Record).

record_to_json(Record,json(Props)):-
	functor(Record,Type,_),
	findall(Prop,
		(   record_arg_name_type(Type,Arg,Name,ValueType),
		    arg(Arg,Record,ArgValue),
		    record_arg_json(ValueType,ArgValue,Value),
		    Prop=(Name=Value)
		), Props).
record_arg_json(ValueType,Value,Json):-
	(   current_record(ValueType,_)
	->  record_to_json(Value,Json)
	;   Json=Value
	).


props_options([],_RecordType,[]).
props_options([Name=Value|MoreProps],RecordType,[Option|MoreOptions]):-
	record_arg_name_type(RecordType,_,Name,ValueType),
	prop_option(ValueType,Name,Value,Option),
	props_options(MoreProps,RecordType,MoreOptions).

prop_option(Type,Name,Value,Option):-
	(   current_record(Type,_)
	->  json_to_record(Type,Value,RecordValue),
	    Option =..[Name,RecordValue]
	;   Option =..[Name,Value]
	).


%%	--------------------------------



start(Cfg):-
	default_configuration(Cfg).

player(Cfg,Player):-
	configuration_player(Cfg,Player).
board(Cfg,Board):-
	configuration_board(Cfg,Board).


empty(Cfg,Field):-
	board(Cfg,Board),
        board_field_piece(Board,Field,empty).

occupied(Cfg,Field,Player):-
	board(Cfg,Board),
	board_field_piece(Board,Field,Player),
	Player \= empty.

oponent(black,white).
oponent(white,black).

remove_piece(Cfg0,Field,Player,Cfg1):-
	status(Cfg0,Player,Unused,Lost0),
	board(Cfg0,Board0),
	occupied(Cfg0,Field,Player),
	board_set(Field,Board0,empty,Board1),
	succ(Lost0,Lost1),
	make_player_status([unused(Unused),lost(Lost1)],Status),
	StatusOption =.. [Player,Status],
	set_configuration_fields([board(Board1),StatusOption],Cfg0,Cfg1).

place_piece(Cfg0,Player,Field,Cfg1):-
	empty(Cfg0,Field),
	board(Cfg0,Board0),
	status(Cfg0,Player,Unused0,Lost),
	succ(Unused1,Unused0),
	make_player_status([unused(Unused1),lost(Lost)],Status),
	StatusOption =.. [Player,Status],
	board_set(Field,Board0,Player,Board1),
	set_configuration_fields([board(Board1),StatusOption],Cfg0,Cfg1).

swap_fields(Cfg0,A,B,Cfg1):-
	board(Cfg0,Board0),
	board_field_piece(Board0,A,PieceA),
	board_field_piece(Board0,B,PieceB),
	board_set([A=PieceB,B=PieceA],Board0,Board1),
	set_board_of_configuration(Board1,Cfg0,Cfg1).

next_player(Cfg0,Cfg1):-
	player(Cfg0,A),
	oponent(A,B),
	set_player_of_configuration(B,Cfg0,Cfg1).

status(Cfg,Player,Unused,Lost):-
	make_player_status([unused(Unused),lost(Lost)],Status),
	configuration_data(Player,Cfg,Status).



