:- module(configuration,[

	  ]).

:- use_module('bake.pl').
:- use_module(library(record)).

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
