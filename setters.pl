:-module(setters,[op(1150, fx, setters)]).

:- multifile
	system:term_expansion/2.
:- dynamic
	system:term_expansion/2.

system:term_expansion((:-setters(Name/Arity)),Clauses):-
	setter_clauses(Name,Arity,Clauses).

setter_clause(Name,Arity,N,Clause):-
	atom_concat(Name,'_set',PredName),
	between(1,Arity,N),
	functor(In,Name,Arity),
	functor(Out,Name,Arity),
	In=..[_|Ins],
	Out=..[_|Outs],
	Clause =..[PredName,N,In,V,Out],
	set_elm(Ins,N,V,Outs).


setter_clauses(Name,Arity,Clauses):-
	findall(
	    Clause,
	    setter_clause(Name,Arity,_,Clause),
	    Clauses
	).

set_elm([],_,_,[]).
set_elm([_|Rest],1,V,[V|Rest]):-!.
set_elm([Field|Rest0],N,V,[Field|Rest1]):-
	M is N - 1,
	set_elm(Rest0,M,V,Rest1).

