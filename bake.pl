:-module(bake,[]).

:- multifile
	system:term_expansion/2.
:- dynamic
	system:term_expansion/2.

system:term_expansion((:-bake(Head,Goal)),Clauses):-
	findall(Head,Goal,Clauses).
