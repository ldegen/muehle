:- module('brett',[
  leeres_brett/1,
  brett_set/3,
  brett_set/4,
  feld/1,
  benachbart/2,
  mühle/3,
  in_mühle/2,
  brett_feld_figur/3,
  write_brett/1
]).
:- use_module('setters.pl').
:- use_module('bake.pl').

arg0(N,T,Arg):-
	when((ground(N);ground(M)),succ(N,M)),
	arg(M,T,Arg).


:- setters(brett/24).
leeres_brett(brett(
              frei,frei,frei,frei,
              frei,frei,frei,frei,
              frei,frei,frei,frei,
              frei,frei,frei,frei,
              frei,frei,frei,frei,
              frei,frei,frei,frei
           )
         ).

brett_set([],In,In).
brett_set([Feld=Wert|FelderWerte],In,Out):-
	brett_set(Feld,In,Wert,In1),
	brett_set(FelderWerte,In1,Out).

feld(A):-
	between(1,24,A).

feld_felge(Feld,Felge):-
	feld_koordinaten(Feld,_,Felge).

feld_speiche(Feld,Speiche):-
	feld_koordinaten(Feld,Speiche,_),
	1 is Speiche mod 2.

feld_koordinaten(Feld,Winkel,Felge):-
	(   ground(Feld)
	->  Winkel is Feld mod 8,
	    Felge is Feld // 8
	;   between(0,7,Winkel),
	    between(0,2,Felge),
	    Feld is Felge * 8 + Winkel
	).

benachbart_(A,B):-
  benachbart_felge(A,B).
benachbart_(A,B):-
  benachbart_speiche(B,A).

benachbart_felge(A,B):-
	feld_felge(A,Fg),
	feld_felge(B,Fg),
	Dist is (A-B) mod 8,
	memberchk(Dist,[1,7]).

benachbart_speiche(A,B):-
	feld_speiche(A,Sp),
	feld_speiche(B,Sp),
	feld_felge(A,FgA),
	feld_felge(B,FgB),
	1 is abs(FgA-FgB).

:- bake(benachbart(A1,B1),(benachbart_(A,B),succ(A,A1),succ(B,B1))).

brett_feld_figur(Brett,Feld,Farbe):-
  arg(Feld,Brett,Farbe).

mühle(1,2,3).
mühle(9,10,11).
mühle(17,18,19).
mühle(8,16,24).
mühle(4,12,20).
mühle(21,22,23).
mühle(13,14,15).
mühle(5,6,7).
mühle(1,7,8).
mühle(9,15,16).
mühle(17,23,24).
mühle(2,10,18).
mühle(6,14,22).
mühle(19,20,21).
mühle(11,12,13).
mühle(3,4,5).

perm(A,B,C,A,B,C).
perm(A,B,C,A,C,B).
perm(A,B,C,B,A,C).
perm(A,B,C,B,C,A).
perm(A,B,C,C,A,B).
perm(A,B,C,C,B,A).


:- bake(mühle_perm(A,B,C),(
	perm(A,B,C,A0,B0,C0),
	mühle(A0,B0,C0))).

in_mühle(Brett,Feld):-
	mühle_perm(Feld,B,C),
	arg(Feld,Brett,Farbe),
	arg(B,Brett,Farbe),
	arg(C,Brett,Farbe).


farbe_symbol(weiß,'(W)').
farbe_symbol(schwarz,'(S)').
farbe_symbol(frei,'   ').

format_figur(_,Farbe):-
	farbe_symbol(Farbe,Symbol),
	write(Symbol).

:- format_predicate('F',format_figur(_,_)).

map_args([],_,[]).
map_args([A|More],In,[V|Out]):-
	arg(A,In,V),
	map_args(More,In,Out).


write_brett(Brett):-
	map_args([
	    1, 2, 3,
	    9, 10, 11,
	    17,18,19,
	    8,16,24,20,12,4,
	    23,22,21,
	    15,14,13,
	    7,6,5
	],Brett,Figuren),
	FormatLines =[
	    '~F---------~F---------~F',
	    ' |           |           | ',
	    ' |  ~F-----~F-----~F	 | ',
	    ' |   |       |	     |   | ',
	    ' |   |	~F-~F-~F  |   | ',
	    ' |   |	 |       |   |   | ',
	    '~F-~F-~F     ~F-~F-~F',
	    ' |   |	 |       |   |   | ',
	    ' |   |  ~F-~F-~F  |   | ',
	    ' |   |       |       |   | ',
	    ' |  ~F-----~F-----~F	 | ',
	    ' |           |           | ',
	    '~F---------~F---------~F~n'
	],
	atomic_list_concat(FormatLines,'~n',Format),
	format(Format,Figuren).


