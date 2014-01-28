:- use_module(library(record)).
:- use_module('setters.pl').
:- use_module('bake.pl').

arg0(N,T,Arg):-
	(   ground(N)
	->  M is N + 1,
	    arg(M,T,Arg)
	;   arg(M,T,Arg),
	    N is M -1
	).


gegner(schwarz,weiß).
gegner(weiß,schwarz).

:- setters(brett/24).
brett_set([],In,In).
brett_set([Feld=Wert|FelderWerte],In,Out):-
	brett_set(Feld,In,Wert,In1),
	brett_set(FelderWerte,In1,Out).



:- record konf(
	      spieler:oneof([schwarz,weiß])=weiß,
	      brett:compound=brett(
			  frei,frei,frei,frei,
			  frei,frei,frei,frei,
			  frei,frei,frei,frei,
			  frei,frei,frei,frei,
			  frei,frei,frei,frei,
			  frei,frei,frei,frei
			 ),
	      weiß:compound=figuren(9,0),
	      schwarz:compound=figuren(9,0)
    ).
feld(A):-
	between(0,23,A).

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

phase(Konf,Phase):-
	spieler(Konf,Farbe),
	konf_data(Farbe,Konf,figuren(Setzen,Verloren)),
	(   Verloren > 6
	->  Phase = zuwenig_steine
	;   Setzen > 0
	->  Phase = setzen
	;   Verloren = 6
	->  Phase = springen
	;   Phase = ziehen
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

:- bake(benachbart(A,B),(feld(A),feld(B),benachbart_(A,B))).

spieler(Konf,Farbe):-
	konf_spieler(Konf,Farbe).
brett(Konf,Brett):-
	konf_brett(Konf,Brett).


frei(Konf,Feld):-
	brett(Konf,Brett),
	arg0(Feld,Brett,frei).

besetzt(Konf,Feld,Farbe):-
	brett(Konf,Brett),
	arg0(Feld,Brett,Farbe),
	Farbe \= frei.

mühle(0,1,2).
mühle(8,9,10).
mühle(16,17,18).
mühle(7,15,23).
mühle(3,11,19).
mühle(20,21,22).
mühle(12,13,14).
mühle(4,5,6).
mühle(0,6,7).
mühle(8,14,15).
mühle(16,22,23).
mühle(1,9,17).
mühle(5,13,21).
mühle(18,19,20).
mühle(10,11,12).
mühle(2,3,4).

perm(A,B,C,A,B,C).
perm(A,B,C,A,C,B).
perm(A,B,C,B,A,C).
perm(A,B,C,B,C,A).
perm(A,B,C,C,A,B).
perm(A,B,C,C,B,A).

mühle_perm(A,B,C):-
	perm(A,B,C,A0,B0,C0),
	mühle(A0,B0,C0).

in_mühle(Brett,Feld):-
	mühle_perm(Feld,B,C),
	arg0(Feld,Brett,Farbe),
	arg0(B,Brett,Farbe),
	arg0(C,Brett,Farbe).

start(Konf):-
	default_konf(Konf).

züge([Last],Konf0,Konf1,Last):-
	zug(Konf0,Last,Konf1).
züge([Zug|Züge],Konf0,Konf2,Last):-
	zug(Konf0,Zug,Konf1),
	züge(Züge,Konf1,Konf2,Last).

verloren(Konf,Spieler):-
	spieler(Konf,Spieler),
	\+ bewegen(Konf,_,_,_).

gewonnen(Konf,Spieler):-
	verloren(Konf,Gegner),
	gegner(Gegner,Spieler).


figuren_übrig(Konf,Anzahl):-
	spieler(Konf,Spieler),
	figuren_übrig(Konf,Spieler,Anzahl).

figuren_übrig(Konf,Spieler,Anzahl):-
	konf_data(Spieler,Konf,figuren(_,Verloren)),
	Anzahl is 9 - Verloren.

schlagen(Konf0,Feld,GeschlagenesFeld,Konf1):-
	brett(Konf0,Brett0),
	spieler(Konf0,Ich),
	gegner(Ich,Gegner),
	konf_data(Gegner,Konf0,figuren(ÜberigGegner,VerlorenGegner0)),
	(   in_mühle(Brett0,Feld)
	->  besetzt(Konf0,GeschlagenesFeld,Gegner),
	    brett_set(GeschlagenesFeld,Brett0,frei,Brett1),
	    VerlorenGegner1 is VerlorenGegner0 + 1
	;   Brett0=Brett1,
	    GeschlagenesFeld=x,
	    VerlorenGegner1 = VerlorenGegner0
	),
	FigurenGegner =.. [Gegner,figuren(ÜberigGegner,VerlorenGegner1)],
	set_konf_fields([
	    brett(Brett1),
	    spieler(Gegner),
	    FigurenGegner
	],Konf0,Konf1).

setzen(Konf0,Feld,Konf1):-
	frei(Konf0,Feld),
	brett(Konf0,Brett0),
	spieler(Konf0,Ich),
	konf_data(Ich,Konf0,figuren(ÜberigIch0,VerlorenIch)),
	ÜberigIch1 is ÜberigIch0 - 1,
	FigurenIch =..[Ich,figuren(ÜberigIch1,VerlorenIch)],
	brett_set(Feld,Brett0,Ich,Brett1),
	set_konf_fields([brett(Brett1),FigurenIch],Konf0,Konf1).

ziehen(Konf0,Von,Nach,Konf1):-
	spieler(Konf0,Ich),
	besetzt(Konf0,Von,Ich),
	benachbart(Von,Nach),
	frei(Konf0,Nach),
	brett(Konf0,Brett0),
	brett_set([Von=frei,Nach=Ich],Brett0,Brett1),
	set_brett_of_konf(Brett1,Konf0,Konf1).

springen(Konf0,Von,Nach,Konf1):-
	spieler(Konf0,Ich),
	besetzt(Konf0,Von,Ich),
	frei(Konf0,Nach),
	brett(Konf0,Brett0),
	brett_set([Von=frei,Nach=Ich],Brett0,Brett1),
	set_brett_of_konf(Brett1,Konf0,Konf1).


bewegen(Konf0,Von,Nach,Konf1):-
	(   phase(Konf0,setzen)
	->  setzen(Konf0,Nach,Konf1),
	    Von=x
	;   phase(Konf0,ziehen)
	->  ziehen(Konf0,Von,Nach,Konf1)
	;   phase(Konf0,springen)
	->  springen(Konf0,Von,Nach,Konf1)
	).
freiheit(Konf,Spieler,Feld):-
	besetzt(Konf,Feld,Spieler),
	benachbart(Feld,Nachbar),
	frei(Konf,Nachbar).

freiheiten(Konf,Spieler,Anz):-
	aggregate_all(count,freiheit(Konf,Spieler,_Feld),Anz).

heuristik(Konf,Ich,G):-
	gegner(Ich,Er),
	figuren_übrig(Konf,Ich,MeineFiguren),
	figuren_übrig(Konf,Er,SeineFiguren),
	freiheiten(Konf,Ich,MeineFreiheiten),
	freiheiten(Konf,Er,SeineFreiheiten),
	G is MeineFiguren-SeineFiguren + MeineFreiheiten-SeineFreiheiten.



bewerten(Konf,MaxTiefe,Erg):-
	spieler(Konf,Spieler),
	bewerten(Konf,0,MaxTiefe,Spieler,Erg).

bewerten(Konf,_,_,Farbe,gewonnen(9999999)):-
	gewonnen(Konf,Farbe),
	!.
bewerten(Konf,_,_,Farbe,verloren(-9999999)):-
	verloren(Konf,Farbe),
	!.
bewerten(Konf,Tiefe,MaxTiefe,Farbe,stop(N)):-
	Tiefe >= MaxTiefe,
	spieler(Konf,Farbe),
	!,
	heuristik(Konf,Farbe,N).
bewerten(Konf,Tiefe,MaxTiefe,Farbe,Ergebnis):-
	Tiefe1 is Tiefe +1,
	(   spieler(Konf,Farbe)
	->  MinMax=max(M,Zug)
	;   MinMax=min(M,Zug)
	),
	aggregate_all(
	    MinMax,
	    (   zug(Konf,Zug,Konf1),
		bewerten(Konf1,Tiefe1,MaxTiefe,Farbe,Ergebnis1),
		arg(1,Ergebnis1,M)
	    ),
	    Ergebnis
	).


zug(Konf0,zug(Von,Nach,Geschlagen),Konf2):-
	bewegen(Konf0,Von,Nach,Konf1),
	schlagen(Konf1,Nach,Geschlagen,Konf2).



farbe_symbol(weiß,'(W)').
farbe_symbol(schwarz,'(S)').
farbe_symbol(frei,'   ').

format_figur(_,Farbe):-
	farbe_symbol(Farbe,Symbol),
	write(Symbol).

:- format_predicate('F',format_figur(_,_)).

map_args([],_,[]).
map_args([A|More],In,[V|Out]):-
	arg0(A,In,V),
	map_args(More,In,Out).


write_brett(Brett):-
	map_args([
	    0, 1, 2,
	    8, 9, 10,
	    16,17,18,
	    7,15,23,19,11,3,
	    22,21,20,
	    14,13,12,
	    6,5,4
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



zeige(K):-
	brett(K,B),
	spieler(K,Farbe),
	phase(K,Phase),
	write_brett(B),
	format('~w am Zug (~w)~n',[Farbe,Phase]).

bewerte(K,Tiefe):-
	bewerten(K,Tiefe,Erg),
	arg(2,Erg,Zug),
	zug(K,Zug,K1),
	format('Ergebnis: ~w~n',[Erg]),
	zeige(K1).








