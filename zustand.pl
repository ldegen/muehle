:- module('zustand',[
    start/1,
    spieler/2,
    gegner/2,
    brett/2,
    frei/2,
    besetzt/3,
    entferne_figur/4,
    setze_figur/4,
    tausche_felder/4,
    nächster_spieler/2,
    figuren/4
  ]).
:- use_module('brett.pl').
:- use_module(library(record)).
:- record konf(
	      spieler=weiß,
	      brett,
	      weiß=figuren(9,0),
	      schwarz=figuren(9,0)
    ).

start(Konf):-
        leeres_brett(B),
	make_konf([brett(B)],Konf).

spieler(Konf,Farbe):-
	konf_spieler(Konf,Farbe).
brett(Konf,Brett):-
	konf_brett(Konf,Brett).


frei(Konf,Feld):-
	brett(Konf,Brett),
        brett_feld_figur(Brett,Feld,frei).

besetzt(Konf,Feld,Farbe):-
	brett(Konf,Brett),
	brett_feld_figur(Brett,Feld,Farbe),
	Farbe \= frei.

gegner(schwarz,weiß).
gegner(weiß,schwarz).

entferne_figur(Konf0,Feld,Farbe,Konf1):-
    konf_data(Farbe,Konf0,figuren(Unbenutzt,Verloren0)),
    brett(Konf0,Brett0),
    besetzt(Konf0,Feld,Farbe),
    brett_set(Feld,Brett0,frei,Brett1),
    succ(Verloren0,Verloren1),
    Figuren =.. [Farbe,figuren(Unbenutzt,Verloren1)],
    set_konf_fields([brett(Brett1),Figuren],Konf0,Konf1).

setze_figur(Konf0,Farbe,Feld,Konf1):-
    frei(Konf0,Feld),
    brett(Konf0,Brett0),
    konf_data(Farbe,Konf0,figuren(Unbenutzt0,Verloren)),
    succ(Unbenutzt1,Unbenutzt0),
    Figuren =..[Farbe,figuren(Unbenutzt1,Verloren)],
    brett_set(Feld,Brett0,Farbe,Brett1),
    set_konf_fields([brett(Brett1),Figuren],Konf0,Konf1).

tausche_felder(Konf0,A,B,Konf1):-
    brett(Konf0,Brett0),
    brett_feld_figur(Brett0,A,FigurA),
    brett_feld_figur(Brett0,B,FigurB),
    brett_set([A=FigurB,B=FigurA],Brett0,Brett1),
    set_brett_of_konf(Brett1,Konf0,Konf1).

nächster_spieler(Konf0,Konf1):-
    spieler(Konf0,A),
    gegner(A,B),
    set_spieler_of_konf(B,Konf0,Konf1).

figuren(Konf,Farbe,Unbenutzt,Verloren):-
      konf_data(Farbe,Konf,figuren(Unbenutzt,Verloren)).

