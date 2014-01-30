:- module('heuristik',[
    heuristik/3
  ]).
:- use_module('zustand.pl').
:- use_module('brett.pl').

figuren_übrig(Konf,Anzahl):-
	spieler(Konf,Spieler),
	figuren_übrig(Konf,Spieler,Anzahl).

figuren_übrig(Konf,Spieler,Anzahl):-
        figuren(Konf,Spieler,_,Verloren),
	Anzahl is 9 - Verloren.

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
	G is 7*(MeineFiguren-SeineFiguren) + MeineFreiheiten-SeineFreiheiten.

