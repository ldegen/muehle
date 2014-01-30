:- module('regeln',[
    verloren/2,
    gewonnen/2,
    zug/3,
    phase/2
  ]).
:- use_module('zustand.pl').

verloren(Konf,Spieler):-
	spieler(Konf,Spieler),
	\+ bewegen(Konf,_,_,_).

gewonnen(Konf,Spieler):-
	verloren(Konf,Gegner),
	gegner(Gegner,Spieler).

zug(Konf0,zug(Von,Nach,Geschlagen),Konf3):-
	bewegen(Konf0,Von,Nach,Konf1),
	schlagen(Konf1,Nach,Geschlagen,Konf2),
        nächster_spieler(Konf2,Konf3).

ungeschützt(Konf,Farbe,Feld):-
	besetzt(Konf,Feld,Farbe),
	brett(Konf,Brett),
	\+ in_mühle(Brett,Feld).
schlagen(Konf0,Feld,GeschlagenesFeld,Konf1):-
	brett(Konf0,Brett0),
	spieler(Konf0,Ich),
	gegner(Ich,Gegner),
	(   in_mühle(Brett0,Feld)
	->  (   ungeschützt(Konf0,Gegner,GeschlagenesFeld)
	    ->	entferne_figur(Konf0,GeschlagenesFeld,Gegner,Konf1)
	    ;	besetzt(Konf0,GeschlagenesFeld,Gegner),
	        entferne_figur(Konf0,GeschlagenesFeld,Gegner,Konf1)
	    )
	;   Konf1=Konf0,
	    GeschlagenesFeld=x
	).

setzen(Konf0,Feld,Konf1):-
	spieler(Konf0,Ich),
        setze_figur(Konf0,Ich,Feld,Konf1).

ziehen(Konf0,Von,Nach,Konf1):-
	spieler(Konf0,Ich),
	besetzt(Konf0,Von,Ich),
	benachbart(Von,Nach),
	frei(Konf0,Nach),
        tausche_felder(Konf0,Von,Nach,Konf1).

springen(Konf0,Von,Nach,Konf1):-
	spieler(Konf0,Ich),
	besetzt(Konf0,Von,Ich),
	frei(Konf0,Nach),
        tausche_felder(Konf0,Von,Nach,Konf1).

bewegen(Konf0,Von,Nach,Konf1):-
	(   phase(Konf0,setzen)
	->  setzen(Konf0,Nach,Konf1),
	    Von=x
	;   phase(Konf0,ziehen)
	->  ziehen(Konf0,Von,Nach,Konf1)
	;   phase(Konf0,springen)
	->  springen(Konf0,Von,Nach,Konf1)
	).

phase(Konf,Phase):-
	spieler(Konf,Farbe),
        figuren(Konf,Farbe,Unbenutzt,Verloren),
	(   Verloren > 6
	->  Phase = zuwenig_steine
	;   Unbenutzt > 0
	->  Phase = setzen
	;   Verloren = 6
	->  Phase = springen
	;   Phase = ziehen
	).
