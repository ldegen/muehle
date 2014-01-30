

:-use_module('zustand.pl').
:-use_module('regeln.pl').
:-use_module('ki.pl').
:-use_module('brett.pl').


züge([Last],Konf0,Konf1,Last):-
	zug(Konf0,Last,Konf1).
züge([Zug|Züge],Konf0,Konf2,Last):-
	zug(Konf0,Zug,Konf1),
	züge(Züge,Konf1,Konf2,Last).






zeige(K):-
	brett(K,B),
	spieler(K,Farbe),
	phase(K,Phase),
	write_brett(B),
	figuren(K,schwarz,UnbenutztS,VerlorenS),
	figuren(K,weiß,UnbenutztW,VerlorenW),
	ÜbrigS is 9 - VerlorenS,
	ÜbrigW is 9 - VerlorenW,
	ImSpielS is ÜbrigS - UnbenutztS,
	ImSpielW is ÜbrigW - UnbenutztW,
	format('          Steine insgesamt | im Spiel | unbenutzt | verloren ~n',[]),
	format('          -----------------+----------+-----------+----------~n',[]),
	format('   weiß:   ~w               | ~w        | ~w         | ~w        ~n',[ÜbrigW,ImSpielW,UnbenutztW,VerlorenW]),
	format('schwarz:   ~w               | ~w        | ~w         | ~w        ~n~n',[ÜbrigS,ImSpielS,UnbenutztS,VerlorenS]),
	format('~w am Zug (~w)~n',[Farbe,Phase]).


:- dynamic history/1.
partie_starten(Farbe,Horizont):-
	retractall(history(_)),
	start(S0),
	partie_fortsetzen(S0,Farbe,Horizont).

partie_fortsetzen(S0,Farbe,Horizont):-
	zeige(S0),
	(   gewonnen(S0,Farbe)
	->  format('~w hat gewonnen!~n',[Farbe])
	;   nachdenken(S0,Farbe,Horizont,Zug),
	    (	Zug=end_of_file
	    ->	writeln(S0)
	    ;	Zug=tipp(T)
	    ->	finde_zug(S0,T,Tipp,Wert),
		parse(Tipp2,Tipp),
		format('ki empfiehlt: ~w ~w~n',[Tipp2,Wert]),
		partie_fortsetzen(S0,Farbe,Horizont)
	    ;	Zug = undo
	    ->	retract(history(_)),retract(history(_)),
		history(S1),
		!,
		partie_fortsetzen(S1,Farbe,Horizont)
	    ;	zug(S0,Zug,S1)
	    ->	asserta(history(S1)),
		partie_fortsetzen(S1,Farbe,Horizont)
	    ;	format('kein gültiger Befehl.',[]),
		partie_fortsetzen(S0,Farbe,Horizont)
	    )
	).

nachdenken(S0,Farbe,Horizont,Zug):-
	(   spieler(S0,Farbe)
	->  zug_mensch(Zug)
	;   zug_ki(S0,Horizont,Zug)
	).

zug_ki(S,Horizont,Zug):-
	time(finde_zug(S,Horizont,Zug,Wert)),
	parse(Zug2,Zug),
	format('ki hat sich entschieden: ~w ~w~n',[Zug2,Wert]).

zug_mensch(Zug):-
	catch(
	    (	read(Befehl),
		parse(Befehl,Zug)
	    ),
	    _,
	    Zug=nüscht
	).

parse((A-B,C),zug(A,B,C)):-integer(A),integer(B),integer(C),!.
parse((A-B),zug(A,B,x)):-integer(A),integer(B),!.
parse((B,C),zug(x,B,C)):-integer(B),integer(C),!.
parse(B,zug(x,B,x)):-integer(B),!.
parse(end_of_file,end_of_file):-!.
parse(tipp(T),tipp(T)):-integer(T),!.
parse(A,A).













