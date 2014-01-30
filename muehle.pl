

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



partie_starten:-
	start(S0),
	partie_fortsetzen(S0).

partie_fortsetzen(S0):-
	zeige(S0),
	(   gewonnen(S0,Farbe)
	->  format('~w hat gewonnen!~n',[Farbe])
	;	spieler(S0,Farbe),
		nachdenken(Farbe,S0,Zug),
	    zug(S0,Zug,S1),
	    partie_fortsetzen(S1)
	).

nachdenken(weiß,S,Zug):-
	time(finde_zug(S,5,Zug,Wert)),
	parse(Zug2,Zug),
	format('ki hat sich entschieden: ~w ~w~n',[Zug2,Wert]).

nachdenken(schwarz,S,Zug):-
	repeat,
	catch(
	    read(Zug0),_,
	    (writeln('?!!'), fail  )
	),
	(   Zug0 == end_of_file
	->  !,
	    writeln(S)
	;   parse(Zug0,Zug),zug(S,Zug,_)
	->  !
	;   writeln('versteh ich nicht, bitte nochmal'),
	    fail
	).

parse((A-B,C),zug(A,B,C)):-integer(A),integer(B),integer(C),!.
parse((A-B),zug(A,B,x)):-integer(A),integer(B),!.
parse((B,C),zug(x,B,C)):-integer(B),integer(C),!.
parse(B,zug(x,B,x)):-integer(B),!.









