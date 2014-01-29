:- use_module(library(record)).



stellung_zug(Von,Zug,Nach):-
  zug(Von,Zug,Nach).

:- dynamic stellung_wert_cache/3.
stellung_wert(Stellung,Wert):-
	term_hash(Stellung,Hash),
        (   stellung_wert_cache(Hash,Stellung,Wert),
	    !
	;   heuristik(Stellung,weiß,Wert),
	    !,
	    assert(stellung_wert_cache(Hash,Stellung,Wert))
	).

:- record cx(horizont=5,tiefe=0,alpha=(-9999999),beta=9999999).


ich(Cx):-
	cx_tiefe(Cx,T),
	0 is T mod 2.

runter(Cx0,Cx):-
	cx_tiefe(Cx0,T0),
	T is T0 + 1,
	set_tiefe_of_cx(T,Cx0,Cx).

finde_zug(Stellung,Horizont,Zug,Wert):-
	make_cx([horizont(Horizont)],Cx),
	wert(Stellung,Cx,Zug-Wert).


wert(K,Cx,Zug-Wert):-
  ( horizont_erreicht(Cx)
  ->stellung_wert(K,Wert),
    Zug=horizont
  ; spielende_erreicht(K)
  ->stellung_wert(K,Wert),
    Zug=ende
  ; suche_wert(K,Cx,Zug-Wert)
  ).


horizont_erreicht(Cx):-
	cx_horizont(Cx,H),
	cx_tiefe(Cx,T),
	T>H.
spielende_erreicht(K):-
	\+ stellung_zug(K,_,_).

suche_wert(K0,Cx0,Zug-Wert):-
  findall(H-(Z-K),(stellung_zug(K0,Z,K),stellung_wert(K,H)),Ks),
  keysort(Ks,SortedKs),
  reverse(SortedKs,ReverseSortedKs),
  runter(Cx0,Cx),
  ( ich(Cx0)
  ->maximize(ReverseSortedKs,Cx,(nix-(-9999999)),Zug-Wert)
  ; minimize(ReverseSortedKs,Cx,(nix-9999999),Zug-Wert)
  ).


beta_cut(Cx,Zug-Wert,Cx,Zug-Wert):-
	cx_beta(Cx,Beta),
	Wert >= Beta.

alpha_cut(Cx,Zug-Wert,Cx,Zug-Wert):-
	cx_alpha(Cx,Alpha),
	Wert =< Alpha.

improvement(Cx0,WertMax,Zug-Wert,Cx,Zug-Wert):-
	Wert > WertMax,
	set_alpha_of_cx(Wert,Cx0,Cx).

worsening(Cx0,WertMin,Zug-Wert,Cx,Zug-Wert):-
	Wert < WertMin,
	set_beta_of_cx(Wert,Cx0,Cx).

maximize([],_,Zug-Wert,Zug-Wert).
maximize([_-(ZugK-K)|Ks],Cx0,Zug0-Wert0,Zug-Wert):-
  wert(K,Cx0,_-WertK),
  ( beta_cut(Cx0,ZugK-WertK,Cx,Zug1-Wert1)
  ->Rest=[]
  ; improvement(Cx0,Wert0,ZugK-WertK,Cx,Zug1-Wert1)
  ->Rest=Ks
  ; Rest=Ks,
    Wert1 = Wert0,
    Zug1 = Zug0,
    Cx=Cx0
  ),
  maximize(Rest,Cx,Zug1-Wert1,Zug-Wert).


minimize([],_,Zug-Wert,Zug-Wert).
minimize([_-(ZugK-K)|Ks],Cx0,Zug0-Wert0,Zug-Wert):-
  wert(K,Cx0,_-WertK),
  ( alpha_cut(Cx0,ZugK-WertK,Cx,Zug1-Wert1)
  ->Rest=[]
  ; worsening(Cx0,Wert0,ZugK-WertK,Cx,Zug1-Wert1)
  ->Rest=Ks
  ; Rest=Ks,
    Wert1 = Wert0,
    Zug1=Zug0,
    Cx=Cx0
  ),
  minimize(Rest,Cx,Zug1-Wert1,Zug-Wert).



