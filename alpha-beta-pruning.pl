:- use_module(library(record)).

stellung_zug(Von,Zug,Nach):-
  zug(Von,Zug,Nach).

stellung_wert(Stellung,Wert):-
  heuristik(Stellung,weiß,Wert).

horizont(5).

suche_wert(K0,Tiefe0,Alpha,Beta,Zug-Wert):-
  Tiefe is Tiefe0 + 1,
  findall(Z-K,stellung_zug(K0,Z,K),Ks),
  ( 0 is Tiefe0 mod 2
  ->maximize(Ks,Tiefe,(nix-(-9999999)),Alpha,Beta,Zug-Wert)
  ; minimize(Ks,Tiefe, (nix-9999999),Alpha,Beta,Zug-Wert)
  ).

wert(K,Tiefe,Alpha,Beta,Zug-Wert):-
  horizont(Horizont),
  ( Tiefe > Horizont
  ->stellung_wert(K,Wert),
    Zug=horizont
  ; \+ stellung_zug(K,_,_)
  ->stellung_wert(K,Wert),
    Zug=ende
  ; suche_wert(K,Tiefe,Alpha,Beta,Zug-Wert)
  ).

maximize([],_,Zug-Wert,_,_,Zug-Wert).
maximize([ZugK-K|Ks],Tiefe,Zug0-Wert0,Alpha0,Beta,Zug-Wert):-
  wert(K,Tiefe,Alpha0,Beta,_-WertK),
  ( WertK >= Beta
  ->Rest=[],
    Wert1=WertK,
    Zug1=ZugK,
    Alpha=Alpha0
  ; WertK > Wert0
  ->Rest=Ks,
    Wert1 = WertK,
    Zug1 = ZugK,
    Alpha = WertK
  ; Rest=Ks,
    Wert1 = Wert0,
    Zug1 = Zug0,
    Alpha = Alpha0
  ),
  maximize(Rest,Tiefe,Zug1-Wert1,Alpha,Beta,Zug-Wert).


minimize([],_,Zug-Wert,_,_,Zug-Wert).
minimize([ZugK-K|Ks],Tiefe,Zug0-Wert0,Alpha,Beta0,Zug-Wert):-
  wert(K,Tiefe,Alpha,Beta0,_-WertK),
  ( WertK =< Alpha
  ->Rest=[],
    Wert1=WertK,
    Zug1=ZugK,
    Beta=Beta0
  ; WertK < Wert0
  ->Rest=Ks,
    Wert1 = WertK,
    Zug1=ZugK,
    Beta=WertK
  ; Rest=Ks,
    Wert1 = Wert0,
    Zug1=Zug0,
    Beta=Beta0
  ),
  minimize(Rest,Tiefe,Zug1-Wert1,Alpha,Beta,Zug-Wert).



