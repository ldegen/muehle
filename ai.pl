:- module('ai',[find_move/4]).

:- use_module(library(record)).
:- use_module('rules.pl').
:- use_module('configuration.pl').
:- use_module('heuristic.pl').

cfg_player(Cfg,Player):-
	player(Cfg,Player).
legal_move(From,Move,To):-
  move(From,Move,To).

:- dynamic cfg_value_cache/4.

cfg_value_calculate(Cfg,Player,Value):-
	(   victorious(Cfg,Player)
	->  Value=9999999
	;   defeated(Cfg,Player)
	->  Value=(-9999999)
	;   heuristic(Cfg,Player,Value)
	).
cfg_value(Cfg,Player,Value):-
	term_hash(Cfg,Hash),
        (   cfg_value_cache(Hash,Cfg,Player,Value),
	    !
	;   cfg_value_calculate(Cfg,Player,Value),
	    !,
	    assert(cfg_value_cache(Hash,Cfg,Player,Value))
	).

:- record cx(player=white,horizon=5,depth=0,alpha=(-9999999),beta=9999999).


me(Cx):-
	cx_depth(Cx,D),
	0 is D mod 2.

down(Cx0,Cx):-
	cx_depth(Cx0,D0),
	D is D0 + 1,
	set_depth_of_cx(D,Cx0,Cx).

find_move(Cfg,Horizon,Move,Value):-
	cfg_player(Cfg,Player),
	make_cx([horizon(Horizon),player(Player)],Cx),
	value(Cfg,Cx,Move-Value).


value(Cfg,Cx,Move-Value):-
  cx_player(Cx,Player),
  ( passed_horizon(Cx)
  ->cfg_value(Cfg,Player,Value),
    Move=horizon
  ; game_over(Cfg)
  ->cfg_value(Cfg,Player,Value),
    Move=game_over
  ; value_recursive(Cfg,Cx,Move-Value)
  ).


passed_horizon(Cx):-
	cx_horizon(Cx,H),
	cx_depth(Cx,D),
	D>H.
game_over(Cfg):-
	\+ legal_move(Cfg,_,_).

value_recursive(Cfg0,Cx0,Move-Value):-
  cx_player(Cx0,Player),
  findall(H-(Z-Cfg),(legal_move(Cfg0,Z,Cfg),cfg_value(Cfg,Player,H)),Cfgs),
  keysort(Cfgs,SortedCfgs),
  reverse(SortedCfgs,ReverseSortedCfgs),
  down(Cx0,Cx),
  ( me(Cx0)
  ->maximize(ReverseSortedCfgs,Cx,(nix-(-9999999)),Move-Value)
  ; minimize(ReverseSortedCfgs,Cx,(nix-9999999),Move-Value)
  ).


beta_cut(Cx,Move-Value,Cx,Move-Value):-
	cx_beta(Cx,Beta),
	Value >= Beta.

alpha_cut(Cx,Move-Value,Cx,Move-Value):-
	cx_alpha(Cx,Alpha),
	Value =< Alpha.

improvement(Cx0,ValueMax,Move-Value,Cx,Move-Value):-
	Value > ValueMax,
	set_alpha_of_cx(Value,Cx0,Cx).

worsening(Cx0,ValueMin,Move-Value,Cx,Move-Value):-
	Value < ValueMin,
	set_beta_of_cx(Value,Cx0,Cx).

maximize([],_,Move-Value,Move-Value).
maximize([_-(MoveK-Cfg)|Cfgs],Cx0,Move0-Value0,Move-Value):-
  value(Cfg,Cx0,_-ValueK),
  ( beta_cut(Cx0,MoveK-ValueK,Cx,Move1-Value1)
  ->Rest=[]
  ; improvement(Cx0,Value0,MoveK-ValueK,Cx,Move1-Value1)
  ->Rest=Cfgs
  ; Rest=Cfgs,
    Value1 = Value0,
    Move1 = Move0,
    Cx=Cx0
  ),
  maximize(Rest,Cx,Move1-Value1,Move-Value).


minimize([],_,Move-Value,Move-Value).
minimize([_-(MoveK-Cfg)|Cfgs],Cx0,Move0-Value0,Move-Value):-
  value(Cfg,Cx0,_-ValueK),
  ( alpha_cut(Cx0,MoveK-ValueK,Cx,Move1-Value1)
  ->Rest=[]
  ; worsening(Cx0,Value0,MoveK-ValueK,Cx,Move1-Value1)
  ->Rest=Cfgs
  ; Rest=Cfgs,
    Value1 = Value0,
    Move1=Move0,
    Cx=Cx0
  ),
  minimize(Rest,Cx,Move1-Value1,Move-Value).
