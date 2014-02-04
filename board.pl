:- module('board',[
  empty_board/1,
  board_set/3,
  board_set/4,
  field/1,
  adjacent/2,
  mill/3,
  in_mill/2,
  board_field_piece/3,
  write_board/1
]).
:- use_module('setters.pl').
:- use_module('bake.pl').

arg0(N,T,Arg):-
	when((ground(N);ground(M)),succ(N,M)),
	arg(M,T,Arg).


:- setters(board/24).
empty_board(board(
              empty,empty,empty,empty,
              empty,empty,empty,empty,
              empty,empty,empty,empty,
              empty,empty,empty,empty,
              empty,empty,empty,empty,
              empty,empty,empty,empty
           )
         ).

board_set([],In,In).
board_set([Field=Player|FieldsPlayers],In,Out):-
	board_set(Field,In,Player,In1),
	board_set(FieldsPlayers,In1,Out).

field(A):-
	between(1,24,A).

field_square(Field,Square):-
	field_coordinates(Field,_,Square).

field_line(Field,Line):-
	field_coordinates(Field,Line,_),
	1 is Line mod 2.

field_coordinates(Field,Angle,Square):-
	(   ground(Field)
	->  Angle is Field mod 8,
	    Square is Field // 8
	;   between(0,7,Angle),
	    between(0,2,Square),
	    Field is Square * 8 + Angle
	).

adjacent_(A,B):-
  adjacent_square(A,B).
adjacent_(A,B):-
  adjacent_line(B,A).

adjacent_square(A,B):-
	field_square(A,Fg),
	field_square(B,Fg),
	Dist is (A-B) mod 8,
	memberchk(Dist,[1,7]).

adjacent_line(A,B):-
	field_line(A,Sp),
	field_line(B,Sp),
	field_square(A,FgA),
	field_square(B,FgB),
	1 is abs(FgA-FgB).

:- bake(adjacent(A1,B1),(adjacent_(A,B),succ(A,A1),succ(B,B1))).

board_field_piece(Board,Field,Player):-
  arg(Field,Board,Player).

mill(1,2,3).
mill(9,10,11).
mill(17,18,19).
mill(8,16,24).
mill(4,12,20).
mill(21,22,23).
mill(13,14,15).
mill(5,6,7).
mill(1,7,8).
mill(9,15,16).
mill(17,23,24).
mill(2,10,18).
mill(6,14,22).
mill(19,20,21).
mill(11,12,13).
mill(3,4,5).

perm(A,B,C,A,B,C).
perm(A,B,C,A,C,B).
perm(A,B,C,B,A,C).
perm(A,B,C,B,C,A).
perm(A,B,C,C,A,B).
perm(A,B,C,C,B,A).


:- bake(mill_perm(A,B,C),(
	perm(A,B,C,A0,B0,C0),
	mill(A0,B0,C0))).

in_mill(Board,Field):-
	mill_perm(Field,B,C),
	arg(Field,Board,Player),
	arg(B,Board,Player),
	arg(C,Board,Player).


player_symbol(white,'(W)').
player_symbol(black,'(B)').
player_symbol(empty,'   ').

format_piece(_,Player):-
	player_symbol(Player,Symbol),
	write(Symbol).

:- format_predicate('F',format_figur(_,_)).

map_args([],_,[]).
map_args([A|More],In,[V|Out]):-
	arg(A,In,V),
	map_args(More,In,Out).


write_board(Board):-
	map_args([
	    1, 2, 3,
	    9, 10, 11,
	    17,18,19,
	    8,16,24,20,12,4,
	    23,22,21,
	    15,14,13,
	    7,6,5
	],Board,Pieces),
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
	format(Format,Pieces).


