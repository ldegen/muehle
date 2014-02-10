muehle
======

The Nine Men's Morris board game (German: "Mühle"), implemented in Prolog. Including a AI-oponent. Yay! :-)

The source code was originally written in German, I am currently translating it, please bear with me.

News
----
feb, 10th 2014:
- translation to English almost done
  The interactive toplevel and the ai code is not yet translated, so I keep the old german code files for now.
- The record term for representing game state is usefull as an internal representation, but does not look well
  in for instance an URL. I found a more compact schema that basically interpretes the 24 Board positions as
  digits of a base-3 integer(0: empty, 1: white, 2: black); with this 
  it is possible to write any board configuration as an integer between
  0 (empty board) and 3^24-1 (the whole board full of black pieces).
  Add 4 decimal digits to represent the counters and another boolean (actually I stick with {1,2} for consistency)
  and you end up with something like 6070/94143329731/2 to represent the configurationafter a7,b6,d6,f6,e4.

feb, 3th 2014: some updates
- started translating the code to English
- work started on having a simple REST frontend, so eventually we may enjoy some sort of web interface.
- fixed some really stupid bugs that kept the AI from realizing its own defeat.
- while addressing the fields through numbers helps with the implementation, I think I will adapt the
  WMD notation for the interface.

Prequisites
-----------

I am using [SWI-Prolog][http://swi-prolog.org], but porting to other systems should be easy.

How to run it
-------------
To start the interactive toplevel, fire up swipl, consult muehle.pl and try

~~~~
% start a game, play white ("weiß"), ki plays black ("schwarz") and uses a search horizon of 4.

?- partie_starten(weiß, 4).
~~~~

The Board
---------
The fields on the boards are numbered like this:

~~~~

 1 --------- 2 --------- 3   
 |           |           | 
 |  9  ----- 10-----11 	 | 
 |   |       |	     |   | 
 |   |	 17-18 -19   |   | 
 |   |	 |       |   |   | 
 8 -16 -24      20 -12 - 4  
 |   |	 |       |   |   | 
 |   |  23 -22 -21   |   | 
 |   |       |       |   | 
 |  15 -----14 -----13 	 | 
 |           |           | 
 7 --------- 6 --------- 5   

~~~~

Commands
--------

~~~~
16.       put piece at field 16
16,2.     put piece at field 16 and remove the piece at field 2
15-16.    move piece from field 15 to field 16
15-16,2.  move piece from field 15 to field 16 and remove piece at field 2
tipp(2).  ask the AI for a hint, using a search horizon of 2
undo.     take back the last move.
end_of_file.  abort the game.
~~~~

Misc
----
The dynamic predicate history/1 is used to keep a history of configurations in LIFO order.
You can continue a previously interrupted match with something like this:
~~~~
?- history(A),!,partie_fortsetzen(A,weiß,4).
~~~~

Implementation Notes
--------------------
I created this project because I was curious how easy/difficult it would be to create a useable AI in Prolog.

There are much more sophisticated AIs, this particular one does not (yet) use transposition tables, 
no iterative deepening, no nothings. Just basic minmax-Search with alpha-beta-pruning.
The heuristik for evaluating game configurations beyond the search horizon isn't particularily clever either:
it mainly compares the number of pieces on the board and degrees of freedom left for moving them.

I was suprised by how easily this rather naive implementation was able to beat me. otoh, I am probably not a very
good player :-)

Search-time for 5 levels is below 20s on my notebooks. This could probably be improved quiet a bit, but it is ok for now.

Quite annoyingly, the AI does currently not detect draw-situations (i.e. circular paths).

The interactive toplevel is very basic. It is just what I needed for my experiments.
I think about adding a nice web frontend. Should be easy, at least with SWI-Prolog.
