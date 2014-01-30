muehle
======

AI-Player for the Nine Man's Morris board game (German: "Mühle"), implemented in Prolog


The Code is all in German for now, sorry for that. :-(

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
The dynamic predicate history/1 is used to keep a history of configurations in FIFO order.

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
