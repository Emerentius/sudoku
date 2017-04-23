Version 0.2.0 (2016-02-07)
==========================
* Implemented new and faster solver based on exact cover matrix capable of solving sudokus of all difficulties quickly (on the order of milliseconds or less on a modern desktop)

Version 0.2.1 (2017-04-08)
--------------------------
* Fixed a logic bug that led to returning multiple invalid solutions for some sudokus.

Version 0.3.0 (2017-04-17)
==========================
* Switch algorithm to that of the Jsolve sudoku solver (with some minor changes), increasing speed more than 10x.

Version 0.3.1 (2017-04-18)
--------------------------
* Build on stable rust again

Version 0.3.2 (2017-04-18)
--------------------------
* Bugfix: `sudoku.is_solved()` incorrectly returned true when `sudoku` was not solved,
  but solvable through naked single tactics alone
