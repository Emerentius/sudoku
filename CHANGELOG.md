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

Version 0.4.0 (2017-12-11)
==========================
* Changed parser functions for `Sudoku`
  - Added `from_str_line()` for parsing sudokus in line format. Also added `to_str_line()` for a printable representation of the same.
  - Split `from_str()` into `from_str_block()` and `from_str_block_permissive()`. As the names suggest, the latter is much more forgiving than the former but will also fail in strange ways on malformed data.
* Added `from_bytes()`, `from_bytes_slice()`, `into_bytes()` to `Sudoku` for construction and deconstruction for byte arrays and slices. Errors will be revisited.
* Added new module `parser_errors`

Version 0.4.1 (2018-01-01)
--------------------------
* Add functions for generation of random sudokus: `generate_filled()` and `generate_unique()`
  for random solved and unsolved but uniquely solvable sudokus respectively.
* Implement `Hash`, `PartialOrd`, `Ord` for `Sudoku` and `SudokuLine`.
