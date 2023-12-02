<!-- * Print textual representation of sudoku board with remaining candidates for unsolved cells. -->

Unreleased
==========
* Raise minimum Rust version to 1.51
* `Sudoku::canonicalized`. Maps all sudokus of an equivalence class to the same sudoku
  and counts automorphisms.
  Limited to uniquely solvable sudokus (for now).
* New strategies:
  - XyWing
  - XyzWing
  - Mutant Swordfish
  - Mutant Jellyfish
* Generation of symmetrical sudokus. The desired symmetry can be chosen via the `Symmetry` enum.
* Rename many of `Sudoku`'s methods
  - `solve_unique` -> `solution`
  - `solve_one` -> `some_solution`
  - `count_at_most` -> `solutions_count_up_to`
  - `solve_at_most` -> `solutions_up_to`
  - `solve_at_most_buffer` -> `solutions_up_to_buffer`
  - `generate_filled` -> `generate_solved`
  - `generate_unique` -> `generate`
  - `generate_unique_with_symmetry` -> `generate_with_symmetry`
  - `generate_unique_from` -> `generate_from`
  - `generate_unique_with_symmetry_from` -> `generate_with_symmetry_from`
* Add new APIs:
  * `Sudoku::shuffled`
  * two functions for generating sudokus with a user-chosen RNG:
    * `generate_solved_with_rng`
    * `generate_with_symmetry_and_rng_from`
* 
* Improved errors for `Sudoku` methods.
  Errors now implement `std::error::Error` and none of them return `Result<T, ()>` anymore.
  Moved `parse_errors` module to `errors`.

Version 0.7.0 (2018-08-19)
==========================
* Raise minimum Rust version to 1.28
* Add `StrategySolver` for solving sudokus using human strategies
  as well as various helper types, including a set of position types.
  For a start, 6 kinds of strategies are available which separate
  into a total of 12 strategies.
* Switch default sudoku print format to the line format
* Add `SudokuBlock` wrapper to print a grid representation
  created by `display_block()` on `Sudoku`
* Remove `Sudoku::solve`. Use `solve_one`, `solve_unique` or `solve_at_most`.
* Rename <br>
   `(Pub)Entry`  -> `InvalidEntry` <br>
   `LineFormatParseError` -> `LineParseError` <br>
   `BlockFormatParseError` -> `BlockParseError`

Version 0.6.2 (2018-06-24)
==========================
* Add `solve_at_most_buffer`, primarily for C FFI

Version 0.6.1 (2018-06-6)
=========================
* Improve upon jczsolve and raise performance by ~15-20%
* Allow semicolons and commas (';', ',') as comment delimiters in line sudokus

Version 0.6.0 (2018-05-30)
==========================
* Raise minimum Rust version to 1.26
* Add `generate_unique_from(sudoku)`
* Add `shuffle()` which transforms a sudoku into a random equivalent sudoku.
* Add `n_clues()` for the number of filled cells

Version 0.5.0 (2018-02-14)
==========================
* Optional `serde` support
* Implemented the JCZsolve algorithm which is to date and to the best knowledge of the author
  the fastest sudoku solver in existence.
* Added `unchecked_indexing` feature
  While testing hasn't shown any incorrect indexing, bounds checks are activated by default to guarantee memory safety in case of a bug. This lowers the speed by 2-12% and can be deactivated by the aforementioned feature
* Added two new functions `count_at_most(limit)` and `is_uniquely_solvable()` which run slightly faster
  than their equivalents that return the solved grid.

Version 0.4.1 (2018-01-01)
==========================
* Add functions for generation of random sudokus: `generate_filled()` and `generate_unique()`
  for random solved and unsolved but uniquely solvable sudokus respectively.
* Implement `Hash`, `PartialOrd`, `Ord` for `Sudoku` and `SudokuLine`.

Version 0.4.0 (2017-12-11)
==========================
* Changed parser functions for `Sudoku`
  - Added `from_str_line()` for parsing sudokus in line format. Also added `to_str_line()` for a printable representation of the same.
  - Split `from_str()` into `from_str_block()` and `from_str_block_permissive()`. As the names suggest, the latter is much more forgiving than the former but will also fail in strange ways on malformed data.
* Added `from_bytes()`, `from_bytes_slice()`, `into_bytes()` to `Sudoku` for construction and deconstruction for byte arrays and slices. Errors will be revisited.
* Added new module `parser_errors`

Version 0.3.2 (2017-04-18)
==========================
* Bugfix: `sudoku.is_solved()` incorrectly returned true when `sudoku` was not solved,
  but solvable through naked single tactics alone

Version 0.3.1 (2017-04-18)
==========================
* Build on stable rust again

Version 0.3.0 (2017-04-17)
==========================
* Switch algorithm to that of the Jsolve sudoku solver (with some minor changes), increasing speed more than 10x.

Version 0.2.1 (2017-04-08)
==========================
* Fixed a logic bug that led to returning multiple invalid solutions for some sudokus.

Version 0.2.0 (2016-02-07)
==========================
* Implemented new and faster solver based on exact cover matrix capable of solving sudokus of all difficulties quickly (on the order of milliseconds or less on a modern desktop)
