Sudoku
======

[![Crates.io Status](http://meritbadge.herokuapp.com/sudoku)](https://crates.io/crates/sudoku) [![Build Status](https://travis-ci.org/Emerentius/sudoku.svg?branch=master)](https://travis-ci.org/Emerentius/sudoku)

Utilities for classical 9x9 sudokus.

This library currently offers extremely fast sudoku solving, a basic sudoku
generator and a prototype solver using human strategies. The fast solver is
based on [jczsolve](http://forum.enjoysudoku.com/3-77us-solver-2-8g-cpu-testcase-17sodoku-t30470-210.html#p249309)
which is currently and to the best knowledge of the author one of the world's fastest sudoku
solver algorithm. A few modifications were made to improve the speed further.  
Some competing solvers are [fsss2](https://github.com/dobrichev/fsss2), [SK_BFORCE2](https://github.com/GPenet/SK_BFORCE2) and [tdoku](https://github.com/t-dillon/tdoku). The latter is particularly strong for sudokus that don't have a unique solution or that are very hard.
See also benchmarks done by tdoku's author: [benchmarks](https://github.com/t-dillon/tdoku/tree/master/benchmarks)   ([results at time of writing](https://github.com/t-dillon/tdoku/tree/9d62b98/benchmarks)).

The strategy solver is capable of applying a few simple strategies, namely naked and hidden singles, locked candidates,
naked and hidden subsets (Pairs, Triples, Quads) and basic fish (X-Wing, Swordfish, Jellyfish). An explanation of
these strategies and more can be found at <http://www.sudokuwiki.org/Strategy_Families>.

A future goal is the extension and further optimization of the strategy solver,
so that most sudokus can be graded, hinted and the solution path explained. With the ability to
grade sudokus, puzzles of any desired desired difficulty can also be generated.

## Example

```rust
use sudoku::Sudoku;

// Sudokus can be created from &str's in both block or line formats or directly from bytes.
// here, an example in line format
let sudoku_line = "...2...633....54.1..1..398........9....538....3........263..5..5.37....847...1...";

let sudoku = Sudoku::from_str_line(sudoku_line).unwrap();

// Solve, print or convert the sudoku to another format
if let Some(solution) = sudoku.solution() {
    // print the solution in line format
    println!("{}", solution);

    // or return it as a byte array
    let cell_contents: [u8; 81] = solution.to_bytes();
}
```
