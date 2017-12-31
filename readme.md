Sudoku
======

[![Crates.io Status](http://meritbadge.herokuapp.com/sudoku)](https://crates.io/crates/sudoku) [![Build Status](https://travis-ci.org/Emerentius/sudoku.svg?branch=master)](https://travis-ci.org/Emerentius/sudoku)

> A Rust library with the only goal of solving sudokus

# Usage Example

```rust
extern crate sudoku;

use sudoku::Sudoku;

fn main() {
    // in block format
    let sudoku_str = "\
___|2__|_63
3__|__5|4_1
__1|__3|98_
---+---+---
___|___|_9_
___|538|___
_3_|___|___
---+---+---
_26|3__|5__
5_3|7__|__8
47_|__1|___";

    // or line format (same sudoku)
    let sudoku_str2 = "...2...633....54.1..1..398........9....538....3........263..5..5.37....847...1...";

    let mut sudoku = Sudoku::from_str_block(sudoku_str).unwrap();
    let mut sudoku2 = Sudoku::from_str_line(sudoku_str2).unwrap();

    sudoku.solve();
    sudoku2.solve();

    // print as block
    println!("{}", sudoku);
    // or as line
    println!("{}", sudoku.to_str_line());

    assert!(sudoku == sudoku2);
}
```

# Planned

- [ ] Sudoku generation
    - [X] generate uniquely solvable sudokus
    - [ ] grade difficulty
- [ ] Step-by-step hinting & solving with pluggable strategies
- [ ] Detailed errors for invalid sudokus
