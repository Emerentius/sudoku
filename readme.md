Sudoku
======

[![Crates.io Status](http://meritbadge.herokuapp.com/sudoku)](https://crates.io/crates/sudoku) [![Build Status](https://travis-ci.org/Emerentius/sudoku.svg?branch=master)](https://travis-ci.org/Emerentius/sudoku)

> A Rust library with the only goal of solving sudokus

# Usage Example

```rust
extern crate sudoku;

use sudoku::Sudoku;

fn main() {
    // Pipes are ignored, you can also omit them
    let sudoku_str = "\
___|2__|_63
3__|__5|4_1
__1|__3|98_
___|___|_9_
___|538|___
_3_|___|___
_26|3__|5__
5_3|7__|__8
47_|__1|___";

    let mut sudoku = Sudoku::from_str(sudoku_str).unwrap();
    sudoku.solve();
    println!("{}", sudoku);
}
```

# Planned

- [ ] Sudoku generation
- [ ] Step-by-step hinting & solving with pluggable strategies
- [ ] Detailed errors for invalid sudokus
