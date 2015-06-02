Sudoku
======

[![Crates.io Status](http://meritbadge.herokuapp.com/sudoku)](https://crates.io/crates/sudoku)

> A Rust library with the only goal of solving sudokus

# Usage Example

```rust
extern crate sudoku;

use sudoku::Sudoku;

fn main() {
    let sudoku_str =
"___2___63
3____54_1
__1__398_
_______9_
___538___
_3_______
_263__5__
5_37____8
47___1___";

    let mut sudoku = Sudoku::from_str(sudoku_str).unwrap();
    sudoku.solve();
    println!("{}", sudoku);
}
```

# To do list

- [ ] C bindings
- [ ] Replace usages of `HashSet` by a special struct based on a `u16` (we can use each bit as a boolean flag to indicate if the number at the given index is present)
