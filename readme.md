Sudoku
======

`sudoku` is a Rust library with the only goal of solving sudokus.
You can use it as shown in the following example:

```
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

# To d