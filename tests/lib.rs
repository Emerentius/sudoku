extern crate sudoku;
use sudoku::Sudoku;

#[test]
fn solve_1() {
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

#[test]
fn solve_2() {
    let sudoku_str = "\
7__|4__|__2
21_|3_5|46_
__9|_28|__1
___|542|3__
___|___|___
__5|817|___
5__|73_|9__
_63|2_4|_17
8__|__9|__3";

	let mut sudoku = Sudoku::from_str(sudoku_str).unwrap();
	sudoku.solve();
	println!("{}", sudoku);
}

#[test]
#[should_panic]
fn wrong_format_1() {
    let sudoku_str =
"___2___63
3____54_1
__1__398_
_______9_
___538___
_3_______
_263__5__
5_37____8";

    Sudoku::from_str(sudoku_str).unwrap();
}
