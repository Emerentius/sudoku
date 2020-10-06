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

    let sudoku = Sudoku::from_str_block(sudoku_str).unwrap();
    if let Some(solution) = sudoku.solution() {
        println!("{}", solution);
    }
}
