extern crate sudoku;
use sudoku::Sudoku;

fn main() {
    for _ in 0..100 {
        loop {
            let mut sudoku = Sudoku::generate_unique();
            match sudoku.se_grade() {
                Ok(grade) if grade > 53 => {
                    println!("{}", sudoku.to_str_line());
                    break
                }
                Ok(grade) => {
                    //println!("too low: {}", grade);
                }
                Err(_grade) => {
                    //println!("too high");
                    //println!("{}", sudoku.to_str_line());
                    //break
                }
            }
        }
    }
}
