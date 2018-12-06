// Solution to Problem 096 of Project Euler
// https://projecteuler.net/problem=96
//
// For demonstration purposes only. If you're gonna use this
// you may as well not do the problem at all

use sudoku::Sudoku;

fn main() {
    let mut sum = 0;
    let sudokus = include_str!("p096_sudoku.txt");
    for sudoku in sudokus
        .split("Grid")
        .skip(1)
        .map(Sudoku::from_str_block_permissive)
        .map(Result::unwrap)
    {
        let array = sudoku.solve_one().unwrap().to_bytes();
        sum += array[..3].iter().fold(0, |acc, &num| acc * 10 + num as u32);
    }
    println!("{}", sum);
}
