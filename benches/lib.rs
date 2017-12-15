#![feature(test)]
extern crate test;
extern crate sudoku;
use sudoku::Sudoku;

fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    sudokus_str.lines()
        .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
        .collect()
}

#[bench]
fn easy_sudokus_solve(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for mut sudoku in sudokus_1000.iter().cloned() { sudoku.solve(); }
	})
}

#[bench]
fn easy_sudokus_solve_unique(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_unique(); }
	})
}

#[bench]
fn easy_sudokus_solve_one(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_one(); }
	})
}

#[bench]
fn easy_sudokus_solve_at_most_100(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_at_most(100); }
	})
}

#[bench]
fn medium_sudokus_solve(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for mut sudoku in sudokus_1000.iter().cloned() { sudoku.solve(); }
	})
}

#[bench]
fn medium_sudokus_solve_unique(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_unique(); }
	})
}

#[bench]
fn medium_sudokus_solve_one(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_one(); }
	})
}

#[bench]
fn medium_sudokus_solve_at_most_100(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_at_most(100); }
	})
}

#[bench]
fn hard_sudokus_solve(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for mut sudoku in sudokus_1000.iter().cloned() { sudoku.solve(); }
	})
}

#[bench]
fn hard_sudokus_solve_unique(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_unique(); }
	})
}

#[bench]
fn hard_sudokus_solve_one(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_one(); }
	})
}

#[bench]
fn hard_sudokus_solve_at_most_100(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_at_most(100); }
	})
}
