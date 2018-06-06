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
fn easy_sudokus_solve_one(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.solve_one(); }
	})
}

#[bench]
fn easy_sudokus_solve_all(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.is_uniquely_solvable(); }
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
fn medium_sudokus_solve_all(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.is_uniquely_solvable(); }
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
fn hard_sudokus_solve_all(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for sudoku in sudokus_1000.iter().cloned() { sudoku.is_uniquely_solvable(); }
	})
}

#[bench]
fn generate_filled_sudoku(b: &mut test::Bencher) {
	b.iter(Sudoku::generate_filled)
}

#[bench]
fn generate_unique_sudoku(b: &mut test::Bencher) {
	b.iter(Sudoku::generate_unique)
}

// this test is probabilistic in nature
// if an error occurs, note down the sudoku that it generated
#[bench]
fn shuffle(b: &mut test::Bencher) {
    let mut sudoku = Sudoku::generate_filled();
	b.iter(|| {
		sudoku.shuffle();
		test::black_box(sudoku);
	});
}

#[bench]
fn parse_line(b: &mut test::Bencher) {
	let sudokus = (0..1000).map(|_| Sudoku::generate_unique().to_str_line())
		.collect::<Vec<_>>();
	let mut sudokus = sudokus.iter().cycle();

	b.iter(|| {
		Sudoku::from_str_line(sudokus.next().unwrap())
	})
}

#[bench]
fn parse_lines(b: &mut test::Bencher) {
	let sudokus = (0..1000).map(|_| Sudoku::generate_unique().to_str_line())
		.collect::<Vec<_>>();
	let sudokus = sudokus.iter().map(|line| &**line)
		.collect::<Vec<_>>();
	let sudokus = sudokus.join("\n");

	b.iter(|| {
		for line in sudokus.lines() {
			let _ = Sudoku::from_str_line(line);
		}
	})
}

#[bench]
fn is_solved_on_unsolved(b: &mut test::Bencher) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
	b.iter(||
		for sudoku in sudokus.iter().cloned() {
			sudoku.is_solved();
		}

	)
}

#[bench]
fn is_solved_on_solved(b: &mut test::Bencher) {
    let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_hard_sudokus.txt") );
	b.iter(||
		for sudoku in solved_sudokus.iter().cloned() {
			sudoku.is_solved();
		}

	)
}
