#[macro_use]
extern crate criterion;
extern crate sudoku;
use sudoku::Sudoku;
use criterion::Criterion;

#[allow(unused)]
fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    sudokus_str.lines()
        .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
        .collect()
}

fn _1_easy_sudokus_solve_one(c: &mut Criterion) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
	let mut iter = sudokus.iter().cycle().cloned();
	c.bench_function("_1_easy_sudokus_solve_one", |b| b.iter(|| {
		iter.next()
			.unwrap()
			.solve_one()
	}));
}

fn _1_easy_sudokus_solve_all(c: &mut Criterion) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
	let mut iter = sudokus.iter().cycle().cloned();
	c.bench_function("_1_easy_sudokus_solve_all", |b| b.iter(|| {
		iter.next()
			.unwrap()
			.solve_unique()
	}));
}

fn _2_medium_sudokus_solve_one(c: &mut Criterion) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
	let mut iter = sudokus.iter().cycle().cloned();
	c.bench_function("_2_medium_sudokus_solve_one", |b| b.iter(|| {
		iter.next()
			.unwrap()
			.solve_one()
	}));
}

fn _2_medium_sudokus_solve_all(c: &mut Criterion) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
	let mut iter = sudokus.iter().cycle().cloned();
	c.bench_function("_2_medium_sudokus_solve_all", |b| b.iter(|| {
		iter.next()
			.unwrap()
			.solve_unique()
	}));
}

fn _3_hard_sudokus_solve_one(c: &mut Criterion) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
	let mut iter = sudokus.iter().cycle().cloned();
	c.bench_function("_3_hard_sudokus_solve_one", |b| b.iter(|| {
		iter.next()
			.unwrap()
			.solve_one()
	}));
}

fn _3_hard_sudokus_solve_all(c: &mut Criterion) {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
	let mut iter = sudokus.iter().cycle().cloned();
	c.bench_function("_3_hard_sudokus_solve_all", |b| b.iter(|| {
		iter.next()
			.unwrap()
			.solve_unique()
	}));
}

fn _4_generate_filled_sudoku(c: &mut Criterion) {
	c.bench_function("_4_generate_filled_sudoku", |b| b.iter(Sudoku::generate_filled));
}

fn _4_generate_unique_sudoku(c: &mut Criterion) {
	c.bench_function("_4_generate_unique_sudoku", |b| b.iter(Sudoku::generate_unique));
}
criterion_group!(
    benches,
    _1_easy_sudokus_solve_one,
    _1_easy_sudokus_solve_all,
    _2_medium_sudokus_solve_one,
    _2_medium_sudokus_solve_all,
    _3_hard_sudokus_solve_one,
    _3_hard_sudokus_solve_all,
    _4_generate_filled_sudoku,
    _4_generate_unique_sudoku
);
criterion_main!(benches);