#![feature(test)]
extern crate test;
extern crate sudoku;
use sudoku::Sudoku;

fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    let sudokus_str = sudokus_str.replace("\r\n", "\n");
    let mut sudokus = vec![];
    for i in 0.. {
        // 9 lines with 9 cells each + 1 linefeed character per line
        // + 1 LF char between each sudoku
        // 9*(9+1) + 1
        let rg = 0+i*91..90+i*91;
        if rg.end > sudokus_str.len() { break }
        sudokus.push( Sudoku::from_str( &sudokus_str[rg] ).expect("Benchmark sudokus file contains sudoku in incorrect format") )
    }
    sudokus
}

#[bench]
fn easy_sudokus(b: &mut test::Bencher) {
    let sudokus = read_sudokus( &include_str!("easy_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for mut sudoku in sudokus_1000.iter().cloned() { sudoku.solve(); }
	})
}


#[bench]
fn medium_sudokus(b: &mut test::Bencher) {
    let sudokus = read_sudokus( &include_str!("medium_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for mut sudoku in sudokus_1000.iter().cloned() { sudoku.solve(); }
	})
}

#[bench]
fn hard_sudokus(b: &mut test::Bencher) {
    let sudokus = read_sudokus( &include_str!("hard_sudokus.txt") );
    let sudokus_1000 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();;
	b.iter(|| {
		for mut sudoku in sudokus_1000.iter().cloned() { sudoku.solve(); }
	})
}
