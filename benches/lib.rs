#![feature(test)]
extern crate test;
use sudoku::strategy::{Strategy, StrategySolver};
use sudoku::Sudoku;

fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    sudokus_str
        .lines()
        .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
        .collect()
}

/// Set of all available strategies, for test purposes
#[allow(unused)]
#[rustfmt::skip]
const ALL_STRATEGIES: &[Strategy] = &[
                                // difficulty as assigned by
                                // SudokuExplainer
    Strategy::NakedSingles,     // 23
    Strategy::HiddenSingles,    // 15
    Strategy::LockedCandidates, // 28
    Strategy::NakedPairs,       // 30
    Strategy::XWing,            // 32
    Strategy::HiddenPairs,      // 34
    Strategy::NakedTriples,     // 36
    Strategy::Swordfish,        // 38
    Strategy::HiddenTriples,    // 40
    Strategy::XyWing,           // 42
    Strategy::XyzWing,          // 44
    Strategy::NakedQuads,       // 50
    Strategy::Jellyfish,        // 52
    Strategy::HiddenQuads,      // 54
    //Strategy::SinglesChain,
];

macro_rules! make_benches {
    ( $sudokus_folder:expr; $($name:ident, $f:expr, $file_name:expr);* ) => {
        $(
            #[bench]
            fn $name (b: &mut test::Bencher) {
                let sudokus = read_sudokus(
                    include_str!( concat!($sudokus_folder, $file_name) )
                );
                let mut sudokus = sudokus.iter().cloned().cycle();
                b.iter(|| {
                    for sudoku in sudokus.by_ref().take(100) {
                        $f(sudoku);
                    }
                })
            }
        )*
    };
}

make_benches!(
    "../sudokus/Lines/";
    sudoku_solve_one_1_easy,     Sudoku::solve_one, "easy_sudokus.txt";
    sudoku_solve_one_2_medium,   Sudoku::solve_one, "medium_sudokus.txt";
    sudoku_solve_one_3_hard,     Sudoku::solve_one, "hard_sudokus.txt";

    sudoku_is_solvable_1_easy,   Sudoku::is_uniquely_solvable, "easy_sudokus.txt";
    sudoku_is_solvable_2_medium, Sudoku::is_uniquely_solvable, "medium_sudokus.txt";
    sudoku_is_solvable_3_hard,   Sudoku::is_uniquely_solvable, "hard_sudokus.txt";

    // the sudokus are proper so this will exhaust the entire search tree
    // trying to find a 2nd solution
    sudoku_solve_all_1_easy,   |s: Sudoku| s.solve_at_most(2), "easy_sudokus.txt";
    sudoku_solve_all_2_medium, |s: Sudoku| s.solve_at_most(2), "medium_sudokus.txt";
    sudoku_solve_all_3_hard,   |s: Sudoku| s.solve_at_most(2), "hard_sudokus.txt"
);

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
fn from_bytes(b: &mut test::Bencher) {
    let sudokus = (0..1000)
        .map(|_| Sudoku::generate_unique().to_bytes())
        .collect::<Vec<_>>();

    b.iter(||
        for &sudoku in &sudokus {
            let _ = Sudoku::from_bytes(sudoku).unwrap();
        }
    )
}

#[bench]
fn parse_line(b: &mut test::Bencher) {
    let sudokus = (0..1000)
        .map(|_| Sudoku::generate_unique().to_str_line())
        .collect::<Vec<_>>();
    let mut sudokus = sudokus.iter().cycle();

    b.iter(|| Sudoku::from_str_line(sudokus.next().unwrap()))
}

#[bench]
fn parse_lines(b: &mut test::Bencher) {
    let sudokus = (0..1000)
        .map(|_| Sudoku::generate_unique().to_str_line())
        .collect::<Vec<_>>();
    let sudokus = sudokus.iter().map(|line| &**line).collect::<Vec<_>>();
    let sudokus = sudokus.join("\n");

    b.iter(|| {
        for line in sudokus.lines() {
            let _ = Sudoku::from_str_line(line);
        }
    })
}

#[bench]
fn is_solved_on_unsolved(b: &mut test::Bencher) {
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/hard_sudokus.txt"));
    b.iter(|| {
        for sudoku in sudokus.iter().cloned() {
            sudoku.is_solved();
        }
    })
}

#[bench]
fn is_solved_on_solved(b: &mut test::Bencher) {
    let solved_sudokus = read_sudokus(include_str!("../sudokus/Lines/solved_hard_sudokus.txt"));
    b.iter(|| {
        for sudoku in solved_sudokus.iter().cloned() {
            sudoku.is_solved();
        }
    })
}

#[bench]
fn strategy_solver_1_easy_sudokus(b: &mut test::Bencher) {
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/easy_sudokus.txt"));
    let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
    let strategies = ALL_STRATEGIES;
    b.iter(|| {
        for sudoku in sudokus_100.iter().cloned() {
            StrategySolver::from_sudoku(sudoku).solve(&strategies).unwrap(); //.unwrap();
        }
    })
}

#[bench]
fn strategy_solver_2_medium_sudokus(b: &mut test::Bencher) {
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/medium_sudokus.txt"));
    let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
    let strategies = ALL_STRATEGIES;
    b.iter(|| {
        for sudoku in sudokus_100.iter().cloned() {
            // solution not guaranteed yet, discard error.
            let _ = StrategySolver::from_sudoku(sudoku).solve(&strategies); //.unwrap();
        }
    })
}

#[bench]
fn canonicalize(b: &mut test::Bencher) {
    let sudokus = (0..1000).map(|_| Sudoku::generate_filled()).collect::<Vec<_>>();
    let mut sudokus = sudokus.iter().cycle();

    b.iter(|| {
        sudokus.next().unwrap().canonicalized();
    })
}
