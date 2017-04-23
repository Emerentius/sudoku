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

#[test]
fn solutionless_sudokus() {
    let sudokus = read_sudokus( include_str!("../sudokus/invalid_sudokus_reformatted.txt") );
    for sudoku in sudokus {
        assert!(sudoku.solve_one().is_none());
    }
}

#[test]
fn is_solved_on_unsolved() {
    let sudokus = read_sudokus( include_str!("../sudokus/easy_sudokus.txt") );
    for sudoku in sudokus {
        assert!(!sudoku.is_solved());
    }
}

#[test]
fn is_solved_on_solved() {
    let sudokus = read_sudokus( include_str!("../sudokus/solved_easy_sudokus.txt") );
    for sudoku in sudokus {
        assert!(sudoku.is_solved());
    }
}

#[test]
fn correct_solution_easy_sudokus() {
    let sudokus = read_sudokus( include_str!("../sudokus/easy_sudokus.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/solved_easy_sudokus.txt") );
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        if let Some(solution) = sudoku.clone().solve_unique() {
            assert_eq!( solved_sudoku, solution);
        } else {
            panic!("Found multiple solutions to sudoku with unique solution or none at all for {}th sudoku:\n{}", i, sudoku);
        }
    }
}

#[test]
fn correct_solution_medium_sudokus() {
    let sudokus = read_sudokus( include_str!("../sudokus/medium_sudokus.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/solved_medium_sudokus.txt") );
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        if let Some(solution) = sudoku.clone().solve_unique() {
            assert_eq!( solved_sudoku, solution);
        } else {
            panic!("Found multiple solutions to sudoku with unique solution or none at all for {}th sudoku:\n{}", i, sudoku);
        }
    }
}

#[test]
fn correct_solution_hard_sudokus() {
    let sudokus = read_sudokus( include_str!("../sudokus/hard_sudokus.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/solved_hard_sudokus.txt") );
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        if let Some(solution) = sudoku.clone().solve_unique() {
            assert_eq!( solved_sudoku, solution);
        } else {
            panic!("Found multiple solutions to sudoku with unique solution or none at all for {}th sudoku:\n{}", i, sudoku);
        }
    }
}

#[test]
#[ignore]
fn correct_solution_top50k() {
    let sudokus = read_sudokus( include_str!("../sudokus/top50k.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/solved_top50k.txt") );
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        if let Some(solution) = sudoku.clone().solve_unique() {
            assert_eq!( solved_sudoku, solution);
        } else {
            panic!("Found multiple solutions to sudoku with unique solution or none at all for {}th sudoku:\n{}", i, sudoku);
        }
    }
}

#[test]
#[ignore]
fn correct_solution_sudoku17() {
    let sudokus = read_sudokus( include_str!("../sudokus/sudoku17.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/solved_sudoku17.txt") );
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        if let Some(solution) = sudoku.clone().solve_unique() {
            assert_eq!( solved_sudoku, solution);
        } else {
            panic!("Found multiple solutions to sudoku with unique solution or none at all for {}th sudoku:\n{}", i, sudoku);
        }
    }
}
