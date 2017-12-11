extern crate sudoku;
use sudoku::Sudoku;

fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    sudokus_str.lines()
        .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
        .collect()
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

    let mut sudoku = Sudoku::from_str_block(sudoku_str).unwrap();
    sudoku.solve();
    println!("{}", sudoku);
}

#[test]
fn solve_2() {
    let sudoku_str = "\
7__|4__|__2 comment
21_|3_5|46_
__9|_28|__1
----------- comment
___|542|3__
___|___|___
__5|817|___
-----------
5__|73_|9__
_63|2_4|_17
8__|__9|__3";

	let mut sudoku = Sudoku::from_str_block(sudoku_str).unwrap();
	sudoku.solve();
	println!("{}", sudoku);
}

#[test]
fn readme() {
    let sudoku_str = "\
___|2__|_63
3__|__5|4_1
__1|__3|98_
---+---+---
___|___|_9_
___|538|___
_3_|___|___
---+---+---
_26|3__|5__
5_3|7__|__8
47_|__1|___";

    let sudoku_str2 = "...2...633....54.1..1..398........9....538....3........263..5..5.37....847...1...";

    let mut sudoku = Sudoku::from_str_block(sudoku_str).unwrap();
    let mut sudoku2 = Sudoku::from_str_line(sudoku_str2).unwrap();
    sudoku.solve();
    sudoku2.solve();
    println!("{}", sudoku);
    println!("{}", sudoku.to_str_line());
    assert!(sudoku == sudoku2);
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

    Sudoku::from_str_block(sudoku_str).unwrap();
}

#[test]
fn solutionless_sudokus() {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/invalid_sudokus.txt") );
    for sudoku in sudokus {
        assert!(sudoku.solve_one().is_none());
    }
}

#[test]
fn is_solved_on_unsolved() {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
    for sudoku in sudokus {
        assert!(!sudoku.is_solved());
    }
}

#[test]
fn is_solved_on_solved() {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_easy_sudokus.txt") );
    for sudoku in sudokus {
        assert!(sudoku.is_solved());
    }
}

#[test]
fn correct_solution_easy_sudokus() {
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_easy_sudokus.txt") );
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
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_medium_sudokus.txt") );
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
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_hard_sudokus.txt") );
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        if let Some(solution) = sudoku.clone().solve_unique() {
            assert_eq!( solved_sudoku, solution);
        } else {
            panic!("Found multiple solutions to sudoku with unique solution or none at all for {}th sudoku:\n{}", i, sudoku);
        }
    }
}

#[test]
fn parse_permissive() {
    let sudokus = [
        // http://norvig.com/sudoku.html
        r"
        4 . . |. . . |8 . 5
        . 3 . |. . . |. . .
        . . . |7 . . |. . .
        ------+------+------
        . 2 . |. . . |. 6 .
        . . . |. 8 . |4 . .
        . . . |. 1 . |. . .
        ------+------+------
        . . . |6 . 3 |. 7 .
        5 . . |2 . . |. . .
        1 . 4 |. . . |. . .
        ",
        // https://projecteuler.net/problem=96
        r"
        Grid 01
        003020600
        900305001
        001806400
        008102900
        700000008
        006708200
        002609500
        800203009
        005010300
        Grid 02
        200080300
        060070084
        030500209
        000105408
        000000000
        402706000
        301007040
        720040060
        004010003
        ",
    ];
    let sudokus_line = [
        "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......",
        "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3..",
   ];

   for (sudoku, line_sudoku) in sudokus.iter().zip(sudokus_line.iter()) {
       let sudoku1 = Sudoku::from_str_block_permissive(sudoku).expect("permissive block parse error");
       let sudoku2 = Sudoku::from_str_line(line_sudoku).expect("line parse error");
       assert!(sudoku1 == sudoku2);
   }

}

#[allow(unused)]
// as it stands SudokuLine seems to be unnameable because it is not exported
// compile time check to see if it is constructable and printable
fn print_line() {
    let sudoku = Sudoku::from_bytes([0; 81]).unwrap();
    let line = sudoku.to_str_line();
    let dereffed_line: &str = &line;
    println!("{}", line);
}
