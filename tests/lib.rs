extern crate sudoku;
use sudoku::Sudoku;

fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    sudokus_str
        .lines()
        .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
        .collect()
}

#[test]
fn solve_1() {
    let sudoku_str = "___2___63
3____54_1
__1__398_
_______9_
___538___
_3_______
_263__5__
5_37____8
47___1___";

    let sudoku = Sudoku::from_str_block(sudoku_str).unwrap();
    let sudoku = sudoku.solve_unique().unwrap();
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

    let sudoku = Sudoku::from_str_block(sudoku_str).unwrap();
    let sudoku = sudoku.solve_unique().unwrap();
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

    let sudoku = Sudoku::from_str_block(sudoku_str)
        .unwrap()
        .solve_unique()
        .unwrap();
    let sudoku2 = Sudoku::from_str_line(sudoku_str2)
        .unwrap()
        .solve_unique()
        .unwrap();
    println!("{}", sudoku.display_block());
    println!("{}", sudoku.to_str_line());
    assert!(sudoku == sudoku2);
}

#[test]
#[should_panic]
fn wrong_format_1() {
    let sudoku_str = "___2___63
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
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/invalid_sudokus.txt"));
    for sudoku in sudokus {
        assert!(sudoku.solve_one().is_none());
    }
}

#[test]
fn is_solved_on_unsolved() {
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/easy_sudokus.txt"));
    for sudoku in sudokus {
        assert!(!sudoku.is_solved());
    }
}

#[test]
fn is_solved_on_solved() {
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/solved_easy_sudokus.txt"));
    for sudoku in sudokus {
        assert!(sudoku.is_solved());
    }
}

#[test]
#[should_panic]
fn solve_unique_multiple_solutions() {
    // an empty grid
    // the ultimate sudoku with multiple solutions
    let sudoku = Sudoku::from_bytes([0; 81]).unwrap();
    sudoku.solve_unique().unwrap();
}

#[test]
fn correct_solution_easy_sudokus() {
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/easy_sudokus.txt"));
    let solved_sudokus = read_sudokus(include_str!("../sudokus/Lines/solved_easy_sudokus.txt"));
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        let solutions = sudoku.solve_at_most(2);
        match solutions.len() {
            1 => assert_eq!(solved_sudoku, solutions[0]),
            0 => panic!("Found no solution for {}. sudoku:\n{}", i, sudoku.to_str_line()),
            _ => panic!(
                "Found multiple solutions for {}. sudoku\n{})",
                i,
                sudoku.to_str_line()
            ),
        }
    }
}

#[test]
fn correct_solution_medium_sudokus() {
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/medium_sudokus.txt"));
    let solved_sudokus = read_sudokus(include_str!("../sudokus/Lines/solved_medium_sudokus.txt"));
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        let solutions = sudoku.solve_at_most(2);
        match solutions.len() {
            1 => assert_eq!(solved_sudoku, solutions[0]),
            0 => panic!("Found no solution for {}. sudoku:\n{}", i, sudoku.to_str_line()),
            _ => panic!(
                "Found multiple solutions for {}. sudoku\n{})",
                i,
                sudoku.to_str_line()
            ),
        }
    }
}

#[test]
fn correct_solution_hard_sudokus() {
    let sudokus = read_sudokus(include_str!("../sudokus/Lines/hard_sudokus.txt"));
    let solved_sudokus = read_sudokus(include_str!("../sudokus/Lines/solved_hard_sudokus.txt"));
    let mut no_solution_sudokus = vec![];
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        let solutions = sudoku.solve_at_most(2);
        match solutions.len() {
            1 => assert_eq!(solved_sudoku, solutions[0]),
            0 => no_solution_sudokus.push((i, sudoku)), //panic!("Found no solution for {}. sudoku:\n{}", i, sudoku.to_str_line()),
            _ => panic!(
                "Found multiple solutions for {}. sudoku\n{})",
                i,
                sudoku.to_str_line()
            ),
        }
    }
    if !no_solution_sudokus.is_empty() {
        println!("Found no solution for the following sudokus:");
        for (i, sudoku) in no_solution_sudokus {
            println!("{:2}: {}", i, sudoku.to_str_line());
        }
        panic!();
    }
}
/*
#[test]
fn unsolved_hard() {
    let select = [3, 4, 5, 6, 7, 9, 10, 11, 13, 18, 19];
    let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
    let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_hard_sudokus.txt") );
    let mut no_solution_sudokus = vec![];
    for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
        if !select.contains(&i) {
            continue
        }
        let solutions = sudoku.solve_at_most(2);
        match solutions.len() {
            1 => assert_eq!( solved_sudoku, solutions[0]),
            0 => no_solution_sudokus.push((i, sudoku)), //panic!("Found no solution for {}. sudoku:\n{}", i, sudoku.to_str_line()),
            _ => panic!("Found multiple solutions for {}. sudoku\n{})", i, sudoku.to_str_line()),
        }
    }
    if no_solution_sudokus.len() != select.len() {
        panic!("Solved more!");
    } else {
        panic!();
    }
    /*
    if !no_solution_sudokus.is_empty() {
        println!("Found no solution for the following sudokus:");
        for (i, sudoku) in no_solution_sudokus {
            println!("{:2}: {}", i, sudoku.to_str_line());
        }
        panic!();
    }
    */
}
*/
// this test is probabilistic in nature
// if an error occurs, note down the sudoku that it generated
#[test]
fn generate_filled_sudoku_correctness() {
    for _ in 0..1000 {
        let sudoku = Sudoku::generate_filled();
        let solved_sudoku = sudoku.solve_one();
        if solved_sudoku.is_none() {
            panic!(
                "Randomly generated an invalid sudoku. Please save the sudoku for debugging:\n{}",
                sudoku.to_str_line()
            );
        }
    }
}

// this test is probabilistic in nature
// if an error occurs, note down the sudoku that it generated
#[test]
fn generate_unique_sudoku_uniqueness() {
    for _ in 0..100 {
        let sudoku = Sudoku::generate_unique();
        let solved_sudoku = sudoku.solve_unique();
        if solved_sudoku.is_none() {
            panic!(
                "Randomly generated a non-proper sudoku. Please save the sudoku for debugging:\n{}",
                sudoku.to_str_line()
            );
        }
    }
}

// this test is probabilistic in nature
// if an error occurs, note down the sudoku that it generated
#[test]
fn shuffle_unsolved() {
    let sudoku = Sudoku::generate_unique();
    test_shuffle_sudoku(sudoku);
}

// this test is probabilistic in nature
// if an error occurs, note down the sudoku that it generated
#[test]
fn shuffle_solved() {
    let sudoku = Sudoku::generate_filled();
    test_shuffle_sudoku(sudoku);
}

// test if any two sudokus are equal
// also asserts that the number of clues don't change
fn test_shuffle_sudoku(sudoku: Sudoku) {
    let mut sudokus = vec![sudoku; 1000];
    let n_clues = sudoku.n_clues();
    sudokus.iter_mut().for_each(Sudoku::shuffle);
    sudokus.sort();

    let mut duplicates = vec![];
    for (i, sudokus) in sudokus.windows(2).enumerate() {
        if let [sudoku1, sudoku2] = sudokus {
            assert_eq!(sudoku1.n_clues(), n_clues);
            if sudoku1 == sudoku2 {
                duplicates.push(i);
            }
        } else {
            unreachable!();
        }
    }

    if duplicates.len() > 0 {
        for i in duplicates {
            println!("sudoku nr {} and next: {}", i, sudokus[i].to_str_line());
        }

        panic!("\nRandomly shuffled a sudoku into the above equal sudoku(s). This is possible, but very unlikely. Please save the sudoku(s) for debugging.");
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

#[test]
fn canonicalize() {
    let mut sudoku = Sudoku::generate_filled();
    let canonical = sudoku.canonicalized().unwrap();

    for i in 0..1_000 {
        sudoku.shuffle();
        let recanonicalized = sudoku.canonicalized().unwrap();
        if recanonicalized != canonical {
            panic!("canonicalize(shuffle(canonical_grid)) != canonical_grid on attempt nr {}\nbefore {}\nafter  {}", i, canonical.to_str_line(), recanonicalized.to_str_line());
        }
    }
}

#[test]
fn canonicalize_idempotency() {
    for _ in 0..1_000 {
        let mut sudoku = Sudoku::generate_filled().canonicalized().unwrap();
        let recanonicalized = sudoku.canonicalized().unwrap();
        if sudoku != recanonicalized {
            panic!("canonicalize(canonicalize(sudoku)) != canonicalize(sudoku)\n1. {}\n2. {}", recanonicalized.to_str_line(), sudoku.to_str_line());
        }
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
