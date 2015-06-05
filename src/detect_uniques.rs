/*

Implements methods to detect numbers which can only be in one field
If such a number is found, it will be assigned to the field and then projected as described in project_numbers

Example:

---|---|12-
6--|---|---
---|6--|---

will become

---|---|126
6--|---|---
---|6--|---

The coordinate (9, 1) is allowed to have all numbers from 3 to 9,
but it is the only field in which the number 6 can be.

*/

use super::Sudoku;
use super::project_numbers::project_number;
use std::collections::HashSet;

pub fn detect_uniques(sudoku: &mut Sudoku) -> bool {
    let mut progress = false;

    for x in 0..9 {
        for y in 0..9 {
            // Discard the field if we have already found a number for it
            if sudoku.get(x, y).number_found() {
                continue;
            }

            // Not pretty, but otherwise I couldn't get through the compiler
            // In the future the borrow checker will be able to handle this special case
            let other_numbers_v = other_numbers_v(sudoku, x, y);
            let other_numbers_h = other_numbers_h(sudoku, x, y);
            let other_numbers_square = other_numbers_square(sudoku, x, y);

            let possible_numbers = sudoku.get(x, y).possible_numbers.clone();

            progress = check_and_assign(sudoku, x, y, &possible_numbers, &other_numbers_v)
                || check_and_assign(sudoku, x, y, &possible_numbers, &other_numbers_h)
                || check_and_assign(sudoku, x, y, &possible_numbers, &other_numbers_square)
                || progress;
        }
    }

    progress
}

// Check if the set difference between the possible_numbers of the current field
// and the other_numbers leaves a single value
// If that is the case assign it to the field in the given coordinates and project it
fn check_and_assign(sudoku: &mut Sudoku, x: u8, y: u8, possible_numbers: &HashSet<u8>, other_numbers: &HashSet<u8>) -> bool {
    let mut difference = possible_numbers.difference(other_numbers);
    match (difference.next(), difference.next()) {
        (Some(&a), None) => {
            sudoku.get_mut(x, y).set_number(a);
            project_number(sudoku, x, y);
            true
        }
        _ => { false }
    }
}

// Get a set with the possible numbers of all fields in the vertical line,
// discarding the number located in the given coordinates
fn other_numbers_v(sudoku: &mut Sudoku, x: u8, y: u8) -> HashSet<u8> {
    let mut other_numbers = HashSet::new();
    for off_y in 0..9 {
        if off_y != y {
            other_numbers.extend(sudoku.get(x, off_y).possible_numbers.iter().map(|&n| n))
        }
    }

    other_numbers
}

// Get a set with the possible numbers of all fields in the horizontal line,
// discarding the number located in the given coordinates
fn other_numbers_h(sudoku: &mut Sudoku, x: u8, y: u8) -> HashSet<u8> {
    let mut other_numbers = HashSet::new();
    for off_x in 0..9 {
        if off_x != x {
            other_numbers.extend(sudoku.get(off_x, y).possible_numbers.iter().map(|&n| n));
        }
    }

    other_numbers
}

// Get a set with the possible numbers of all fields in the square,
// discarding the number located in the given coordinates
fn other_numbers_square(sudoku: &mut Sudoku, x: u8, y: u8) -> HashSet<u8> {
    let mut other_numbers = HashSet::new();
    let (corner_x, corner_y) = Sudoku::get_corner(x, y);
    for off_x in 0..3 {
        for off_y in 0..3 {
            // Push only the values of the other fields
            if corner_x + off_x != x || corner_y + off_y != y {
                other_numbers.extend(sudoku.get(corner_x + off_x, corner_y + off_y).possible_numbers.iter().map(|&n| n));
            }
        }
    }

    other_numbers
}
