/*

Implements methods to detect lines of empty fields that are guaranteed to contain
a certain number. If such a line is found, the number will be projected.

Example:

---|123|45-
6--|---|---
---|---|---

will become

---|123|456
6--|---|---
---|---|---
    ^^^
    In one of those there must be a 6

*/

use super::Sudoku;
use std::collections::HashSet;

// Checks each square to see if it contains any lines that can be projected
pub fn project_lines(sudoku: &mut Sudoku) -> bool {
    let mut progress = false;

    for x in 0..3 {
        for y in 0..3 {
            progress = check_square(sudoku, x * 3, y * 3) || progress;
        }
    }

    progress
}

// Check a single square to see if it contains any lines that can be projected
// If such lines are found, project them
fn check_square(sudoku: &mut Sudoku, corner_x: u8, corner_y: u8) -> bool {
    let mut progress = false;

    // Horizontal lines
    for y in 0..3 {
        let diff = get_h_difference(sudoku, corner_x, y);
        for &num in diff.iter() {
            progress = project_h_line(sudoku, corner_x, y, num) || progress;
        }
    }

    // Vertical lines
    for x in 0..3 {
        let diff = get_v_difference(sudoku, x, corner_y);
        for &num in diff.iter() {
            progress = project_v_line(sudoku, x, corner_y, num) || progress;
        }
    }

    progress
}

// Get the set of possible numbers in the given horizontal line, within the square
// and take the difference with the rest of the square
fn get_h_difference(sudoku: &mut Sudoku, corner_x: u8, y: u8) -> Vec<u8> {
    // Set of possible numbers in given line
    let mut possible_numbers = HashSet::<u8>::new();
    for i in 0..3 {
        possible_numbers.extend(sudoku.get(corner_x + i, y).possible_numbers.iter().map(|&n| n));
    }

    // Set of possible numbers in the rest of the square
    let (_, corner_y) = Sudoku::get_corner(corner_x, y);
    let mut other_numbers = HashSet::<u8>::new();
    for off_y in 0..3 {
        // Discard numbers in the row Y
        if corner_y + off_y != y {
            for off_x in 0..3 {
                other_numbers.extend(sudoku.get(corner_x + off_x, corner_y + off_y).possible_numbers.iter().map(|&n| n));
            }
        }
    }

    possible_numbers.difference(&other_numbers).map(|&x| x).collect()
}

fn get_v_difference(sudoku: &mut Sudoku, x: u8, corner_y: u8) -> Vec<u8> {
    // Set of possible numbers in given line
    let mut possible_numbers = HashSet::new();
    for i in 0..3 {
        possible_numbers.extend(sudoku.get(x, corner_y + i).possible_numbers.iter().map(|&n| n));
    }

    // Set of possible numbers in the rest of the square
    let (corner_x, _) = Sudoku::get_corner(x, corner_y);
    let mut other_numbers = HashSet::new();
    for off_x in 0..3 {
        // Discard numbers in the column X
        if corner_x + off_x != x {
            for off_y in 0..3 {
                other_numbers.extend(sudoku.get(corner_x + off_x, corner_y + off_y).possible_numbers.iter().map(|&n| n));
            }
        }
    }

    // Difference
    possible_numbers.difference(&other_numbers).map(|&x| x).collect()
}

// Project a number horizontally to other squares
fn project_h_line(sudoku: &mut Sudoku, corner_x: u8, y: u8, projected_number: u8) -> bool {
    let mut progress = false;

    for x in 0..9 {
        // Do not project to same squre
        if x < corner_x || corner_x + 3 <= x {
            progress = sudoku.get_mut(x, y).cannot_be(projected_number) || progress;
        }
    }

    progress
}

// Project a number vertically to other squares
fn project_v_line(sudoku: &mut Sudoku, x: u8, corner_y: u8, projected_number: u8) -> bool {
    let mut progress = false;

    for y in 0..9 {
        // Do not project to same square
        if y < corner_y || corner_y + 3 <= y {
            progress = sudoku.get_mut(x, y).cannot_be(projected_number) || progress;
        }
    }

    progress
}