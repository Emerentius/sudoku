/*

Implements a brute force algorithm to solve the sudoku when the
fast_solve method fails.

The steps are the following:
1- Get all coordinates of empty fields
2- Loop through each of them
    Assign a number, check if it is valid for the sudoku
    If it is valid, assign the next field
    If it is invalid, go back to the previous field and assign the next valid number
3- If we reach the last field and we can assign it a valid number, the sudoku is completed
4- If we get back to the first field and we cannot assign it a valid number, the sudoku is not valid

*/

use super::Sudoku;

struct Point(u8, u8);

/// Attemts to brute_force the sudoku and returns true if it works
pub fn brute_force(sudoku: &mut Sudoku) -> bool {
    // Assign numbers to the empty fields recursively
    let e_fields = get_empty_fields(sudoku);
    assign_field(sudoku, e_fields.iter())
}

// Recursive function to brute force the empty fields
fn assign_field(sudoku: &mut Sudoku, mut empty_fields: ::std::slice::Iter<Point>) -> bool
{
    // If all empty fields are assigned without errors, the sudoku is completed
    let x; let y;
    match empty_fields.next() {
        Some(&Point(field_x, field_y)) => { x = field_x; y = field_y; }
        None => { return true }
    }

    // set_next_number will return false when there is no number left to be assigned
    while sudoku.get_mut(x, y).set_next_number() {
        // If the condition is not broken, assign the next field
        // If it is broken, test with the next available number

        if sudoku.is_valid(x, y) && assign_field(sudoku, empty_fields.clone()) {
            return true;
        }
    }

    false
}

fn get_empty_fields(sudoku: &mut Sudoku) -> Vec<Point> {
    let mut points = vec!();

    for x in 0..9 {
        for y in 0..9 {
            if !sudoku.get(x, y).number_found() {
                points.push(Point(x, y));
            }
        }
    }

    points
}