/*

Implements methods to project each field
Projection means that we tell other fields which numbers they cannot contain
This will sometimes leave just one possibility, which becomes the number assigned to the field

Example:

123|456|78-

will become

123|456|789

If we project the numbers, we tell the blank field that it cannot be any number from 1 to 8
That means that it can only be 9

The same happens for squares:

123|
456|
78-|

will become

123|
456|
789|

*/

use super::Sudoku;

// Projects all fields that are not empty and haven't yet been projected
pub fn project_numbers(sudoku: &mut Sudoku) -> bool {
    let mut progress = false;

    for x in 0..9 {
        for y in 0..9 {
            if !sudoku.get(x, y).projected && sudoku.get(x, y).number_found() {
                progress = project_number(sudoku, x, y) || progress;
            }
        }
    }

    progress
}

// Will return true if we make progress so we can know if we are stuck
pub fn project_number(sudoku: &mut Sudoku, x: u8, y: u8) -> bool {
    sudoku.get_mut(x, y).projected = true;
    project_h(sudoku, x, y) | project_v(sudoku, x, y) | project_square(sudoku, x, y)
}

// Project the number in its horizontal line
fn project_h(sudoku: &mut Sudoku, x: u8, y: u8) -> bool {
    let num = sudoku.get(x, y).get_number();
    let mut progress = false;
    for i in 0..9 {
        progress = sudoku.get_mut(i, y).cannot_be(num) || progress;
    }

    progress
}

// Project the number in its vertical line
fn project_v(sudoku: &mut Sudoku, x: u8, y: u8) -> bool {
    let num = sudoku.get(x, y).get_number();
    let mut progress = false;
    for i in 0..9 {
        progress = sudoku.get_mut(x, i).cannot_be(num) || progress;
    }

    progress
}

// Project the number in its square
fn project_square(sudoku: &mut Sudoku, x: u8, y: u8) -> bool {
    let num = sudoku.get(x, y).get_number();
    let mut progress = false;

    let (corner_x, corner_y) = Sudoku::get_corner(x, y);
    for i in (corner_x..corner_x + 3) {
        for j in (corner_y..corner_y + 3) {
            progress = sudoku.get_mut(i, j).cannot_be(num) || progress;
        }
    }

    progress
}