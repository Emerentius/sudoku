/*

Implements the Sudoku struct, with the functionality to solve a sudoku.

There is a fast_solve method used to solve the sudoku without brute forcing it. If
it fails, you can use the brute_force method defined in brute_force.rs

For details about how the algorithm works, take a look at project_numbers.rs,
detect_uniques.rs, project_lines.rs and brute_force.rs

*/

mod field;
mod brute_force;
mod detect_uniques;
mod parse_error;
mod project_lines;
mod project_numbers;

use self::field::Field;
use self::brute_force::BruteForce;
use self::detect_uniques::DetectUniques;
pub use self::parse_error::ParseError;
use self::project_lines::ProjectLines;
use self::project_numbers::ProjectNumbers;

use std::fmt::{self, Display};
use std::io::BufRead;
use std::iter;

// Sudoku
#[derive(Clone)]
pub struct Sudoku {
    fields: Vec<Vec<Field>>
}

impl Sudoku {
    pub fn from_str(s: &str) -> Result<Sudoku, ParseError> {
        Sudoku::from_reader(s.as_bytes())
    }

    pub fn from_reader<T: BufRead>(reader: T) -> Result<Sudoku, ParseError> {
        // Use one column of 9 fields to fill 9 rows
        let column = vec![Field::new(); 9];
        let mut rows = vec![column; 9];

        // Read a row per line
        for (y, line) in reader.lines().take(9).enumerate() {
            let line_nr = y as u8 + 1;
            let line = line.ok().unwrap_or("".to_string());
            let numbers: Vec<char> = line.trim_right().chars().collect();

            if numbers.len() != 9 {
                return Err(ParseError::InvalidLineLength(line_nr));
            }

            // Values that cannot be parsed are interpreted as empty fields
            for x in 0..9 {;
                match numbers[x].to_digit(10) {
                    Some(i) if i != 0 => rows[x][y].set_number(i as u8),
                    _ => return Err(ParseError::InvalidNumber(line_nr, numbers[x]))
                }
            }
        }

        Ok(Sudoku { fields: rows })
    }

    /// Attempts to solve the sudoku
    ///
    /// If the fast technique doesn't work, we fall back to brute forcing
    pub fn solve(&mut self) {
        self.fast_solve();

        if !self.is_solved() {
            self.brute_force();
        }
    }

    // Attempts to solve the sudoku without brute forcing it
    pub fn fast_solve(&mut self) {
        let mut progress = true;

        // If the functions cannot discover new numbers, they will return false
        while progress {
            progress = self.project_numbers()
                    || self.detect_uniques()
                    || self.project_lines();
        }
    }

    /// Attempts to solve the sudoku by brute forcing it
    ///
    /// The numbers that have already been found are keeped
    pub fn brute_force_solve(&mut self) {
        self.brute_force();
    }

    // Returns true if the sudoku is completed
    pub fn is_solved(&self) -> bool {
        self.fields.iter().all(|column| column.iter().all(|field|
            field.number_found())
        )
    }

    // Returns the top-left corner of the square in which the given point is
    pub fn get_corner(x: u8, y: u8) -> (u8, u8) {
        assert!(x < 9 && y < 9);
        ((x / 3) * 3, (y / 3) * 3)
    }

    pub fn get(&self, x: u8, y: u8) -> &Field {
        &self.fields[x as usize][y as usize]
    }

    pub fn get_mut(&mut self, x: u8, y: u8) -> &mut Field {
        &mut self.fields[x as usize][y as usize]
    }

    // Check that the number in the given coordinates does not break
    // the sudoku condition
    fn is_valid(&self, x: u8, y: u8) -> bool {
        let number = self.get(x, y).get_number();

        // Check horizontal line
        for i in 0..9 {
            if i != x
            && self.get(i, y).number_found()
            && self.get(i, y).get_number() == number {
                return false;
            }
        }

        // Check vertical line
        for i in 0..9 {
            if i != y
            && self.get(x, i).number_found()
            && self.get(x, i).get_number() == number {
                return false;
            }
        }

        // Check square
        let (corner_x, corner_y) = Sudoku::get_corner(x, y);
        for off_x in 0..3 {
            for off_y in 0..3 {
                if corner_x + off_x != x || corner_y + off_y != y {
                    if self.get(corner_x + off_x, corner_y + off_y).number_found()
                    && self.get(corner_x + off_x, corner_y + off_y).get_number() == number {
                        return false;
                    }
                }
            }
        }

        true
    }
}

impl Display for Sudoku {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for y in 0..9 {
            if y == 3 || y == 6 {
                try!(write!(f, "{}\n", iter::repeat("-").take(12).collect::<String>()));
            }
            for x in 0..9 {
                if x == 3 || x == 6 {
                    try!(write!(f, "|"));
                }

                if self.get(x, y).number_found() {
                    try!(write!(f, "{}", self.get(x, y).get_number()))
                } else {
                    try!(write!(f, " "))
                }
            }

            try!(write!(f, "\n"));
        }

        Ok(())
    }
}
