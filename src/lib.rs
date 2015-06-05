#![warn(missing_docs)]

//! The Sudoku library
//!
//! ## Overview
//!
//! Sudoku is a library that aims to provide a simple API to solve sudokus
//! without having to deal with too much details.
//!
//! ## Example
//!
//! ```no_run
//! use sudoku::Sudoku;
//!
//! let sudoku_str =
//! "___2___63
//! 3____54_1
//! __1__398_
//! _______9_
//! ___538___
//! _3_______
//! _263__5__
//! 5_37____8
//! 47___1___";
//!
//! let mut sudoku = Sudoku::from_str(sudoku_str).unwrap();
//! sudoku.solve();
//! println!("{}", sudoku);
//! ```

mod field;
mod brute_force;
mod detect_uniques;
mod parse_error;
mod project_lines;
mod project_numbers;

use self::field::Field;
use brute_force::brute_force;
use detect_uniques::detect_uniques;
pub use self::parse_error::ParseError;
use project_lines::project_lines;
use project_numbers::project_numbers;

use std::fmt::{self, Display};
use std::io::BufRead;
use std::iter;

/// The main structure exposing all the functionality of the library
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Sudoku {
    fields: Vec<Vec<Field>>
}

impl Sudoku {
    /// Creates a new sudoku based on a `&str`. See the crate documentation
    /// for an example of the expected format
    pub fn from_str(s: &str) -> Result<Sudoku, ParseError> {
        Sudoku::from_reader(s.as_bytes())
    }

    /// Creates a new sudoku based on a reader. See the crate documentation
    /// for an example of the expected format
    pub fn from_reader<T: BufRead>(reader: T) -> Result<Sudoku, ParseError> {
        // Use one column of 9 fields to fill 9 rows
        let column = vec![Field::new(); 9];
        let mut rows = vec![column; 9];

        // Read a row per line
        let mut line_nr = 0;
        for line in reader.lines().take(9) {
            line_nr += 1;
            let line = line.ok().unwrap_or("".to_string());
            let numbers: Vec<char> = line.trim_right().chars().collect();

            if numbers.len() != 9 {
                return Err(ParseError::InvalidLineLength(line_nr));
            }

            // Values that cannot be parsed are interpreted as empty fields
            for x in 0..9 {;
                match numbers[x].to_digit(10) {
                    Some(i) if i != 0 => rows[x][line_nr as usize - 1].set_number(i as u8),
                    None if numbers[x] == '_' => (),
                    _ => return Err(ParseError::InvalidNumber(line_nr, numbers[x]))
                }
            }
        }

        if line_nr < 9 {
            Err(ParseError::NotEnoughRows)
        } else {
            Ok(Sudoku { fields: rows })
        }
    }

    /// Attempts to solve the sudoku
    ///
    /// The `fast_solve` method is attempted first, and `brute_force_solve`
    /// is used as a fallback in case the sudoku is still unsolved
    pub fn solve(&mut self) {
        self.fast_solve();

        if !self.is_solved() {
            self.brute_force_solve();
        }
    }

    /// Attempts to solve the sudoku without brute forcing it
    ///
    /// This doesn't always succeed. You can use the `is_solved` method
    /// to check if the sudoku was successfuly solved
    pub fn fast_solve(&mut self) {
        let mut progress = true;

        // If the functions cannot discover new numbers, they will return false
        while progress {
            progress = project_numbers(self)
                    || detect_uniques(self)
                    || project_lines(self);
        }
    }

    /// Attempts to solve the sudoku by brute forcing it
    ///
    /// The numbers that have already been found are not changed
    pub fn brute_force_solve(&mut self) {
        brute_force(self);
    }

    /// Returns true if the sudoku is solved
    pub fn is_solved(&self) -> bool {
        self.fields.iter().all(|column| column.iter().all(|field|
            field.number_found())
        )
    }

    // Returns the top-left corner of the square in which the given point is
    fn get_corner(x: u8, y: u8) -> (u8, u8) {
        assert!(x < 9 && y < 9);
        ((x / 3) * 3, (y / 3) * 3)
    }

    fn get(&self, x: u8, y: u8) -> &Field {
        &self.fields[x as usize][y as usize]
    }

    fn get_mut(&mut self, x: u8, y: u8) -> &mut Field {
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
    
    let mut sudoku = Sudoku::from_str(sudoku_str).unwrap();
}
