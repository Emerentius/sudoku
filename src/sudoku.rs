use consts::*;
use entry::Entry;
use parse_error::ParseError;
use solver::SudokuSolver;

use std::{fmt, ops, slice, iter};
use std::io::BufRead;

#[derive(PartialEq, Eq, Debug, Clone)]
/// The main structure exposing all the functionality of the library
pub struct Sudoku(Vec<u8>);

pub type Iter<'a> = iter::Map<slice::Iter<'a, u8>, fn(&u8)->Option<u8>>; // Iter over Sudoku cells

impl Sudoku {
	/// Creates a new sudoku based on a `&str`. See the crate documentation
	/// for an example of the expected format
	pub fn from_str(s: &str) -> Result<Sudoku, ParseError> {
		Sudoku::from_reader(s.as_bytes())
	}

	/// Creates a new sudoku based on a reader. See the crate documentation
	/// for an example of the expected format
	pub fn from_reader<T: BufRead>(reader: T) -> Result<Sudoku, ParseError> {
		let mut grid = Vec::with_capacity(N_SUDOKU_CELLS);

		// Read a row per line
		for (line_nr, line) in Iterator::zip(1..9+1, reader.lines().take(9)) {
			let line = line.ok().unwrap_or("".to_string());
			let trimmed_line = line.trim_right();
			if trimmed_line.chars().filter(|&c| c!= '|').count() != 9 {
				return Err(ParseError::InvalidLineLength(line_nr));
			}

			for ch in trimmed_line.chars().filter(|&c| c != '|') {
				match ch {
					'1'...'9' => grid.push( ch.to_digit(10).unwrap() as u8 ),
					'_'       => grid.push(0),
					_         => return Err(ParseError::InvalidNumber(line_nr, ch)),
				}
			}
		}

		if grid.len() < N_SUDOKU_CELLS {
			Err(ParseError::NotEnoughRows)
		} else {
			Ok(Sudoku(grid))
		}
	}

    fn into_solver(self) -> SudokuSolver {
        SudokuSolver::from_sudoku(self)
    }

	/// Try to find a solution to the sudoku and fill it in. Return true if a solution was found.
	/// This is a convenience interface. Use one of the other solver methods for better error handling
	pub fn solve(&mut self) -> bool {
		match self.clone().into_solver().solve_one() {
			Some(solution) => {
				*self = solution;
				true
			},
			None => false,
		}
	}

	/// Find a solution to the sudoku. If multiple solutions exist, it will not find them and just stop at the first.
	/// Return `None` if no solution exists.
    pub fn solve_one(self) -> Option<Sudoku> {
        self.into_solver().solve_one()
    }

    /// Solve sudoku and return solution if solution is unique.
	pub fn solve_unique(self) -> Option<Sudoku> {
		self.into_solver().solve_unique()
	}

	/// Solve sudoku and return the first `limit` solutions it finds. If less solutions exist, return only those. Return `None` if no solution exists.
	/// No specific ordering of solutions is promised. It can change across versions.
    pub fn solve_at_most(self, limit: usize) -> Option<Vec<Sudoku>> {
        let results = self.into_solver().solve_at_most(limit);
		if results.len() == 0 {
			None
		} else {
			Some(results)
		}
    }

	/// Check whether the sudoku is solved.
	pub fn is_solved(&self) -> bool {
		self.clone().into_solver().is_solved()
	}

    /// Returns an Iterator over sudoku, going from left to right, top to bottom
    pub fn iter(&self) -> Iter {
        self.0.iter().map(num_to_opt)
    }
}

fn num_to_opt(num: &u8) -> Option<u8> {
	if *num == 0 { None } else { Some(*num) }
}

impl fmt::Display for Sudoku {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		for entry in self.iter().enumerate().map(|(cell, num)| Entry { cell: cell, num: num.unwrap()} ) {
			try!( match (entry.row(), entry.col()) {
				(_, 3) | (_, 6) => write!(f, " "),    // seperate fields in columns
				(3, 0) | (6, 0) => write!(f, "\n\n"), // separate fields in rows
				(_, 0)          => write!(f, "\n"),   // separate lines not between fields
				_ => Ok(()),
			});
			try!(write!(f, "{}", entry.num()));
		}
		Ok(())
	}
}

// Indexing by cell number, numbering starts at 0
impl ops::Index<usize> for Sudoku {
	type Output = u8;

	fn index(&self, idx: usize) -> &Self::Output {
		self.0.index(idx)
	}
}

impl ops::IndexMut<usize> for Sudoku {
	fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
		self.0.index_mut(idx)
	}
}

// row/column-indexing
//impl ops::Index<(usize, usize)> for Sudoku {
//    type Output = u8;
//
//	fn index(&self, (row, col): (usize, usize)) -> &Self::Output {
//		self.0.index(row*9+col)
//	}
//}

//impl ops::IndexMut<(usize, usize)> for Sudoku {
//	fn index_mut(&mut self, (row, col): (usize, usize)) -> &mut Self::Output {
//		self.0.index_mut(row*9+col)
//	}
//}
