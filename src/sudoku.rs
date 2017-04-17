use consts::*;
use types::{Entry, Covers, ParseError};

use std::{fmt, slice, iter, mem};
use std::io::BufRead;

/// The main structure exposing all the functionality of the library
#[derive(Copy)]
pub struct Sudoku([u8; 81]);

impl PartialEq for Sudoku {
	fn eq(&self, other: &Sudoku) -> bool {
		&self.0[..] == &other.0[..]
	}
}

impl Eq for Sudoku {}

impl fmt::Debug for Sudoku {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
		self.0.fmt(fmt)
	}
}

impl Clone for Sudoku {
	fn clone(&self) -> Self {
		*self
	}
}

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
		let mut grid = [0; N_CELLS];

		// Read a row per line
		let mut line_count = 0;
		for (line_nr, line) in Iterator::zip(1..9+1, reader.lines().take(9)) {
			line_count += 1;
			let line = line.ok().unwrap_or("".to_string());
			let trimmed_line = line.trim_right();
			if trimmed_line.chars().filter(|&c| c!= '|').count() != 9 {
				return Err(ParseError::InvalidLineLength(line_nr));
			}

			for (col, ch) in trimmed_line.chars().filter(|&c| c != '|').enumerate() {
				match ch {
					'1'...'9' => grid[(line_nr-1) as usize *9 + col] = ch.to_digit(10).unwrap() as u8,
					'_'       => grid[(line_nr-1) as usize *9 + col] = 0,
					_         => return Err(ParseError::InvalidNumber(line_nr, ch)),
				}
			}
		}

		if line_count < 9 {
			Err(ParseError::NotEnoughRows)
		} else {
			Ok(Sudoku(grid))
		}
	}

    fn into_solver(self) -> Result<SudokuSolver, Unsolvable> {
        SudokuSolver::from_sudoku(self)
    }

	/// Try to find a solution to the sudoku and fill it in. Return true if a solution was found.
	/// This is a convenience interface. Use one of the other solver methods for better error handling
	pub fn solve(&mut self) -> bool {
		match self.clone().into_solver().map(|solver| solver.solve_one()).unwrap_or(None) {
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
        self.into_solver().map(SudokuSolver::solve_one).unwrap_or(None)
    }

    /// Solve sudoku and return solution if solution is unique.
	pub fn solve_unique(self) -> Option<Sudoku> {
		self.into_solver().map(SudokuSolver::solve_unique).unwrap_or(None)
	}

	/// Solve sudoku and return the first `limit` solutions it finds. If less solutions exist, return only those. Return `None` if no solution exists.
	/// No specific ordering of solutions is promised. It can change across versions.
    pub fn solve_at_most(self, limit: usize) -> Option<Vec<Sudoku>> {
        let results = self.into_solver().map(|solver| solver.solve_at_most(limit))
			.unwrap_or(vec![]);
		if results.len() == 0 {
			None
		} else {
			Some(results)
		}
    }

	/// Check whether the sudoku is solved.
	pub fn is_solved(&self) -> bool {
		self.clone().into_solver().map(|solver| solver.is_solved()).unwrap_or(false)
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
		for entry in self.0.iter().enumerate().map(|(cell, &num)| Entry { cell: cell as u8, num: num } ) {
			try!( match (entry.row(), entry.col()) {
				(_, 3) | (_, 6) => write!(f, " "),    // seperate fields in columns
				(3, 0) | (6, 0) => write!(f, "\n\n"), // separate fields in rows
				(_, 0)          => write!(f, "\n"),   // separate lines not between fields
				_ => Ok(()),
			});
			//try!(
            try!( match entry.num() {
                0 => write!(f, "_"),
                1...9 => write!(f, "{}", entry.num()),
                _ => unreachable!(),
            });
                //uwrite!(f, "{}", entry.num())
            //);
		}
		Ok(())
	}
}

// Solving happens by an exact cover algorithm
// There are a total of 729 (81 cells * 9 numbers) sudoku entry possibilities
//
// Every entry (cell-number-combination) satisfies 4 constraints
// 1. a row    needs to have 1 of each number (9 rows, 9 numbers each)
// 2. a column needs to have 1 of each number (9 cols, 9 numbers each)
// 3. a field  needs to have 1 of each number (9 fields, 9 numbers each)
// 4. a cell needs to be filled               (81 cells, 1 number each)
//
// For a total of 81*4 = 324 constraints
//
// The covers property in SudokuSolver contains the information what entries can
// be added at a certain point in the solving process, which constraints are
// already satisfied and how many possibilities still exist for a given constraint.
// See also the covers module.
//
// Solving happens by recursively walking the tree of possible sudokus
// If some constraint can only be satisfied by 1 entry, it will be entered immediately
// This is equivalent to finding naked singles and hidden singles
// If no entry can be deduced, a constraint with the least amount of possibilites
// is chosen and all possibilites tried out.

// Helper struct for recursive solving
#[derive(Clone, Debug)]
pub struct SudokuSolver {
	pub grid: Sudoku,
	pub covers: Covers,
}

impl SudokuSolver {
	fn new() -> SudokuSolver {
		SudokuSolver {
			grid: Sudoku([0; 81]),
			covers: Covers::new(),
		}
	}

	pub fn from_sudoku(sudoku: Sudoku) -> Result<SudokuSolver, Unsolvable> {
		let mut solver = Self::new();
		let mut stack = sudoku.iter()
			.enumerate()
			.flat_map(|(i, num)| num.map(|n| Entry { cell: i as u8, num: n }))
			.collect();
		solver.insert_entries(&mut stack)?;
		Ok(solver)
	}

	fn _insert_entry(&mut self, entry: Entry) -> Result<(), Unsolvable> {
		// duplicate entry, skip
		if self.grid.0[entry.cell()] == entry.num() {
			return Ok(())
		}
		// cell already filled with different number
		if self.covers.covered[entry.cell_constraint()]
			|| self.covers.covered[entry.row_constraint()]
			|| self.covers.covered[entry.col_constraint()]
			|| self.covers.covered[entry.field_constraint()]
		{
			return Err(Unsolvable)
		}
		self.grid.0[entry.cell()] = entry.num();
		self.covers.covered[entry.row_constraint()] = true;
		self.covers.covered[entry.col_constraint()]  = true;
		self.covers.covered[entry.field_constraint()] = true;
		self.covers.covered[entry.cell_constraint()] = true;
		Ok(())
	}

	fn decrement_possibilities_count(&mut self, impossible_entry: Entry) {
		self.covers.possibilities_count[impossible_entry.row_constraint()] -= 1;
		self.covers.possibilities_count[impossible_entry.col_constraint()] -= 1;
		self.covers.possibilities_count[impossible_entry.field_constraint()] -= 1;
		self.covers.possibilities_count[impossible_entry.cell_constraint()] -= 1;
	}

	fn insert_entry(&mut self, entry: Entry) -> Result<(), Unsolvable> {
		self._insert_entry(entry)?;

		// remove impossible entries, keep possibilities counter accurate
		let mut entries = mem::replace(&mut self.covers.entries, vec![] );
		entries.retain(|&old_entry| {
			if old_entry.conflicts_with(entry) { // remove old_entry
				self.decrement_possibilities_count(old_entry);
				false
			} else {
				true
			}
		});
		self.covers.entries = entries;
		Ok(())
	}

	fn insert_entries(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		for entry in stack.drain(..) {
			self._insert_entry(entry)?;
		}

		// remove impossible entries, keep possibilities counter accurate
		let mut entries = mem::replace(&mut self.covers.entries, vec![] );
		entries.retain(|&old_entry| {
			if self.covers.covered[old_entry.cell_constraint()]
				|| self.covers.covered[old_entry.row_constraint()]
				|| self.covers.covered[old_entry.col_constraint()]
				|| self.covers.covered[old_entry.field_constraint()]
			{
				self.decrement_possibilities_count(old_entry);
				false
			} else {
				true
			}
		});
		self.covers.entries = entries;
		Ok(())
	}

	fn with_entry(&self, entry: Entry) -> Result<Self, Unsolvable> {
		let mut sudoku = self.clone();
		sudoku.insert_entry(entry)?;
		Ok(sudoku)
	}

	#[inline]
	pub fn is_solved(&self) -> bool {
		self.covers.entries.is_empty() && &self.covers.covered[..] == &[true; 324][..]
	}

	#[inline]
	fn is_impossible(&self) -> bool {
		Iterator::zip( self.covers.possibilities_count.iter(), self.covers.covered.iter() )
			.any(|(&poss, &covered)| !covered && poss == 0)
	}

	// return true if new entries were found
	fn insert_deduced_entries(&mut self, stack: &mut Vec<Entry>) -> Result<bool, Unsolvable> {
		stack.extend(self.covers.possibilities_count.iter()
			.enumerate()
			.filter(|&(_, &n_poss)| n_poss == 1)
			.map(|(idx, _)| self.matching_entry(idx) )
		);

		let entries_added = stack.len() != 0;
		self.insert_entries(stack)?;
		Ok(entries_added)
	}

	// may fail, but only if used incorrectly
	#[inline]
	fn matching_entry(&self, constraint_nr: usize) -> Entry {
		self.covers.entries.iter()
			.cloned()
			.find(|e| e.constrains(constraint_nr))
			.unwrap()
	}

	pub fn solve_one(self) -> Option<Sudoku> {
		self.solve_at_most(1)
			.into_iter()
			.next()
	}

	pub fn solve_unique(self) -> Option<Sudoku> {
		let result = self.solve_at_most(2);
		if result.len() == 1 {
			result.into_iter().next()
		} else {
			None
		}
	}

	pub fn solve_at_most(self, limit: usize) -> Vec<Sudoku> {
		let mut solutions = vec![];
		let mut stack = Vec::with_capacity(81);
		let _ = self._solve_at_most(limit, &mut stack, &mut solutions);
		solutions
	}

	fn _solve_at_most(mut self, limit: usize, stack: &mut Vec<Entry>, solutions: &mut Vec<Sudoku>) {
		if solutions.len() == limit { return }

		loop {
			match self.insert_deduced_entries(stack) {
				Err(Unsolvable) => return,
				Ok(true) if self.is_impossible() => return,
				Ok(false) => break,
				_ => (), // deduce more
			}
		}

		if self.is_solved() {
			solutions.push(self.grid);
			return
		}

		// find cell with minimum amount of possibilities
		// this is faster (for some unknown reason) than searching rows, cols
		// and fields for minimum amount of possibilities for some number
		let (idx, _) = Iterator::zip(243.., (&self.covers.possibilities_count[243..]).iter())
			.filter(|&(_, &n_poss)| n_poss != 0)
			.min_by_key(|&(_, n_poss)| n_poss)
			.unwrap();
		let cell = idx - CELL_OFFSET;
		for trial_sudoku in self.covers.entries.iter()
			.skip_while(|e| e.cell() != cell)
			.take_while(|e| e.cell() == cell)
			.flat_map(|&new_entry| self.with_entry(new_entry))
		{
			trial_sudoku._solve_at_most(limit, stack, solutions);
		}
	}
}

pub struct Unsolvable;
