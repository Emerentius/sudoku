use consts::*;
use positions::*;
use types::{Mask, Digit, Array81, Entry, PubEntry, BlockFormatParseError, LineFormatParseError, Unsolvable, NotEnoughRows};

use std::{fmt, slice, iter};

/// The main structure exposing all the functionality of the library
/// Sudokus can be parsed in either the line format or the block format
/// 
/// line format:
///
/// `..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.. optional comment`
///
/// block format:
///
/// ```text
/// __3_2_6__ optional comment
/// 9__3_5__1 another comment
/// __18_64__
/// __81_29__
/// 7_______8
/// __67_82__
/// __26_95__
/// 8__2_3__9
/// __5_1_3__
/// ```
///
/// alternatively also with field delimiters
///
/// ```text
/// __3|_2_|6__ optional comment
/// 9__|3_5|__1 another comment
/// __1|8_6|4__
/// ---+---+--- comment: "-----------", i.e. '-' 11 times is also allowed
/// __8|1_2|9__          but has to be consistent
/// 7__|___|__8
/// __6|7_8|2__
/// ---+---+---
/// __2|6_9|5__
/// 8__|2_3|__9
/// __5|_1_|3__
/// ```
///
/// `'_'`, `'.'` and `'0'` are accepted interchangeably as unfilled cells
#[derive(Copy)]
pub struct Sudoku(pub(crate) [u8; 81]);

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
	/// Creates a sudoku from a byte slice.
	/// All numbers must be below 10. Empty cells are denoted by 0, clues by the numbers 1-9.
	/// The slice must be of length 81.
	pub fn from_bytes_slice(bytes: &[u8]) -> Result<Sudoku, ()> {
			if bytes.len() != 81 { return Err(()) }
			let mut sudoku = Sudoku([0; 81]);

			match bytes.iter().all(|&byte| byte <= 9) {
				true => {
					sudoku.0.copy_from_slice(bytes);
					Ok(sudoku)
				},	
				false => Err(())
			}
	}

	/// Creates a sudoku from a byte array.
	/// All numbers must be below 10. Empty cells are denoted by 0, clues by the numbers 1-9.
	pub fn from_bytes(bytes: [u8; 81]) -> Result<Sudoku, ()> {
			match bytes.iter().all(|&byte| byte <= 9) {
				true => Ok(Sudoku(bytes)),
				false => Err(()),
			}
	}

	fn from_chars_line<CI: Iterator<Item=char>>(mut chars: CI) -> Result<Sudoku, LineFormatParseError> {
		let mut grid = [0; N_CELLS];
		let mut i = 0;
		for (cell, ch) in grid.iter_mut().zip(chars.by_ref()) {
			match ch {
				'_' | '.' | '0' => *cell = 0,
				'1' ... '9' => *cell = ch as u8 - b'0',
				// space ends sudoku before grid is filled
				' ' | '\t' => return Err(LineFormatParseError::NotEnoughCells(i)),
				_ => return Err(LineFormatParseError::InvalidEntry(PubEntry{cell: i, ch: ch })),
			}
			i += 1;
		}

		if i != 81 {
			return Err(LineFormatParseError::NotEnoughCells(i))
		}

		// if more than 81 elements, sudoku must be delimited
		if let Some(ch) = chars.next() {
			match ch {
				// delimiters, end of sudoku
				' ' | '\r' | '\n' => (),
				// valid cell entry => too long
				'_' | '.' | '0' | '1'...'9' => {
					return Err(LineFormatParseError::TooManyCells)
				},
				// any other char can not be part of sudoku
				// without having both length and character wrong
				// treat like comment, but with missing delimiter
				_ => return Err(LineFormatParseError::MissingCommentDelimiter),
			}
		}

		// Read a single sudoku in line format
		// '_', '.' and '0' are treated as empty cells
		// an optional comment is allowed after the sudoku
		// which must be delimited by a space
		Ok(Sudoku(grid))
	}

	/// Reads a sudoku in the line format
	/// Stops parsing after the first sudoku
	pub fn from_str_line(s: &str) -> Result<Sudoku, LineFormatParseError> {
		Sudoku::from_chars_line(s.chars())
	}

	/// Reads a sudoku in the block format with or without field delimiters
	/// Stops parsing after the first sudoku
	pub fn from_str_block(s: &str) -> Result<Sudoku, BlockFormatParseError> {
		let mut grid = [0; N_CELLS];
		#[derive(PartialEq)]
		enum Format {
			Unknown,
			Delimited,
			DelimitedPlus,
			Bare,
		}
		let mut format = Format::Unknown;

		// Read a row per line
		let mut n_line_sud = 0;
		for (n_line_str, line) in s.lines().enumerate() {
			// if sudoku complete
			// enforce empty line (whitespace ignored)
			// Maybe allow comment lines in the future
			if n_line_sud == 9 {
				match line.trim().is_empty() {
					true => break,
					false => return Err(BlockFormatParseError::TooManyRows),
				}
			}

			// if delimited, check horizontal field delimiters and skip over line
			if (format == Format::Delimited || format == Format::DelimitedPlus)
			&& (n_line_str == 3 || n_line_str == 7)
			{
				if n_line_str == 3 && (line.starts_with("---+---+---") || line.starts_with("---+---+--- ")) {
					format = Format::DelimitedPlus;
				}
				if format == Format::Delimited {
					match !(line.starts_with("-----------") || line.starts_with("----------- ")) {
						true  => return Err(BlockFormatParseError::IncorrectFieldDelimiter),
						false => continue,
					}
				}
				if format == Format::DelimitedPlus {
					match !(line.starts_with("---+---+---") || line.starts_with("---+---+--- ")) {
						true  => return Err(BlockFormatParseError::IncorrectFieldDelimiter),
						false => continue,
					}
				}
			}

			let mut n_col_sud = 0;
			for (str_col, ch) in line.chars().enumerate() {
				// if line complete
				if n_col_sud == 9 {
					match ch {
						// comment separator
						' ' | '\t' => break,
						// valid entry, line too long
						'1'...'9' | '_' | '.' | '0'   => return Err(BlockFormatParseError::InvalidLineLength(n_line_sud)),
						// invalid entry, interpret as comment but enforce separation
						_ => return Err(BlockFormatParseError::MissingCommentDelimiter(n_line_sud))
					}
				}

				// if in place of vertical field delimiters
				if str_col == 3 || str_col == 7 {
					// Set parse mode on 4th char in 1st line
					if format == Format::Unknown {
						format = if ch == '|' { Format::Delimited } else { Format::Bare };
					}
					// check and skip over delimiters
					if format == Format::Delimited || format == Format::DelimitedPlus {
						match ch {
							'|'  => continue,
							_    => return Err(BlockFormatParseError::IncorrectFieldDelimiter),
						}
					}
				}

				let cell = n_line_sud * 9 + n_col_sud;
				match ch {
					'1'...'9'       => grid[cell as usize] = ch as u8 - b'0',
					'_' | '.' | '0' => grid[cell as usize] = 0,
					_ => return Err(BlockFormatParseError::InvalidEntry(PubEntry{cell: cell as u8, ch })),
				}
				n_col_sud += 1;
			}
			if n_col_sud != 9 {
				return Err(BlockFormatParseError::InvalidLineLength(n_line_sud))
			}

			n_line_sud += 1;
		}
		if n_line_sud != 9 {
			return Err(BlockFormatParseError::NotEnoughRows(n_line_sud+1)) // number of rows = index of last + 1
		}
		Ok(Sudoku(grid))
	}

	/// Reads a sudoku in a variety of block formats, applying few constraints.
	/// '_', '.' and '0' are treated as empty cells. '1' to '9' as clues. Each line needs to have 9 valid cells.
	/// Lines that don't contain 9 valid entries are ignored.
	/// Stops parsing after the first sudoku. Due to the lax format rules, the only failure that can occur
	/// is that there are not enough rows.
	//pub fn from_str_block_permissive<CP>(s: &str, mut matches_empty_cell: CP) -> Result<Sudoku, BlockFormatParseError>
	//	where CP: CharPattern,
	pub fn from_str_block_permissive(s: &str) -> Result<Sudoku, NotEnoughRows>
	{
		let mut grid = [0; N_CELLS];

		let mut valid_rows = 0;
		for line in s.lines() {
			let mut row_vals = [0; 9];
			let mut nums_in_row = 0;
			for ch in line.chars() {
				if ['.', '_', '0'].contains(&ch) {
					row_vals[nums_in_row] = 0;
					nums_in_row += 1;
				} else if '1' <= ch && ch <= '9' {
					row_vals[nums_in_row] = ch as u8 - b'0';
					nums_in_row += 1;
				}
				// full sudoko row, write to grid
				// ignore anything after in same row
				if nums_in_row == 9 {
					grid[valid_rows*9..valid_rows*9 + 9].copy_from_slice(&row_vals);
					valid_rows += 1;
					break
				}
			}
			if valid_rows == 9 {
				return Ok(Sudoku(grid))
			}
		}
		Err(NotEnoughRows(valid_rows as u8))
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
		let mut solver = SudokuSolver::new();
		let mut entries = self.iter()
			.enumerate()
			.flat_map(|(i, num)| num.map(|n| Entry { cell: i as u8, num: n }))
			.collect();
		// if sudoku contains an error, batch_insert_entries returns Err(Unsolvable) and
		// will not insert all 81 entries. Consequently solver.is_solved() will
		// return false
		let _ = solver.batch_insert_entries::<InsertGivenOnly>(&mut entries);
		solver.is_solved()
	}

    /// Returns an Iterator over sudoku, going from left to right, top to bottom
    pub fn iter(&self) -> Iter {
        self.0.iter().map(num_to_opt)
    }

	/// Returns a byte array for the sudoku.
	/// Empty cells are denoted by 0, clues by the numbers 1-9.
	pub fn into_bytes(self) -> [u8; 81] {
		self.0
	}

	/// Returns a representation of the sudoku in line format that can be printed
	/// and which derefs into a &str
	/// 
	/// ```
	/// use sudoku::Sudoku;
	///
	/// let mut grid = [0; 81];
	/// grid[3] = 5;
	/// let sudoku = Sudoku::from_bytes(grid).unwrap();
	/// let line = sudoku.to_str_line(); // :SudokuLine
	/// println!("{}", line);
	///
	/// let line_str: &str = &line;
	/// assert_eq!(
	///		"...5.............................................................................",
	///     line_str
	///	);
	/// ```
	pub fn to_str_line(&self) -> SudokuLine {
		let mut chars = [0; 81];
		for (char_, entry) in chars.iter_mut().zip(self.iter()) {
			*char_ = match entry {
				Some(num) => num + b'0',
				None => b'.',
			};
		}
		SudokuLine(chars)
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

// Helper struct for recursive solving
#[derive(Clone, Debug)]
pub(crate) struct SudokuSolver {
	pub grid: Sudoku,
	pub n_solved_cells: u8,
	pub cell_poss_digits: Array81<Mask<Digit>>,
	pub zone_solved_digits: [Mask<Digit>; 27],
	pub last_cell: u8, // last cell checked in guess routine
}

impl SudokuSolver {
	fn new() -> SudokuSolver {
		SudokuSolver {
			grid: Sudoku([0; 81]),
			n_solved_cells: 0,
			cell_poss_digits: Array81([Mask::ALL; 81]),
			zone_solved_digits: [Mask::NONE; 27],
			last_cell: 0,
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

	fn _insert_entry(&mut self, entry: Entry) {
		self.n_solved_cells += 1;
		self.grid.0[entry.cell()] = entry.num;
		self.cell_poss_digits[entry.cell()] = Mask::NONE;
		self.zone_solved_digits[entry.row() as usize +ROW_OFFSET] |= entry.mask();
		self.zone_solved_digits[entry.col() as usize +COL_OFFSET] |= entry.mask();
		self.zone_solved_digits[entry.field() as usize +FIELD_OFFSET] |= entry.mask();
	}

	fn insert_entries(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		match stack.len() {
			0...4 => self.insert_entries_singly(stack),
			_ => self.batch_insert_entries::<InsertDeducedEntries>(stack),
		}
	}

	// for each entry in the stack, insert it (if cell is unsolved)
	// and then remove possibility from each cell neighbouring it in all
	// zones (rows, cols, fields) eagerly
	// check for naked singles and impossible cells during this check
	fn insert_entries_singly(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		while let Some(entry) = stack.pop() {
			let entry_mask = entry.mask();
			// cell already solved from previous entry in stack, skip
			if self.cell_poss_digits[entry.cell()].is_empty() { continue }

			// is entry still possible?
			if (self.cell_poss_digits[entry.cell()] & entry_mask).is_empty() {
				return Err(Unsolvable);
			}

			self._insert_entry(entry);
			for &cell in neighbours(entry.cell) {
				if (entry_mask & self.cell_poss_digits[cell as usize]).is_empty() {
					continue
				};
				self.remove_impossibilities(cell, entry_mask, stack)?;
			}

			// found a lot of naked singles, switch to batch insertion
			if stack.len() > 4 { return self.batch_insert_entries::<InsertDeducedEntries>(stack) }
		}
		Ok(())
	}

	fn batch_insert_entries<S: InsertionStrategy>(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		for entry in stack.drain(..) {
			// cell already solved from previous entry in stack, skip
			if self.cell_poss_digits[entry.cell()].is_empty() { continue }

			let entry_mask = entry.mask();

			// is entry still possible?
			// have to check zone possibilities, because cell possibility
			// is temporarily out of date
			if self.zone_solved_digits[entry.row() as usize + ROW_OFFSET] & entry_mask != Mask::NONE
			|| self.zone_solved_digits[entry.col() as usize + COL_OFFSET] & entry_mask != Mask::NONE
			|| self.zone_solved_digits[entry.field() as usize +FIELD_OFFSET] & entry_mask != Mask::NONE
			{
				return Err(Unsolvable);
			}

			self._insert_entry(entry);
		}

		// update cell possibilities from zone masks
		for cell in 0..81 {
			if self.cell_poss_digits[cell as usize].is_empty() { continue }
			let zones_mask = self.zone_solved_digits[row_zone(cell)]
				| self.zone_solved_digits[col_zone(cell)]
				| self.zone_solved_digits[field_zone(cell)];

			self.remove_impossibilities(cell, zones_mask, stack)?;
		}
		S::execute_after_insertion(self, stack)
	}

	#[inline]
	pub fn is_solved(&self) -> bool {
		self.n_solved_cells == 81
	}

	fn find_hidden_singles(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		for zone in 0..27 {
			let mut unsolved = Mask::NONE;
			let mut multiple_unsolved = Mask::NONE;

			let cells = cells_of_zone(zone);
			for &cell in cells {
				let poss_digits = self.cell_poss_digits[cell as usize];
				multiple_unsolved |= unsolved & poss_digits;
				unsolved |= poss_digits;
			}
			if unsolved | self.zone_solved_digits[zone as usize] != Mask::ALL {
				return Err(Unsolvable);
			}

			let mut singles = unsolved.without(multiple_unsolved);
			if singles.is_empty() { continue }

			for &cell in cells {
				let mask = self.cell_poss_digits[cell as usize];
				
				if let Ok(maybe_unique) = (mask & singles).unique_num() {
					let num = maybe_unique.ok_or(Unsolvable)?;
					stack.push(Entry{ cell: cell, num: num } );

					// mark num as found
					singles.remove(Mask::from_num(num));
					
					// everything in this zone found
					// return to insert numbers immediately
					if singles.is_empty() { return Ok(()) }
				}
			}
			// can not occur but the optimizer appreciates the info
			break
		}
		Ok(())
	}

	fn find_good_guess(&mut self) -> Entry {
		let mut min_possibilities = 10;
		let mut best_cell = 100;

		{
			let mut cell = (self.last_cell + 1) % 81;
			loop {
				let cell_mask = self.cell_poss_digits[cell as usize];
				let n_possibilities = cell_mask.n_possibilities();
				// 0 means cell was already processed or its impossible in which case,
				// it should have been caught elsewhere
				// 1 shouldn't happen for the same reason, should have been processed
				if n_possibilities > 0 && n_possibilities < min_possibilities {
					best_cell = cell;
					min_possibilities = n_possibilities;
					if n_possibilities == 2 { break }
				}
				if cell == self.last_cell { break }
				cell = if cell == 80 { 0 } else { cell + 1 }
			}
			self.last_cell = cell;
		}

		let num = self.cell_poss_digits[best_cell as usize].one_possibility();
		Entry{ num: num, cell: best_cell }
	}

	// remove impossible digits from masks for given cell
	// also check for naked singles and impossibility of sudoku
	fn remove_impossibilities(&mut self, cell: u8, impossible: Mask<Digit>, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		let cell_mask = &mut self.cell_poss_digits[cell as usize];
		cell_mask.remove(impossible);
		if let Some(num) = cell_mask.unique_num()? {
			stack.push(Entry{ cell: cell, num: num });
		}
		Ok(())
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

	fn _solve_at_most(mut self, limit: usize, stack: &mut Vec<Entry>, solutions: &mut Vec<Sudoku>) -> Result<(), Unsolvable> {
		self.insert_entries(stack)?;
		if self.is_solved() {
			solutions.push(self.grid.clone());
			return Ok(())
		}

		self.find_hidden_singles(stack)?;
		if !stack.is_empty() {
			return self._solve_at_most(limit, stack, solutions);
		}

		let entry = self.find_good_guess();
		stack.push(entry);
		let _ = self.clone()._solve_at_most(limit, stack, solutions);
		stack.clear();
		if solutions.len() == limit { return Ok(()) }

		self.remove_impossibilities(entry.cell, entry.mask(), stack)?;
		self._solve_at_most(limit, stack, solutions)
	}
}

trait InsertionStrategy {
	fn execute_after_insertion(solver: &mut SudokuSolver, stack: &mut Vec<Entry>) -> Result<(), Unsolvable>;
}

struct InsertDeducedEntries;
struct InsertGivenOnly;

impl InsertionStrategy for InsertDeducedEntries {
	#[inline(always)]
	fn execute_after_insertion(solver: &mut SudokuSolver, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		match stack.is_empty() {
			true => Ok(()),
			false => solver.insert_entries(stack),
		}
	}
}

impl InsertionStrategy for InsertGivenOnly {
	#[inline(always)]
	fn execute_after_insertion(_: &mut SudokuSolver, _: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		Ok(())
	}
}

/// Container for the &str representation of a sudoku
// MUST ALWAYS contain valid utf8
#[derive(Copy)]
pub struct SudokuLine([u8; 81]);

impl PartialEq for SudokuLine {
	fn eq(&self, other: &SudokuLine) -> bool {
		&self.0[..] == &other.0[..]
	}
}

impl Eq for SudokuLine {}

impl fmt::Debug for SudokuLine {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
		self.0.fmt(fmt)
	}
}

impl Clone for SudokuLine {
	fn clone(&self) -> Self {
		*self
	}
}

impl ::core::ops::Deref for SudokuLine {
	type Target = str;
	fn deref(&self) -> &Self::Target {
		unsafe { ::core::str::from_utf8_unchecked(&self.0) }
	}
}

use ::core::ops::Deref;
impl ::core::fmt::Display for SudokuLine {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.deref())
	}
}