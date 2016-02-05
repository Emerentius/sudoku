use covers::Covers;
use entry::Entry;
use sudoku::Sudoku;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SudokuSolver {
	pub grid: Sudoku,
	pub covers: Covers,
}

impl SudokuSolver {
	pub fn from_sudoku(sudoku: Sudoku) -> SudokuSolver {
		SudokuSolver {
			covers: Covers::from_sudoku(&sudoku),
			grid: sudoku,
		}
	}

	#[inline]
	fn remove_impossible(&mut self, new_entry: Entry) {
		self.covers.remove_impossible(new_entry);
	}

	fn insert_entry(&mut self, entry: Entry) {
		self.grid[entry.cell()] = entry.num();
		self.remove_impossible(entry);
	}

	fn with_entry(&self, entry: Entry) -> Self {
		let mut sudoku = self.clone();
		sudoku.insert_entry(entry);
		sudoku
	}

	#[inline]
	pub fn is_solved(&self) -> bool {
		&self.covers.covered[..] == &[true; 324][..]
	}

	#[inline]
	fn is_impossible(&self) -> bool {
		Iterator::zip( self.covers.possibilities_count.iter(), self.covers.covered.iter() )
			.any(|(&poss, &covered)| !covered && poss == 0)
	}

	// return true if new entries were found
	fn insert_deduced_entries(&mut self) -> bool {
		let entries = self.covers.possibilities_count.iter()
			.enumerate()
			.filter(|&(_, &n_poss)| n_poss == 1)
			.map(|(idx, _)| self.matching_entry(idx) )//	self.covers.entries.iter().cloned().find(|e| e.constrains(idx))
			.collect::<Vec<_>>();

		let entries_added = entries.len() != 0;
		for entry in entries {
			self.insert_entry(entry);
		}
		entries_added
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
		let result = self.solve_at_most(1);
		if result.len() == 0 {
			None
		} else {
			result.into_iter().next() // just take one
		}
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
		self._solve_at_most(limit, &mut solutions);
		solutions
	}

	fn _solve_at_most(mut self, limit: usize, solutions: &mut Vec<Sudoku>) {
		if solutions.len() == limit { return }

		// deduce entries, but check in between deductions if the sudoku is still possible
		while self.insert_deduced_entries() {
			if self.is_impossible() { return }
		}

		// impossible to insert another number
		// either solved or unsolvable
		if self.covers.is_empty() && self.is_solved() {
			if self.is_solved() {
				solutions.push(self.grid);
			}
			return
		}

		let (idx, _) = self.covers.possibilities_count.iter()
			.enumerate()
			.filter(|&(_, &n_poss)| n_poss != 0)
			.min_by_key(|&(_, n_poss)| n_poss)
			.unwrap();
		for trial_sudoku in self.covers.entries.iter()
			.filter(|e| e.constrains(idx))
			.map(|&new_entry| self.with_entry(new_entry))
		{
			trial_sudoku._solve_at_most(limit, solutions);
		}
	}
}
