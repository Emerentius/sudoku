use entry::Entry;
use sudoku::Sudoku;
use std::mem;

// contains all the information to represent the sparse exact cover matrix
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Covers {
	pub entries: Vec<Entry>, // list of entries that could still be entered
    pub possibilities_count: Vec<u8>, // count of how many entries are still able to satisfy a certain constraint
	pub covered: Vec<bool>, // storage for whether a constraint has been met already
							// a possibility count of 0 would otherwise be ambiguous
							// it could mean 'impossible' or 'already set'
							// could be a BitVec, improvements are miniscule but measurable
}

impl Covers {
	pub fn new() -> Covers {
		Covers {
			entries: (0..9*9*9)
				.map(|i| Entry { cell: (i / 9), num: (i % 9) as u8 + 1 } )
				.collect::<Vec<Entry>>(),
			// idx = row/col/field/cell * 9 + (num - 1) + offset, offset = 81 for col, 162 for field, 243 for cells
			possibilities_count: vec![9; 324],
			covered: vec![false; 324],
		}
	}

	pub fn from_sudoku(sudoku: &Sudoku) -> Covers {
		let mut covers = Covers::new();
		for (cell, num) in sudoku.iter().enumerate().filter(|&(_, num)| num.is_some()) { // is_some() == "num.unwrap() != 0"
			let num = num.unwrap();
			covers.remove_impossible( Entry { cell: cell, num: num } );
		}
		covers
	}

	#[inline]
	pub fn is_empty(&self) -> bool { self.entries.is_empty() }

	pub fn remove_impossible(&mut self, new_entry: Entry) {
		self.covered[new_entry.row_constraint()] = true;
		self.covered[new_entry.col_constraint()]  = true;
		self.covered[new_entry.field_constraint()] = true;
		self.covered[new_entry.cell_constraint()] = true;
		let mut entries = mem::replace(&mut self.entries, vec![] );
		entries.retain(|old_entry| {
			if old_entry.conflicts_with(new_entry) { // remove old_entry
				self.possibilities_count[old_entry.row_constraint()] -= 1;
				self.possibilities_count[old_entry.col_constraint()] -= 1;
				self.possibilities_count[old_entry.field_constraint()] -= 1;
				self.possibilities_count[old_entry.cell_constraint()] -= 1;
				false
			} else {
				true
			}
		});
		self.entries = entries;
	}
}
