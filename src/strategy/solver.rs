#![allow(unused)]
#![warn(unused_variables, unused_mut, unused_must_use)]
#![allow(missing_docs)]
use sudoku::Sudoku;
use types::{Array81, Unsolvable, Entry};
use consts::*;
/*
use positions::{
    row_zone, col_zone, field_zone, cells_of_zone, neighbours,
    Cell, Line, Zone, Slice, Band,
};
*/
use positions2::*;
//use positions2::set_element::SetElement;
use super::{CellState};

type DeductionRange = ::std::ops::Range<usize>;

/// The `StrategySolver` is the struct for solving sudokus with
/// strategies that humans commonly apply.
///
/// It is built from a single `Sudoku` for which it stores the current
/// state and the history of applied strategies. It can find hints
/// or solve the `Sudoku` completely and return the solution path.
/// From the solving path, the difficulty can be graded.

// To allow for the above functionality, this struct contains caches
// of various properties of the sudoku grid. The caches are lazily updated
// on demand. This avoids both unnecessary and repetitive work.
//
// Two histories are kept:
// 1. A list of all strategies that were successfully used to deduce or eliminate entries
// 2. Two lists for all entered and all eliminated digit-cell-entries
//    The former also includes initial clues.
//
// The 1st list is for reporting the sequence of strategies applied
// The 2nd list is for the updating of internal caches.
// It is kept simple to offer an easy interface and can contain duplicates.
//
// These two histories can contain overlapping information and the
// 1st one can also contain references to the 2nd but not vice versa.
#[derive(Debug, Clone)]
pub struct StrategySolver {
	pub(crate) deductions: Vec<_Deduction>,
	pub(crate) deduced_entries: Vec<Entry>,
	pub(crate) eliminated_entries: Vec<Entry>,
	pub(crate) n_solved: u8, // deduced_entries can contain duplicates so a separate counter is necessary
	// current state of the sudoku
	// for when it's faster to recompute from the end state
	// than update through the new entries
	pub(crate) grid: State<Sudoku>,
	// TODO: combine states that are updated together
	// Mask of possible numbers in cell
	pub(crate) cell_poss_digits: State<Array81<Set<Digit>>>,
	// Mask of solved digits in house
	pub(crate) house_solved_digits: State<[Set<Digit>; 27]>,
	// Mask of possible positions for a house and number
	pub(crate) house_poss_positions: State<[[Set<Position<House>>; 9]; 27]>,
}

impl StrategySolver {
	pub fn from_sudoku(sudoku: Sudoku) -> StrategySolver {
		let deduced_entries = sudoku.iter()
			.enumerate()
			.filter_map(|(cell, opt_num)| {
				opt_num.map(|num| Entry { cell: cell as u8, num })
			}).collect();
		StrategySolver {
			deductions: vec![],
			deduced_entries,
			eliminated_entries: vec![],
			n_solved: 0,
			grid: State::from(Sudoku([0; 81])),
			cell_poss_digits: State::from(Array81([Set::ALL; 81])),
			house_solved_digits: State::from([Set::NONE; 27]),
			house_poss_positions: State::from([[Set::ALL; 9]; 27]),
		}

	}

	/// Returns the current state of the Sudoku
	pub fn to_sudoku(&mut self) -> Sudoku {
		self.update_grid();
		self.grid.state
	}

	/// Returns the current state of the Sudoku
	pub fn grid_state(&mut self) -> [CellState; 81] {
		let mut grid = [CellState::Number(0); 81];
		self.update_grid();
		// TODO: continue despite error
		let _ = self._update_cell_poss_zone_solved(false);

		for (cell, &digits) in self.cell_poss_digits.state.iter().enumerate() {
			grid[cell] = CellState::Candidates(digits);
		}
		for (cell, &digit) in self.grid.state.0.iter().enumerate().filter(|(_, &digit)| digit != 0) {
			grid[cell] = CellState::Number(digit);
		}
		grid
	}

	/// Returns the current state of the cell
	pub fn cell_state(&mut self, cell: u8) -> CellState {
		if cell >= 81 {
			panic!("cell {} does not exist. valid cell range is 0..=80", cell);
		}
		self.update_grid();
		let _ = self._update_cell_poss_zone_solved(false);

		let cell = cell as usize;
		let num = self.grid.state.0[cell];
		if num != 0 {
			CellState::Number(num)
		} else {
			let digits = self.cell_poss_digits.state[cell];
			CellState::Candidates(digits)
		}
	}

	/// Try to insert the given entry. Fails, if the cell already contains a digit.
	pub fn insert_entry(&mut self, entry: Entry) -> Result<(), ()> {
		self.update_grid();
		Self::push_new_entry(
			&mut self.grid.state,
			&mut self.deduced_entries,
			entry,
			&mut self.deductions,
			_Deduction::Given(entry)
		)
		.map_err(|Unsolvable| ())?;

		Ok(())
	}

	fn into_deductions(self) -> Deductions {
		Deductions {
			deductions: self.deductions,
			deduced_entries: self.deduced_entries,
			eliminated_entries: self.eliminated_entries,
		}
	}

	fn update_grid(&mut self) {
		for &Entry { cell, num } in &self.deduced_entries {
			self.grid.state.0[cell as usize] = num;
		}
	}

	/// Try to solve the sudoku using the given `strategies`. Returns a `Result` of the sudoku and a struct containing the series of deductions.
	/// If a solution was found, `Ok(..)` is returned, otherwise `Err(..)`.
	pub fn solve(mut self, strategies: &[Strategy]) -> Result<(Sudoku, Deductions), (Sudoku, Deductions)> {
		self.try_solve(strategies);
		self.update_grid();
		match self.is_solved() {
			true => Ok((self.grid.state, self.into_deductions())),
			false => Err((self.grid.state, self.into_deductions())),
		}
	}

	// FIXME: change name
	/// Try to solve the sudoku using the given `strategies`. Returns `true` if new deductions were made.
	pub fn try_solve(&mut self, strategies: &[Strategy]) -> bool {
		// first strategy can be optimized
		let (first, rest) = match strategies.split_first() {
			Some(tup) => tup,
			// no chance without strategies
			None => return false,
		};
		let lens = (self.deduced_entries.len(), self.eliminated_entries.len());
		'outer: loop {
			if self.is_solved() {
				break
			}

			let n_deductions = self.deduced_entries.len();
			let n_eliminated = self.eliminated_entries.len();
			if first.deduce_all(self, true).is_err() { break };
			if self.deduced_entries.len() > n_deductions {
				continue 'outer
			}

			for strategy in rest {
				if strategy.deduce_one(self).is_err() {
					break;
				};
				if self.deduced_entries.len() > n_deductions || self.eliminated_entries.len() > n_eliminated {
					continue 'outer
				}
			}
			break
		}
		lens < (self.deduced_entries.len(), self.eliminated_entries.len())
	}

	fn hint(&mut self, strategies: &[Strategy]) -> Option<Hint> {
		let lens = (self.deduced_entries.len(), self.eliminated_entries.len());
		for strategy in strategies {
			if strategy.deduce_one(self).is_err() {
				break
			}
			if (self.deduced_entries.len(), self.eliminated_entries.len()) > lens {
				return Some(Hint {
					solver: self,
					old_n_deductions: lens.0,
					old_n_eliminations: lens.1,
				})
			}
		}
		None
	}

	pub fn is_solved(&self) -> bool {
		self.n_solved == 81
	}

	fn update_cell_poss_zone_solved(&mut self) -> Result<(), Unsolvable> {
		self._update_cell_poss_zone_solved(false)
	}

	fn _update_cell_poss_zone_solved(&mut self, find_naked_singles: bool) -> Result<(), Unsolvable> {
		{
			let (_, le_cp, cell_poss) = self.cell_poss_digits.get_mut();

			for &entry in &self.eliminated_entries[*le_cp as _..] {
				let impossibles = entry.digit_set();

				// deductions made here may conflict with entries already in the queue
				// in the queue. In that case the sudoku is impossible.
				Self::remove_impossibilities(&mut self.grid.state, cell_poss, entry.cell_type(), impossibles, &mut self.deduced_entries, &mut self.deductions, find_naked_singles)?;
			}
			*le_cp = self.eliminated_entries.len() as _;
		}

		self.insert_entries(find_naked_singles)
	}

	fn update_zone_poss_positions(&mut self) -> Result<(), Unsolvable> {
		// TODO: this has to do massive amounts of work
		//       may just be easier to recompute from full grid every time

		let (ld, le, house_poss_positions) = self.house_poss_positions.get_mut();
		// remove now impossible positions from list
		for entry in &self.eliminated_entries[*le as usize ..] {
			let cell = entry.cell_type();
			let row_pos = cell.row_pos();
			let col_pos = cell.col_pos();
			let block_pos = cell.block_pos();
			let row = Row::from(cell);
			let col = Col::from(cell);
			let block = Block::from(cell);
			// just 1 num
			let num = entry.num as usize - 1;
			//for num in entry.mask().iter() {
			house_poss_positions[row.house_index()][num].remove(row_pos.into());
			house_poss_positions[col.house_index()][num].remove(col_pos.into());
			house_poss_positions[block.house_index()][num].remove(block_pos.into());
			//}
		}
		*le = self.eliminated_entries.len() as _;

		for entry in &self.deduced_entries[*ld as usize ..] {
			let cell = entry.cell_type();
			let num = entry.num as usize - 1;

			// remove num from every house pos in all neighbouring cells
			for cell in neighbours2(cell) {
				let row = Row::from(cell);
				let col = Col::from(cell);
				let block = Block::from(cell);
				let row_pos = cell.row_pos();
				let col_pos = cell.col_pos();
				let block_pos = cell.block_pos();
				house_poss_positions[row.house_index()][num].remove(row_pos.into());
				house_poss_positions[col.house_index()][num].remove(col_pos.into());
				house_poss_positions[block.house_index()][num].remove(block_pos.into());
			}

			let row = Row::from(cell);
			let col = Col::from(cell);
			let block = Block::from(cell);
			let row_pos = cell.row_pos();
			let col_pos = cell.col_pos();
			let block_pos = cell.block_pos();

			// remove entry pos as possible place for all nums
			for num_off in 0..9 {
				house_poss_positions[row.house_index()][num_off].remove(row_pos.into());
				house_poss_positions[col.house_index()][num_off].remove(col_pos.into());
				house_poss_positions[block.house_index()][num_off].remove(block_pos.into());
			}

			// remove all pos as possible place for entry num
			house_poss_positions[row.house_index()][num] = Set::NONE;
			house_poss_positions[col.house_index()][num] = Set::NONE;
			house_poss_positions[block.house_index()][num] = Set::NONE;
		}
		*ld = self.deduced_entries.len() as _;
		Ok(())
	}

	#[inline(always)]
	fn insert_entries(&mut self, find_naked_singles: bool) -> Result<(), Unsolvable> {
		// code hereafter depends on this
		// but it's not necessary in general
		assert!(self.cell_poss_digits.next_deduced == self.house_solved_digits.next_deduced);

		// TODO: Delete?
		// start off with batch insertion so every cell is visited at least once
		// because other strategies may have touched their possibilities which singly_insertion may miss
		self.batch_insert_entries(find_naked_singles)?;
		loop {
			match self.deduced_entries.len() - self.cell_poss_digits.next_deduced as usize {
				0 => break Ok(()),
				1...4 => self.insert_entries_singly(find_naked_singles)?,
				_ => self.batch_insert_entries(find_naked_singles)?,
			}
		}
	}

	// for each entry in the stack, insert it (if cell is unsolved)
	// and then remove possibility from each cell neighbouring it in all
	// zones (rows, cols, fields) eagerly
	// check for naked singles and impossible cells during this check
	fn insert_entries_singly(&mut self, find_naked_singles: bool) -> Result<(), Unsolvable> {
		let (ld_cp, _, cell_poss_digits) = self.cell_poss_digits.get_mut();
		let (ld_zs, _, house_solved_digits) = self.house_solved_digits.get_mut();

		loop {
			if self.deduced_entries.len() <= *ld_cp as usize { break }
			let entry = self.deduced_entries[*ld_cp as usize];
			*ld_cp += 1;
			*ld_zs += 1;
			let entry_mask = entry.digit_set();
			// cell already solved from previous entry in stack, skip
			if cell_poss_digits[entry.cell()] == Set::NONE { continue }

			// is entry still possible?
			if cell_poss_digits[entry.cell()] & entry_mask == Set::NONE {
				return Err(Unsolvable);
			}

			Self::_insert_entry_cp_zs(entry, &mut self.n_solved, cell_poss_digits, house_solved_digits);
			for cell in neighbours2(entry.cell_type()) {
				if entry_mask & cell_poss_digits[cell.as_index()] != Set::NONE {
					Self::remove_impossibilities(&mut self.grid.state, cell_poss_digits, cell, entry_mask, &mut self.deduced_entries, &mut self.deductions, find_naked_singles)?;
				};
			}

			// found a lot of naked singles, switch to batch insertion
			if self.deduced_entries.len() - *ld_cp as usize > 4 { return Ok(()) }
		}
		Ok(())
	}

	#[inline]
	fn _insert_entry_cp_zs(
		entry: Entry,
		n_solved: &mut u8,
		cell_poss_digits: &mut Array81<Set<Digit>>,
		house_solved_digits: &mut [Set<Digit>; 27]
	) {
		*n_solved += 1;
		cell_poss_digits[entry.cell()] = Set::NONE;
		house_solved_digits[entry.row() as usize +ROW_OFFSET] |= entry.digit_set();
		house_solved_digits[entry.col() as usize +COL_OFFSET] |= entry.digit_set();
		house_solved_digits[entry.field() as usize +FIELD_OFFSET] |= entry.digit_set();
	}

	fn batch_insert_entries(&mut self, find_naked_singles: bool) -> Result<(), Unsolvable> {
		let (ld_cp, _, cell_poss_digits) = self.cell_poss_digits.get_mut();
		let (ld_zs, _, house_solved_digits) = self.house_solved_digits.get_mut();
		while self.deduced_entries.len() > *ld_cp as usize {
			let entry = self.deduced_entries[*ld_cp as usize];
			*ld_cp += 1;
			*ld_zs += 1;
			// cell already solved from previous entry in stack, skip
			if cell_poss_digits[entry.cell()] == Set::NONE { continue }

			let entry_mask = entry.digit_set();

			// is entry still possible?
			// have to check house possibilities, because cell possibility
			// is temporarily out of date
			if house_solved_digits[entry.row() as usize + ROW_OFFSET] & entry_mask != Set::NONE
			|| house_solved_digits[entry.col() as usize + COL_OFFSET] & entry_mask != Set::NONE
			|| house_solved_digits[entry.field() as usize +FIELD_OFFSET] & entry_mask != Set::NONE
			{
				return Err(Unsolvable);
			}

			Self::_insert_entry_cp_zs(entry, &mut self.n_solved, cell_poss_digits, house_solved_digits);
		}

		// update cell possibilities from house masks
		for cell in (0..81).map(Cell::new) {
			if cell_poss_digits[cell.as_index()] == Set::NONE { continue }
			let zones_mask = house_solved_digits[Row::from(cell).house_index()]
				| house_solved_digits[Col::from(cell).house_index()]
				| house_solved_digits[Block::from(cell).house_index()];

			Self::remove_impossibilities(&mut self.grid.state, cell_poss_digits, cell, zones_mask, &mut self.deduced_entries, &mut self.deductions, find_naked_singles)?;
		}
		Ok(())
	}

	// remove impossible digits from masks for given cell
	// also check for naked singles and impossibility of sudoku
	fn remove_impossibilities(
		sudoku: &mut Sudoku,
		cell_poss_digits: &mut Array81<Set<Digit>>,
		cell: Cell,
		impossible: Set<Digit>,
		deduced_entries: &mut Vec<Entry>,
		deductions: &mut Vec<_Deduction>,
		find_naked_singles: bool,
	) -> Result<(), Unsolvable> {
		let cell_mask = &mut cell_poss_digits[cell.as_index()];
		cell_mask.remove(impossible);

		if find_naked_singles {
			if let Some(num) = cell_mask.unique()? {
				let entry = Entry { cell: cell.val(), num: num.val() };
				Self::push_new_entry(sudoku, deduced_entries, entry, deductions, _Deduction::NakedSingles(entry))?;
			}
		} else {
			if *cell_mask == Set::NONE {
				return Err(Unsolvable)
			}
		}
		Ok(())
	}

	fn push_new_entry(
		sudoku: &mut Sudoku,
		deduced_entries: &mut Vec<Entry>,
		entry: Entry,
		deductions: &mut Vec<_Deduction>,
		strategy: _Deduction // either a user-given or naked or hidden singles
	) -> Result<(), Unsolvable> {

		#[cfg(debug_assertions)]
		{
			use self::_Deduction::*;
			match strategy {
				NakedSingles(..) | HiddenSingles(..) | Given(_) => (),
				_ => panic!("Internal error: Called push_new_entry with wrong strategy type")
			};
		}

		let old_num = &mut sudoku.0[entry.cell()];
		match *old_num {
			n if n == entry.num => return Ok(()),  // previously solved
			0 => (),                              // not solved
			_ => return Err(Unsolvable),          // conflict
		}
		*old_num = entry.num;
		deduced_entries.push(entry);
		deductions.push(strategy);
		Ok(())
	}

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	////////      Strategies
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	fn find_naked_singles(&mut self, stop_after_first: bool) -> Result<(), Unsolvable> {
		self.update_cell_poss_zone_solved()?;

		for (cell, poss_digits) in (0..).zip(self.cell_poss_digits.state.iter()) {
			// if Err(_), then it's Set::NONE and the cell is already solved (or impossible)
			// skip in that case (via unwrap_or(None))
			if let Some(num) = poss_digits.unique().unwrap_or(None) {
				let entry = Entry { cell, num: num.val() };

				Self::push_new_entry(&mut self.grid.state, &mut self.deduced_entries, entry, &mut self.deductions, _Deduction::NakedSingles(entry))?;
				//Self::push_new_entry(sudoku, deduced_entries, entry, deductions, _Deduction::NakedSingles(entry))?;
				self.deduced_entries.push(entry);
				if stop_after_first {
					break
				}
			}
		}
		// call update again so newly found entries are inserted
		self.update_cell_poss_zone_solved()
	}

	fn find_hidden_singles(&mut self, stop_after_first: bool) -> Result<(), Unsolvable> {
		// TODO: remove auto-deducing naked singles inside update procedure
		self.update_cell_poss_zone_solved()?;

		for house in (0..27).map(House::new) {
			let mut unsolved: Set<Digit> = Set::NONE;
			let mut multiple_unsolved = Set::NONE;

			let cells = house.cells();
			for cell in cells {
				let poss_digits = self.cell_poss_digits.state[cell.as_index()];
				multiple_unsolved |= unsolved & poss_digits;
				unsolved |= poss_digits;
			}
			if unsolved | self.house_solved_digits.state[house.as_index()] != Set::ALL {
				return Err(Unsolvable);
			}

			let mut singles = unsolved.without(multiple_unsolved);
			if singles.is_empty() { continue }

			for cell in cells {
				let mask = self.cell_poss_digits.state[cell.as_index()];

				if let Ok(maybe_unique) = (mask & singles).unique() {
					let num = maybe_unique.ok_or(Unsolvable)?;
					let entry = Entry { cell: cell.val(), num: num.val() };
					let strat_res = _Deduction::HiddenSingles(entry, house.categorize());
					Self::push_new_entry(&mut self.grid.state, &mut self.deduced_entries, entry, &mut self.deductions, strat_res)?;

					// mark num as found
					singles.remove(Set::from(num));

					// everything in this house found
					// return to insert numbers immediately
					match stop_after_first {
						true => return Ok(()),
						false if singles.is_empty() => break, // continue next house
						_ => (), // find rest of singles in house
					}
				}
			}
		}
		Ok(())
	}

	// stop after first will only eliminate line OR field neighbours for ONE number
	// even if multiple are found at the same time
	fn find_locked_candidates(&mut self, stop_after_first: bool) -> Result<(), Unsolvable> {
        self.update_cell_poss_zone_solved()?;
		let (_, _, cell_poss_digits) = self.cell_poss_digits.get_mut();

		for chute in (0..6).map(Chute::new) {
			let mut miniline_poss_digits: [Set<Digit>; 9] = [Set::NONE; 9];

			{ // compute possible digits for each miniline
			// TODO: switch to using house_solved_digits?
				let minilines = chute.minilines();
				for (&miniline, poss_digs) in minilines.iter().zip(miniline_poss_digits.iter_mut()) {
					for cell in miniline.cells() {
						*poss_digs |= cell_poss_digits[cell.as_index()];
					}
				}
			}

			let mut line_unique_digits: [Set<Digit>; 3] = [Set::NONE; 3];
			let mut field_unique_digits: [Set<Digit>; 3] = [Set::NONE; 3];

			{
				let poss_digits = |chute_line, chute_field| miniline_poss_digits[ chute_line*3 + chute_field];
				for chute_line in 0..3 {
					let poss_digits_iter = (0..3)
						.map(|chute_field| poss_digits(chute_line, chute_field) );

					let (_, _, unique) = find_unique(poss_digits_iter);
					line_unique_digits[chute_line] = unique;
				}
				for chute_field in 0..3 {
					let poss_digits_iter = (0..3)
						.map(|chute_line| poss_digits(chute_line, chute_field) );;

					let (_, _, unique) = find_unique(poss_digits_iter);
					field_unique_digits[chute_field] = unique;
				}
			}

			// find minilines that contain the computed unique digits
			// remove them from the appropriate neighbours
			for (i, (&miniline, &poss_digits)) in chute.minilines().iter()
				.zip(miniline_poss_digits.iter())
				.enumerate()
			{
				let chute_line = i / 3;
				let chute_field = i % 3;

				let line_uniques =  poss_digits & line_unique_digits[chute_line];
				let field_uniques = poss_digits & field_unique_digits[chute_field];

				let (line_neighbours, field_neighbours) = miniline.neighbours();

				let eliminated_entries = &mut self.eliminated_entries;
				let mut find_impossibles = |uniques, neighbours: &[MiniLine; 2]| {
					let n_eliminated = eliminated_entries.len();
					for &neighbour in neighbours {
						let conflicts = miniline_poss_digits[neighbour.chute().as_index()] & uniques;
						if conflicts == Set::NONE { continue }

						for cell in neighbour.cells() {
							let conflicts = cell_poss_digits[cell.as_index()] & uniques;
							for num in conflicts {
								eliminated_entries.push( Entry { cell: cell.val(), num: num.val() } )
							}
						}
					}
					n_eliminated..eliminated_entries.len()
				};

				for &(uniques, neighbours) in [(line_uniques, &field_neighbours), (field_uniques, &line_neighbours)].iter()
					.filter(|&&(uniques, _)| uniques != Set::NONE)
				{
					let rg_eliminations = find_impossibles(uniques, neighbours);
					if rg_eliminations.len() > 0 {
						// TODO: If stop_after_first is true, only enter the number whose conflicts were eliminated
						self.deductions.push(_Deduction::LockedCandidates(miniline, uniques, rg_eliminations));

						if stop_after_first {
							return Ok(());
						}
					}
				}
			}
		}
		Ok(())
	}


	fn find_naked_subsets(&mut self, subset_size: usize, stop_after_first: bool) -> Result<(), Unsolvable> 	{
		fn walk_combinations(
			state: &mut StrategySolver,
			total_poss_digs: Set<Digit>,
			cells: SetIter<Cell>,
			house: House,
			stack: &mut Vec<Cell>,
			subset_size: usize,
			stop_after_first: bool,
		) -> bool {
			// subsets of 5 and more numbers always have complementary subsets
			// of 9 - subset_size
			if stack.len() > subset_size { return false }
			if stack.len() == subset_size && total_poss_digs.len() == stack.len() as u8 {
				// found a subset
				let n_eliminated = state.eliminated_entries.len();
				for cell in house.cells().into_iter().filter(|cell| !stack.contains(cell)) {
					let conflicts = state.cell_poss_digits.state[cell.as_index()] & total_poss_digs;
					for num in conflicts {
						state.eliminated_entries.push(Entry{ cell: cell.val(), num: num.val() });
					}
				}
				let rg_eliminations = n_eliminated..state.eliminated_entries.len();
				if rg_eliminations.len() > 0 {
					state.deductions.push(_Deduction::NakedSubsets {
						house,
						cells: stack.clone(),
						digits: total_poss_digs,
						conflicts: rg_eliminations
					});
					if stop_after_first {
						return true
					}
				}
			}

			let mut cells = cells;
			while let Some(cell) = cells.next() {
				let cell_poss_digits = state.cell_poss_digits.state[cell.as_index()];
				// solved cell
				if cell_poss_digits == Set::NONE { continue }
				stack.push(cell);
				let new_total_poss_digs = total_poss_digs | cell_poss_digits;

				// if true, then a subset was found and stop_after_first is set
				// stop recursion
				if walk_combinations(state, new_total_poss_digs, cells.clone(), house, stack, subset_size, stop_after_first) {
					return true
				};
				stack.pop();
			}
			false
		}
		self.update_cell_poss_zone_solved()?;

		let mut stack = vec![];
		for house in (0..27).map(House::new) {
			if self.house_solved_digits.state[house.as_index()] == Set::ALL { continue }
			let cells = house.cells();
			// if true, then a subset was found and stop_after_first is set
			// stop looking
			if walk_combinations(self, Set::NONE, cells.into_iter(), house, &mut stack, subset_size, stop_after_first) {
				break
			};
		}
		Ok(())
	}

	fn find_hidden_subsets(&mut self, subset_size: usize, stop_after_first: bool) -> Result<(), Unsolvable> {
		fn walk_combinations(
			state: &mut StrategySolver,
			house: House,
			total_poss_pos: Set<Position<House>>,
			digits: SetIter<Digit>,
			all_digits: Set<Digit>,
			stack: &mut Vec<Digit>,
			subset_size: usize,
			stop_after_first: bool,
		) -> bool {
			if stack.len() > subset_size { return false }
			let house_poss_positions = state.house_poss_positions.state[house.as_index()];

			if stack.len() == subset_size && total_poss_pos.len() == stack.len() as u8 {

				let n_eliminated = state.eliminated_entries.len();
				for digit in all_digits.into_iter().filter(|dig| !stack.contains(dig)) {
					let conflicts = house_poss_positions[digit.as_index()] & total_poss_pos;
					for pos in conflicts {
						let cell = house.cell_at(pos);
						state.eliminated_entries.push(Entry{ cell: cell.val(), num: digit.val() });
					}
				}
				let rg_eliminations = n_eliminated..state.eliminated_entries.len();
				if rg_eliminations.len() > 0 {
					state.deductions.push(_Deduction::HiddenSubsets {
						house,
						digits: stack.clone(),
						positions: total_poss_pos,
						conflicts: rg_eliminations
					});
					if stop_after_first {
						return true
					}
				}
			}

			let mut digits = digits;
			while let Some(digit) = digits.next() {
				let num_poss_pos = house_poss_positions[digit.as_index()];
				// solved cell
				if num_poss_pos == Set::NONE { continue }
				stack.push(digit);
				let new_total_poss_pos = total_poss_pos | num_poss_pos;
				if walk_combinations(state, house, new_total_poss_pos, digits.clone(), all_digits, stack, subset_size, stop_after_first) {
					return true
				};
				stack.pop();
			}
			false
		}

		self.update_zone_poss_positions()?;

		let mut stack = vec![];
		for house in (0..27).map(House::new) {
			if self.house_solved_digits.state[house.as_index()] == Set::ALL { continue }
			let digits = Set::ALL;
			if walk_combinations(self, house, Set::NONE, digits.into_iter(), digits, &mut stack, subset_size, stop_after_first) {
				break
			};
		}
		Ok(())
	}

	fn find_xwings(&mut self, stop_after_first: bool) {
		self.find_fish(2, stop_after_first)
	}


	fn find_swordfish(&mut self, stop_after_first: bool) {
		self.find_fish(3, stop_after_first)
	}


	fn find_jellyfish(&mut self, stop_after_first: bool) {
		self.find_fish(4, stop_after_first)
	}

	fn find_fish(&mut self, max_size: usize, stop_after_first: bool) {
		self.update_zone_poss_positions().unwrap();
		let mut stack = vec![];
		for digit in (1..10).map(Digit::new) {
			// 0..9 = rows, 9..18 = cols
			for lines in &[Line::ALL_ROWS, Line::ALL_COLS] {
				if basic_fish_walk_combinations(self, digit, max_size, &mut stack, lines, lines, Set::NONE, stop_after_first) {
					return
				};
			}
		}
	}

	fn find_singles_chain(&mut self) -> Result<(), Unsolvable> {
        #[derive(Copy, Clone, PartialEq, Eq)]
        enum Colour {
            Uncoloured,
            A,
            B,
        }

        fn follow_links(digit: Digit, cell: Cell, is_a: bool, sudoku: &StrategySolver, cell_color: &mut [Colour; 81], link_nr: u8, cell_linked: &mut [u8; 81]) {
            if cell_linked[cell.as_index()] <= link_nr { return }

            for &(con_zone, current_pos) in &[
                (cell.row().house(), cell.row_pos().house_pos()),
                (cell.col().house(), cell.col_pos().house_pos()),
                (cell.block().house(), cell.block_pos().house_pos()),
            ] {
                let house_poss_positions = sudoku.house_poss_positions.state[con_zone.as_index()][digit.as_index()];
                if house_poss_positions.len() == 2 {
                    let other_pos = house_poss_positions.without(Set::from(current_pos)).unique().unwrap().unwrap();
                    let other_cell = con_zone.cell_at(other_pos);

                    match cell_linked[other_cell.as_index()] <= link_nr {
                        true => continue,
                        false => cell_linked[other_cell.as_index()] = link_nr,
                    };

                    cell_color[other_cell.as_index()] = if is_a { Colour::A } else { Colour::B };

                    follow_links(digit, other_cell, !is_a, sudoku, cell_color, link_nr, cell_linked);
                }
            }
        }

        for digit in Set::<Digit>::ALL {
            let mut cell_touched = [false; N_CELLS];
            let mut link_nr = 0;

            let mut cell_linked = [0; 81];
            let mut cell_color = [Colour::Uncoloured; 81];

            for house in (0..27).map(House::new) {
                let house_poss_positions = self.house_poss_positions.state[house.as_index()][digit.as_index()];
                if house_poss_positions.len() == 2 {
                    let first = house_poss_positions.into_iter().next().unwrap();
                    let cell = house.cell_at(first);

                    match cell_touched[cell.as_index()] {
                        true => continue,
                        false => cell_touched[cell.as_index()] = true,
                    };

                    follow_links(digit, cell, true, self, &mut cell_color, link_nr, &mut cell_linked);
                    link_nr += 1;
                }
            }

            for link_nr in 0..link_nr {
                // Rule 1:
                // if two cells in the same row, part of the same chain
                // have the same color, those cells must not contain the number
                // Rule 2:
                // if one cell is neighbour to two cells with opposite colours
                // it can not contain the number


                // ===== Rule 1 ======
                for house in (0..27).map(House::new) {
                    // Collect colours in this link chain and this house
                    let mut zone_colors = [Colour::Uncoloured; 9];
                    for (pos, cell) in house.cells()
                        .into_iter()
                        .enumerate()
                        // TODO: Double check the logic here
                        // this used to take the pos for indexing
                        .filter(|(_, cell)| cell_linked[cell.as_index()] == link_nr)
                    {
                        zone_colors[pos] = cell_color[cell.as_index()];
                    }

                    let (n_a, n_b) = zone_colors.iter()
                        .fold((0, 0), |(n_a, n_b), &colour| {
                            match colour {
                                Colour::A => (n_a+1, n_b),
                                Colour::B => (n_a, n_b+1),
                                Colour::Uncoloured => (n_a, n_b),
                            }
                        });

                    fn mark_impossible(num: u8, link_nr: u8, colour: Colour, cell_color: [Colour; 81], cell_linked: [u8; 81], impossible_entries: &mut Vec<Entry>) {
                        (0..81).zip(cell_color.iter()).zip(cell_linked.iter())
                            .filter(|&((_, &cell_colour), &cell_link_nr)| link_nr == cell_link_nr && colour == cell_colour)
                            .for_each(|((cell, _), _)| impossible_entries.push( Entry { cell, num }));
                    }

                    let impossible_colour;
                    match (n_a >= 2, n_b >= 2) {
                        (true, true) => return Err(Unsolvable),
                        (true, false) => impossible_colour = Colour::A,
                        (false, true) => impossible_colour = Colour::B,
                        (false, false) => continue,
                    };
                    mark_impossible(digit.val(), link_nr, impossible_colour, cell_color, cell_linked, &mut self.eliminated_entries);
                    // chain handled, go to next
                    // note: as this eagerly marks a colour impossible as soon as a double in any colour is found
                    //       a case of two doubles in some later house will not always be found
                    //       impossibility is then detected further down the strategy chain
                    break
                }

                // ===== Rule 2 =====
                let mut cell_sees_colour = [(false, false); 81];
                for ((cell, &cell_colour), _) in (0..81).map(Cell::new).
                    zip(cell_color.iter())
                    .zip(cell_linked.iter())
                    .filter(|&((_, &cell_colour), &cell_link_nr)| link_nr == cell_link_nr && cell_colour != Colour::Uncoloured)
                {
                    for &house in &cell.houses() {
                        for neighbour_cell in house.cells().into_iter().filter(|&c| cell != c) {
                            let (sees_a, sees_b) = cell_sees_colour[neighbour_cell.as_index()];
                            if cell_colour == Colour::A && !sees_a {
                                cell_sees_colour[neighbour_cell.as_index()].0 = true;
                                if sees_b {
                                    self.eliminated_entries.push( Entry{ cell: neighbour_cell.val(), num: digit.val() })
                                }
                            } else if cell_colour == Colour::B && !sees_b {
                                cell_sees_colour[neighbour_cell.as_index()].1 = true;
                                if sees_a {
                                    self.eliminated_entries.push( Entry{ cell: neighbour_cell.val(), num: digit.val() })
                                }
                            }
                        }
                    }
                }
            }
        }
		Ok(())
	}
}

//             goal_depth
// <degenerated>   1 (basically a naked/hidden single, not supported by this fn)
// x-wing          2
// swordfish       3
// jellyfish       4
fn basic_fish_walk_combinations(
	sudoku: &mut StrategySolver,
	digit: Digit,
	goal_depth: usize,
	stack: &mut Vec<Line>,
	lines: &[Line],
	all_lines: &[Line; 9],
	union_poss_pos: Set<Position<Line>>,
	stop_after_first: bool,
) -> bool {
	if stack.len() == goal_depth {
		// nothing of interest found
		if union_poss_pos.len() != goal_depth as u8 { return false }
		// found xwing, swordfish, jellyfish, whatever-the-name
		let n_eliminated = sudoku.eliminated_entries.len();
		for line in all_lines.iter().filter(|&line| !stack.contains(line)) {
			for pos in union_poss_pos {
				let cell = line.cell_at(pos);
				let cell_mask = sudoku.cell_poss_digits.state[cell.as_index()];
				if cell_mask & Set::from(digit) != Set::NONE {
					sudoku.eliminated_entries.push(Entry{ num: digit.val(), cell: cell.val() });
				}
			}
		}

		let rg_eliminations = n_eliminated..sudoku.eliminated_entries.len();
		if rg_eliminations.len() > 0 {

			let lines = stack.clone();
			let positions = union_poss_pos;
			let conflicts = rg_eliminations;

			sudoku.deductions.push(
				_Deduction::BasicFish {
					lines, digit, conflicts, positions,
				}
			);
			if stop_after_first {
				return true
			}
		}
	}
	for (i, &line) in lines.iter().enumerate() {
		let possible_pos = sudoku.house_poss_positions.state[line.as_index()][digit.as_index()];
		let n_poss = possible_pos.len();
		let new_union_poss_pos = union_poss_pos | possible_pos.as_line_set(); // TODO: remove the "cast"

		// n_poss == 0 => solved row (or impossible)
		// n_poss == 1 => hidden single
		if n_poss < 2 || new_union_poss_pos.len() > goal_depth as u8 { continue }
		stack.push(line);
		if basic_fish_walk_combinations(sudoku, digit, goal_depth, stack, &lines[i+1..], all_lines, new_union_poss_pos, stop_after_first) {
			return true
		};
		stack.pop();
	}
	false
}


#[derive(Debug, Clone)]
pub(crate) struct State<T> {
	next_deduced: u16,
	last_eliminated: u16, // probably doesn't exceed 2^8, but can't prove it
	state: T,
}

impl<T> State<T> {
	fn from(this: T) -> Self {
		State {
			next_deduced: 0,
			last_eliminated: 0,
			state: this,
		}
	}
}

impl<T> State<T> {
	fn get_mut(&mut self) -> (&mut u16, &mut u16, &mut T) {
		let State { next_deduced: ld, last_eliminated: le, state } = self;
		(ld, le, state)
	}
}
///////////////////////////////////////////////////////////////////////////////////////////

/// The strategies that can be used with the `StrategySolver` to find hints, solve or grade a `Sudoku`.
/// May be expanded in the future.
#[derive(Debug, Clone)]
pub enum Strategy {
    NakedSingles,
    HiddenSingles,
    LockedCandidates,
	NakedPairs,
	NakedTriples,
	NakedQuads,
	HiddenPairs,
	HiddenTriples,
	HiddenQuads,
    XWing,
    Swordfish,
    Jellyfish,
    SinglesChain,
    #[doc(hidden)] __NonExhaustive
}

impl Strategy {
	pub const ALL: &'static [Strategy] = &[
		                            // difficulty as assigned by
									// SudokuExplainer
		Strategy::NakedSingles,     // 23
		Strategy::HiddenSingles,    // 15
		Strategy::LockedCandidates, // 28
		Strategy::NakedPairs,       // 30
		Strategy::XWing,            // 32
		Strategy::HiddenPairs,      // 34
		Strategy::NakedTriples,     // 36
		Strategy::Swordfish,        // 38
		Strategy::HiddenTriples,    // 40
		Strategy::NakedQuads,       // 50
		Strategy::Jellyfish,        // 52
		Strategy::HiddenQuads,      // 54
		//Strategy::SinglesChain,
	];

	// is_first_strategy is an optimization hint
	// it doesn't need to be used
	fn deduce(&self, state: &mut StrategySolver, stop_after_first: bool, is_first_strategy: bool) -> Result<(), Unsolvable> {
		use self::Strategy::*;
        match *self {
            NakedSingles if !stop_after_first && is_first_strategy => state._update_cell_poss_zone_solved(true),
			NakedSingles => state.find_naked_singles(stop_after_first),
			HiddenSingles => state.find_hidden_singles(stop_after_first),
			LockedCandidates => state.find_locked_candidates(stop_after_first),
			NakedPairs => state.find_naked_subsets(2, stop_after_first),
			NakedTriples => state.find_naked_subsets(3, stop_after_first),
			NakedQuads => state.find_naked_subsets(4, stop_after_first),
			HiddenPairs => state.find_hidden_subsets(2, stop_after_first),
			HiddenTriples => state.find_hidden_subsets(3, stop_after_first),
			HiddenQuads => state.find_hidden_subsets(4, stop_after_first),
			XWing => { state.find_xwings(stop_after_first); Ok(()) },
			Swordfish => { state.find_swordfish(stop_after_first); Ok(()) },
			Jellyfish => { state.find_jellyfish(stop_after_first); Ok(()) },
			SinglesChain if !stop_after_first => state.find_singles_chain(), // TODO: Implement non-eager SinglesChain
            _ => unimplemented!(),
        }
	}

    fn deduce_one(&self, state: &mut StrategySolver) -> Result<(), Unsolvable> {
        self.deduce(state, true, false)
    }

    fn deduce_all(&self, state: &mut StrategySolver, is_first_strategy: bool) -> Result<(), Unsolvable> {
        self.deduce(state, false, is_first_strategy)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub enum DeductionResult<'a> {
	Forced(Entry),
	Eliminated(&'a [Entry]),
}

#[derive(Debug)]
/// Result of a single strategy
pub struct Deduction<'a> {
	deduction: &'a _Deduction,
	deduced_entries: &'a [Entry],
	eliminated_entries: &'a [Entry]
}

pub struct Hint<'a> {
	// last _Deduction is the solution
	solver: &'a mut StrategySolver,
	old_n_deductions: usize,
	old_n_eliminations: usize,
}

impl<'a> Hint<'a> {
	/// Enter the hinted deduction into the solver
	pub fn apply(self) {
		// it's already in there, just have to stop the auto-removal
		::std::mem::forget(self)
	}

	fn as_deduction(&self) -> Deduction {
		Deduction {
			deduction: self.solver.deductions.last().unwrap(),
			deduced_entries: &self.solver.deduced_entries,
			eliminated_entries: &self.solver.eliminated_entries,
		}
	}

	/// Returns the strategy that was used for this hint.
	fn strategy(&self) -> Strategy {
		self.as_deduction().strategy()
	}

	/// Returns the entries that were entered or ruled out as a result of this strategy application
	pub fn results(&self) -> DeductionResult {
		// can't go through temporary `Deduction` as that would be dropped
		self.solver.deductions.last().unwrap()
			.result(&self.solver.eliminated_entries)
	}
}

impl<'a> ::std::ops::Drop for Hint<'a> {
	fn drop(&mut self) {
		self.solver.deductions.pop();
		self.solver.eliminated_entries.truncate(self.old_n_eliminations);
		self.solver.deduced_entries.truncate(self.old_n_deductions);
	}
}

impl<'a> Deduction<'a> {
	/// Returns the strategy that was used for this deduction.
	pub fn strategy(&self) -> Strategy {
		self.deduction.strategy()
	}

	/*
	/// Returns the SudokuExplainer compatible difficulty of this `Deduction`
	pub fn se_difficulty(&self) -> u8 {
		self.deduction.se_difficulty()
	}
	*/

	/// Returns the entries that were entered or ruled out as a result of this deduction
	pub fn results(&self) -> DeductionResult {
		self.deduction.result(&self.eliminated_entries)
	}
}

#[derive(Debug)]
/// Contains the sequence of deductions made to solve / partially solve the sudoku
pub struct Deductions {
	deductions: Vec<_Deduction>,
	deduced_entries: Vec<Entry>,
	eliminated_entries: Vec<Entry>,
}

pub struct DeductionsIter<'a> {
	deductions: ::std::slice::Iter<'a, _Deduction>,
	deduced_entries: &'a [Entry],
	eliminated_entries: &'a [Entry]
}

impl<'a> Iterator for DeductionsIter<'a> {
	type Item = Deduction<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		self.deductions.next()
			.map(|deduction|
				Deduction {
					deduction,
					deduced_entries: &self.deduced_entries,
					eliminated_entries: &self.eliminated_entries,
				}
			)
	}
}

impl Deductions {
	/*
	/// Grade the difficulty of the sudoku like SudokuExplainer would.
	/// This means, it returns the SudokuExplainer compatible difficulty
	/// of the most difficult strategy used.
	/// If these deductions could not solve the sudoku, the difficulty is a lower bound.
	///
	/// If `Deductions` is empty, None will be returned.
	pub fn se_difficulty(&self) -> Option<u8> {
		self.deductions.iter()
			.map(_Deduction::se_difficulty)
			.max()
	}
	*/

	/// Returns the number of deductions.
	pub fn len(&self) -> usize {
		self.deductions.len()
	}

	/// Return the `index`th Deduction, if it exists.
	pub fn get(&self, index: usize) -> Option<Deduction<'_>> {
		self.deductions.get(index)
			.map(|deduction| Deduction {
				deduction,
				deduced_entries: &self.deduced_entries,
				eliminated_entries: &self.eliminated_entries,
			})
	}

	/// Return an iterator over the deductions.
	pub fn iter(&self) -> DeductionsIter<'_> {
		DeductionsIter {
			deductions: self.deductions.iter(),
			deduced_entries: &self.deduced_entries,
			eliminated_entries: &self.eliminated_entries,
		}
	}
	/*
	// For debugging solution paths
	fn print_deductions(&self) {
		for deduction in &self.deductions {
			print!("{:25?}: ", deduction.strategy());
			deduction.print_reason();
			print!("\n");
			match deduction.result(&self.eliminated_entries) {
				DeductionResult::Forced(entry) => println!("\tr{}c{} {}", entry.row()+1, entry.col()+1, entry.num),

				DeductionResult::Eliminated(deductions) => {
					for &entry in deductions {
						println!("\tr{}c{} {}", entry.row()+1, entry.col()+1, entry.num);
					}
				}

			}
		}
	}
	*/
}

// TODO:
// Expand and clean up
// This should ultimately be a public type exposing all the necessary information on how the deduction was made
// and with methods that can generate a sequence of explanation steps including
// highlighting of cells / houses / cell possibilities, their removal and cell-to-cell chains
// Basically, the GUI should only need to give the basic highlighting operations and we generate the explanation
#[derive(Debug, Clone)]
pub(crate) enum _Deduction {
	Given(Entry), // by user
    NakedSingles(Entry),
    HiddenSingles(Entry, HouseType),
    LockedCandidates(MiniLine, Set<Digit>, DeductionRange), // which miniline is affected and what's unique
    NakedSubsets {
		house: House,
		cells: Vec<Cell>,     // max 4
		digits: Set<Digit>,  // digits restricted to cells
		conflicts: DeductionRange, // link to impossible entries
	},
    HiddenSubsets {
		house: House,
		digits: Vec<Digit>,       // max 4
		positions: Set<Position<House>>,  // positions restricted to digits
		conflicts: DeductionRange,       // link to impossible entries
	},
    BasicFish {
		lines: Vec<Line>, // 2 lines, TODO: refactor
		positions: Set<Position<Line>>, // which positions in all lines
		digit: Digit,
		conflicts: DeductionRange,
	},

	// TODO: expand information in variants below
    SinglesChain(DeductionRange),
    #[doc(hidden)] __NonExhaustive
}

impl _Deduction {
	fn strategy(&self) -> Strategy {
		use self::_Deduction::*;
		match self {
			Given(_) => unimplemented!(),
			NakedSingles { .. } => Strategy::NakedSingles,
			HiddenSingles { .. } => Strategy::HiddenSingles,
			LockedCandidates { .. } => Strategy::LockedCandidates,
			BasicFish { positions, .. } => {
				match positions.len() {
					2 => Strategy::XWing,
					3 => Strategy::Swordfish,
					4 => Strategy::Jellyfish,
					_ => unreachable!(),
				}
			}
			SinglesChain { .. } => Strategy::SinglesChain,
			NakedSubsets { cells, .. } => {
				match cells.len() {
					2 => Strategy::NakedPairs,
					3 => Strategy::NakedTriples,
					4 => Strategy::NakedQuads,
					_ => unreachable!(),
				}
			}
			HiddenSubsets { digits, .. } => {
				match digits.len() {
					2 => Strategy::HiddenPairs,
					3 => Strategy::HiddenTriples,
					4 => Strategy::HiddenQuads,
					_ => unreachable!(),
				}
			}
			__NonExhaustive => unreachable!(),
		}
	}

	/*
	// SudokuExplainer compatible difficulty of deduction
	// FIXME: correct numbers and do the right analysis
	fn se_difficulty(&self) -> u8 {
		use self::_Deduction::*;
		match self {
			Given(_) => 0,
			NakedSingles(_) => 23,
			HiddenSingles(_, ZoneType::Block) => 12,
			HiddenSingles(_, _) => 15,
			LockedCandidates(_, _, _) => 28,
			NakedSubsets { cells, .. } => {
				match cells.len() {
					2 => 30,
					3 => 36,
					4 => 50,
					_ => unreachable!(),
				}
			}
			HiddenSubsets { num_offsets, .. } => {
				// fixme: direct hidden pairs and triplets are lower
				match num_offsets.len() {
					2 => 34,
					3 => 40,
					4 => 54,
					_ => unreachable!(),
				}
			}
			BasicFish { positions, .. } => {
				match positions.count() {
					2 => 32,
					3 => 38,
					4 => 52,
					_ => unreachable!(),
				}
			}
			SinglesChain(_) => unimplemented!(),
			__NonExhaustive => unreachable!(),
		}
	}
	*/

	/// Returns the entries that were entered or eliminated as a result of this
	/// deduction.
	fn result<'e>(&self, eliminated: &'e [Entry]) -> DeductionResult<'e> {
		use self::_Deduction::*;
		match self {
			| Given(_)
				=> unimplemented!(),
			| NakedSingles(entry)
			| HiddenSingles(entry, _)
				=> DeductionResult::Forced(*entry),
			| LockedCandidates(.., conflicts)
			| NakedSubsets { conflicts, .. }
			| HiddenSubsets { conflicts, .. }
			| BasicFish { conflicts, .. }
			| SinglesChain(conflicts)
				=> DeductionResult::Eliminated(&eliminated[conflicts.clone()]),
			| __NonExhaustive
				=> unreachable!(),
		}
	}
	/*
	// For debugging solution paths
	fn print_reason(&self) {
		use self::_Deduction::*;
		match *self {
			NakedSingles(entry) => print!("r{}c{} {}", entry.row()+1, entry.col()+1, entry.num),
			HiddenSingles(entry, zone_type) => {
				print!("r{}c{} {} {:?}", entry.row()+1, entry.col()+1, entry.num, zone_type);
			},
			LockedCandidates(slice, digit, _) => {
				print!("slice {} num: {}", slice.0, digit);
			},
			NakedSubsets { zone, ref cells, digits, .. } => {
				let zone_type = zone_type(zone.0);
				let nums = digits.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(",");
				let cells = cells.iter().map(|n| format!("r{}c{}", 1 + n.row().0, 1 + n.col().0 - 9)).collect::<Vec<_>>().join(",");
				print!("{:?} {} nums: {}, cells: {}", zone_type, 1 + zone.0 % 9, nums, cells);
			},
			HiddenSubsets { zone, ref num_offsets, positions, .. } => {
				// translate to naked subsets
				// TODO: Refactor the storage of both
				let mut digits = Mask::NONE;
				//let mut cells = vec![];
				for num in num_offsets {
					digits |= Mask::from_num(num + 1);
				}
				let cells = positions.iter()
					.map(|pos| Cell::from_zone_pos(zone, pos))
					.collect();

				_Deduction::NakedSubsets {
					zone, digits, cells, conflicts: 0..0
				}
				.print_reason();
			},
			/*
			XWing{..} => Strategy::XWing,
			Swordfish{..} => Strategy::Swordfish,
			Jellyfish{..} => Strategy::Jellyfish,
			SinglesChain{..} => Strategy::SinglesChain,
			__NonExhaustive => Strategy::__NonExhaustive,
			*/
			_ => ()
		}
	}
	*/
}


#[inline]
fn find_unique<I: Iterator<Item=Set<Digit>>>(possibilities: I) -> (Set<Digit>, Set<Digit>, Set<Digit>) {
	let mut unsolved = Set::NONE;
	let mut multiple_unsolved = Set::NONE;

	for poss_digits in possibilities {
		multiple_unsolved |= unsolved & poss_digits;
		unsolved |= poss_digits;
	}
	// >= 1, >1, =1 occurences
	(unsolved, multiple_unsolved, unsolved.without(multiple_unsolved) )
}

#[cfg(test)]
mod test {
    use super::*;
    fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    sudokus_str.lines()
            .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
            .collect()
    }

    fn strategy_solver_correct_solution<F>(sudokus: Vec<Sudoku>, solved_sudokus: Vec<Sudoku>, solver: F)
        where F: Fn(StrategySolver, &[Strategy]) -> Result<(Sudoku, Deductions), (Sudoku, Deductions)>,
    {
        let n_sudokus = sudokus.len();
        let strategies = Strategy::ALL;
        let mut unsolved = vec![];
        for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
            let cache = StrategySolver::from_sudoku(sudoku);
            match solver(cache, &strategies) {
                Ok((solution, _deductions)) => assert_eq!(solution, solved_sudoku),
                Err((part_solved, _deductions)) => unsolved.push((i, sudoku, part_solved, solved_sudoku)),
            }
        }
        if unsolved.len() != 0 {
            println!("Could not solve {}/{} sudokus:\n", unsolved.len(), n_sudokus);


            for (i, sudoku, part_solution, _solution) in unsolved {
            	println!("\nsudoku nr {}:\n{}\n{}\n{}", i+1, sudoku.to_str_line(), part_solution.to_str_line(), _solution.to_str_line());
            }
            panic!();
        }
    }

    #[test]
    fn strategy_solver_correct_solution_easy_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_easy_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve);
    }

    #[test]
    fn strategy_solver_correct_solution_medium_sudokus() {
		// the 9th sudoku requires more advanced strategies
		let filter_9 = |vec: Vec<_>| vec.into_iter()
			.enumerate()
			.filter(|&(i, _)| i != 8)
			.map(|(_, sudoku)| sudoku)
			.collect::<Vec<_>>();
        let sudokus = filter_9(read_sudokus( include_str!("../../sudokus/Lines/medium_sudokus.txt") ));
        let solved_sudokus = filter_9(read_sudokus( include_str!("../../sudokus/Lines/solved_medium_sudokus.txt") ));
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve);
    }
}

//
