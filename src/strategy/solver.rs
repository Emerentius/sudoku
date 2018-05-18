#![allow(unused)]
#![warn(unused_variables, unused_mut, unused_must_use)]
#![allow(missing_docs)]
use sudoku::Sudoku;
use types::{Array81, Mask, Digit, Position, Unsolvable, Entry};
use consts::*;
use positions::{
    row_zone, col_zone, field_zone, cells_of_zone, neighbours,
    Cell, Line, Zone, Slice, Band,
};

type DeductionRange = ::std::ops::Range<usize>;

/// The `StrategySolver` is the struct that allows applying human style strategies
/// for the solution of sudokus.
///
/// It is built from single `Sudoku` for which it for which it allows
/// the efficient strategy application. It can find hints or solve the
/// `Sudoku` completely and return the sequence of logical steps taken.
/// By weighting the difficulty of each strategy, it can also grade sudoku difficulties.

// To allow for the above functionality, this struct contains caches
// of various properties of the sudoku grid. The caches are lazily updated
// on demand. This avoids both unnecessary and repetitive work.
//
// Two histories are kept:
// 1. A list of all strategies that were successfully used to deduce or eliminate entries
// 2. Two lists for all deduced (or entered) and eliminated entries
//
// The 1st is for reporting the sequence of strategies applied
// The 2nd is for the updating of internal caches. It is kept simple to offer an easy interface
// and can contain duplicates.
//
// These two histories can contain overlapping information and the 1st one can also contain references
// to the 2nd but not vice versa.
#[derive(Debug, Clone)]
pub struct StrategySolver {
	pub(crate) employed_strategies: Vec<StrategyResult>,
	pub(crate) deduced_entries: Vec<Entry>,
	pub(crate) eliminated_entries: Vec<Entry>,
	pub(crate) n_solved: u8, // deduced_entries can contain duplicates so a separate counter is necessary
	// current state of the sudoku
	// for when it's faster to recompute from the end state
	// than update through the new entries
	pub(crate) grid: State<Sudoku>,
	// TODO: combine states that are updated together
	// Mask of possible numbers in cell
	pub(crate) cell_poss_digits: State<Array81<Mask<Digit>>>,
	// Mask of solved digits in zone
	pub(crate) zone_solved_digits: State<[Mask<Digit>; 27]>,
	// Mask of possible positions for a zone and number
	pub(crate) zone_poss_positions: State<[[Mask<Position>; 9]; 27]>,
}

impl StrategySolver {
	pub fn from_sudoku(sudoku: Sudoku) -> StrategySolver {
		let deduced_entries = sudoku.iter()
			.enumerate()
			.filter_map(|(cell, opt_num)| {
				opt_num.map(|num| Entry { cell: cell as u8, num })
			}).collect();
		StrategySolver {
			employed_strategies: vec![],
			deduced_entries,
			eliminated_entries: vec![],
			n_solved: 0,
			grid: State::from(Sudoku([0; 81])),
			cell_poss_digits: State::from(Array81([Mask::ALL; 81])),
			zone_solved_digits: State::from([Mask::NONE; 27]),
			zone_poss_positions: State::from([[Mask::ALL; 9]; 27]),
		}

	}

	/// Returns the current state of the Sudoku
	pub fn to_sudoku(&mut self) -> Sudoku {
		self.update_grid();
		self.grid.state
	}

	fn into_deductions(self) -> Deductions {
		Deductions {
			employed_strategies: self.employed_strategies,
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
				let impossibles = Mask::from_num(entry.num());

				// deductions made here may conflict with entries already in the queue
				// in the queue. In that case the sudoku is impossible.
				Self::remove_impossibilities(&mut self.grid.state, cell_poss, entry.cell, impossibles, &mut self.deduced_entries, &mut self.employed_strategies, find_naked_singles)?;
			}
			*le_cp = self.eliminated_entries.len() as _;
		}

		self.insert_entries(find_naked_singles)
	}

	fn update_zone_poss_positions(&mut self) -> Result<(), Unsolvable> {
		// TODO: this has to do massive amounts of work
		//       may just be easier to recompute from full grid every time

		let (ld, le, zone_poss_positions) = self.zone_poss_positions.get_mut();
		// remove now impossible positions from list
		for entry in &self.eliminated_entries[*le as usize ..] {
			let cell = entry.cell;
			let row_pos = Mask::row_pos_of_cell(cell);
			let col_pos = Mask::col_pos_of_cell(cell);
			let field_pos = Mask::field_pos_of_cell(cell);
			let row = row_zone(cell);
			let col = col_zone(cell);
			let field = field_zone(cell);
			// just 1 num
			let num = entry.num as usize - 1;
			//for num in entry.mask().iter() {
			zone_poss_positions[row][num] &= !row_pos;
			zone_poss_positions[col][num] &= !col_pos;
			zone_poss_positions[field][num] &= !field_pos;
			//}
		}
		*le = self.eliminated_entries.len() as _;

		for entry in &self.deduced_entries[*ld as usize ..] {
			let cell = entry.cell;
			let num = entry.num as usize - 1;

			// remove num from every zone pos in all neighbouring cells
			for &cell in neighbours(cell) {
				let row = row_zone(cell);
				let col = col_zone(cell);
				let field = field_zone(cell);
				let row_pos = Mask::row_pos_of_cell(cell);
				let col_pos = Mask::col_pos_of_cell(cell);
				let field_pos = Mask::field_pos_of_cell(cell);
				zone_poss_positions[row][num] &= !row_pos;
				zone_poss_positions[col][num] &= !col_pos;
				zone_poss_positions[field][num] &= !field_pos;
			}

			let row = row_zone(cell);
			let col = col_zone(cell);
			let field = field_zone(cell);

			let row_pos = Mask::row_pos_of_cell(cell);
			let col_pos = Mask::col_pos_of_cell(cell);
			let field_pos = Mask::field_pos_of_cell(cell);

			// remove entry pos as possible place for all nums
			for num_off in 0..9 {
				zone_poss_positions[row][num_off] &= !row_pos;
				zone_poss_positions[col][num_off] &= !col_pos;
				zone_poss_positions[field][num_off] &= !field_pos;
			}

			// remove all pos as possible place for entry num
			zone_poss_positions[row][num] = Mask::NONE;
			zone_poss_positions[col][num] = Mask::NONE;
			zone_poss_positions[field][num] = Mask::NONE;
		}
		*ld = self.deduced_entries.len() as _;
		Ok(())
	}

	#[inline(always)]
	fn insert_entries(&mut self, find_naked_singles: bool) -> Result<(), Unsolvable> {
		// code hereafter depends on this
		// but it's not necessary in general
		assert!(self.cell_poss_digits.next_deduced == self.zone_solved_digits.next_deduced);

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
		let (ld_zs, _, zone_solved_digits) = self.zone_solved_digits.get_mut();

		loop {
			if self.deduced_entries.len() <= *ld_cp as usize { break }
			let entry = self.deduced_entries[*ld_cp as usize];
			*ld_cp += 1;
			*ld_zs += 1;
			let entry_mask = entry.mask();
			// cell already solved from previous entry in stack, skip
			if cell_poss_digits[entry.cell()] == Mask::NONE { continue }

			// is entry still possible?
			if cell_poss_digits[entry.cell()] & entry_mask == Mask::NONE {
				return Err(Unsolvable);
			}

			Self::_insert_entry_cp_zs(entry, &mut self.n_solved, cell_poss_digits, zone_solved_digits);
			for &cell in neighbours(entry.cell) {
				if entry_mask & cell_poss_digits[cell as usize] != Mask::NONE {
					Self::remove_impossibilities(&mut self.grid.state, cell_poss_digits, cell, entry_mask, &mut self.deduced_entries, &mut self.employed_strategies, find_naked_singles)?;
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
		cell_poss_digits: &mut Array81<Mask<Digit>>,
		zone_solved_digits: &mut [Mask<Digit>; 27]
	) {
		*n_solved += 1;
		cell_poss_digits[entry.cell()] = Mask::NONE;
		zone_solved_digits[entry.row() as usize +ROW_OFFSET] |= entry.mask();
		zone_solved_digits[entry.col() as usize +COL_OFFSET] |= entry.mask();
		zone_solved_digits[entry.field() as usize +FIELD_OFFSET] |= entry.mask();
	}

	fn batch_insert_entries(&mut self, find_naked_singles: bool) -> Result<(), Unsolvable> {
		let (ld_cp, _, cell_poss_digits) = self.cell_poss_digits.get_mut();
		let (ld_zs, _, zone_solved_digits) = self.zone_solved_digits.get_mut();
		while self.deduced_entries.len() > *ld_cp as usize {
			let entry = self.deduced_entries[*ld_cp as usize];
			*ld_cp += 1;
			*ld_zs += 1;
			// cell already solved from previous entry in stack, skip
			if cell_poss_digits[entry.cell()] == Mask::NONE { continue }

			let entry_mask = entry.mask();

			// is entry still possible?
			// have to check zone possibilities, because cell possibility
			// is temporarily out of date
			if zone_solved_digits[entry.row() as usize + ROW_OFFSET] & entry_mask != Mask::NONE
			|| zone_solved_digits[entry.col() as usize + COL_OFFSET] & entry_mask != Mask::NONE
			|| zone_solved_digits[entry.field() as usize +FIELD_OFFSET] & entry_mask != Mask::NONE
			{
				return Err(Unsolvable);
			}

			Self::_insert_entry_cp_zs(entry, &mut self.n_solved, cell_poss_digits, zone_solved_digits);
		}

		// update cell possibilities from zone masks
		for cell in 0..81 {
			if cell_poss_digits[cell as usize] == Mask::NONE { continue }
			let zones_mask = zone_solved_digits[row_zone(cell)]
				| zone_solved_digits[col_zone(cell)]
				| zone_solved_digits[field_zone(cell)];

			Self::remove_impossibilities(&mut self.grid.state, cell_poss_digits, cell, zones_mask, &mut self.deduced_entries, &mut self.employed_strategies, find_naked_singles)?;
		}
		Ok(())
	}

	// remove impossible digits from masks for given cell
	// also check for naked singles and impossibility of sudoku
	fn remove_impossibilities(
		sudoku: &mut Sudoku,
		cell_poss_digits: &mut Array81<Mask<Digit>>,
		cell: u8,
		impossible: Mask<Digit>,
		deduced_entries: &mut Vec<Entry>,
		employed_strategies: &mut Vec<StrategyResult>,
		find_naked_singles: bool,
	) -> Result<(), Unsolvable> {
		let cell_mask = &mut cell_poss_digits[cell as usize];
		cell_mask.remove(impossible);

		if find_naked_singles {
			if let Some(num) = cell_mask.unique_num()? {
				let entry = Entry { cell, num };
				Self::push_new_entry(sudoku, deduced_entries, entry, employed_strategies, StrategyResult::NakedSingles(entry))?;
			}
		} else {
			if *cell_mask == Mask::NONE {
				return Err(Unsolvable)
			}
		}
		Ok(())
	}

	fn push_new_entry(sudoku: &mut Sudoku,
		deduced_entries: &mut Vec<Entry>,
		entry: Entry,
		employed_strategies: &mut Vec<StrategyResult>,
		strategy: StrategyResult // either naked or hidden singles
	) -> Result<(), Unsolvable> {

		#[cfg(debug_assertions)]
		{
			use self::StrategyResult::*;
			match strategy {
				NakedSingles(..) | HiddenSingles(..) => (),
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
		employed_strategies.push(strategy);
		Ok(())
	}

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	////////      Strategies
	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	fn find_naked_singles(&mut self, stop_after_first: bool) -> Result<(), Unsolvable> {
		self.update_cell_poss_zone_solved()?;

		for (cell, poss_digits) in (0..).zip(self.cell_poss_digits.state.iter()) {
			// if Err(_), then it's Mask::NONE and the cell is already solved (or impossible)
			// skip in that case (via unwrap_or(None))
			if let Some(num) = poss_digits.unique_num().unwrap_or(None) {
				let entry = Entry { cell, num };

				Self::push_new_entry(&mut self.grid.state, &mut self.deduced_entries, entry, &mut self.employed_strategies, StrategyResult::NakedSingles(entry))?;
				//Self::push_new_entry(sudoku, deduced_entries, entry, employed_strategies, StrategyResult::NakedSingles(entry))?;
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

		for zone in 0..27 {
			let mut unsolved = Mask::NONE;
			let mut multiple_unsolved = Mask::NONE;

			let cells = cells_of_zone(zone);
			for &cell in cells {
				let poss_digits = self.cell_poss_digits.state[cell as usize];
				multiple_unsolved |= unsolved & poss_digits;
				unsolved |= poss_digits;
			}
			if unsolved | self.zone_solved_digits.state[zone as usize] != Mask::ALL {
				return Err(Unsolvable);
			}

			let mut singles = unsolved.without(multiple_unsolved);
			if singles.is_empty() { continue }

			for &cell in cells {
				let mask = self.cell_poss_digits.state[cell as usize];

				if let Ok(maybe_unique) = (mask & singles).unique_num() {
					let num = maybe_unique.ok_or(Unsolvable)?;
					let entry = Entry { cell, num };
					let strat_res = StrategyResult::HiddenSingles(entry, zone_type(zone));
					Self::push_new_entry(&mut self.grid.state, &mut self.deduced_entries, entry, &mut self.employed_strategies, strat_res)?;

					// mark num as found
					singles.remove(Mask::from_num(num));

					// everything in this zone found
					// return to insert numbers immediately
					match stop_after_first {
						true => return Ok(()),
						false if singles.is_empty() => break, // continue next zone
						_ => (), // find rest of singles in zone
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

		for band in (0..6).map(Band) {
			let mut slice_poss_digits: [Mask<Digit>; 9] = [Mask::NONE; 9];

			{ // compute possible digits for each slice
			// TODO: switch to using zone_solved_digits?
				let slices = band.slices();
				for (&slice, poss_digs) in slices.iter().zip(slice_poss_digits.iter_mut()) {
					for &cell in &slice.cells() {
						*poss_digs |= cell_poss_digits[cell.0 as usize];
					}
				}
			}

			let mut line_unique_digits: [Mask<Digit>; 3] = [Mask::NONE; 3];
			let mut field_unique_digits: [Mask<Digit>; 3] = [Mask::NONE; 3];

			{
				let poss_digits = |band_line, band_field| slice_poss_digits[ band_line*3 + band_field];
				for band_line in 0..3 {
					let poss_digits_iter = (0..3)
						.map(|band_field| poss_digits(band_line, band_field) );

					let (_, _, unique) = find_unique(poss_digits_iter);
					line_unique_digits[band_line] = unique;
				}
				for band_field in 0..3 {
					let poss_digits_iter = (0..3)
						.map(|band_line| poss_digits(band_line, band_field) );;

					let (_, _, unique) = find_unique(poss_digits_iter);
					field_unique_digits[band_field] = unique;
				}
			}

			// find slices that contain the computed unique digits
			// remove them from the appropriate neighbours
			for (i, (&slice, &poss_digits)) in band.slices().iter()
				.zip(slice_poss_digits.iter())
				.enumerate()
			{
				let band_line = i / 3;
				let band_field = i % 3;

				let line_uniques =  poss_digits & line_unique_digits[band_line];
				let field_uniques = poss_digits & field_unique_digits[band_field];

				let (line_neighbours, field_neighbours) = slice.neighbours();

				let eliminated_entries = &mut self.eliminated_entries;
				let mut find_impossibles = |uniques, neighbours: &[Slice; 2]| {
					let n_eliminated = eliminated_entries.len();
					for &neighbour in neighbours {
						let conflicts = slice_poss_digits[neighbour.band_idx()] & uniques;
						if conflicts == Mask::NONE { continue }

						for &cell in neighbour.cells().iter() {
							let conflicts = cell_poss_digits[cell.0 as usize] & uniques;
							for num in conflicts.iter() {
								eliminated_entries.push( Entry { cell: cell.0, num } )
							}
						}
					}
					n_eliminated..eliminated_entries.len()
				};

				for &(uniques, neighbours) in [(line_uniques, &field_neighbours), (field_uniques, &line_neighbours)].iter()
					.filter(|&&(uniques, _)| uniques != Mask::NONE)
				{
					let rg_eliminations = find_impossibles(uniques, neighbours);
					if rg_eliminations.len() > 0 {
						// TODO: If stop_after_first is true, only enter the number whose conflicts were eliminated
						self.employed_strategies.push(StrategyResult::LockedCandidates(slice, uniques, rg_eliminations));

						if stop_after_first {
							return Ok(());
						}
					}
				}
			}
		}
		Ok(())
	}


	fn find_naked_subsets(&mut self, stop_after_first: bool) -> Result<(), Unsolvable> 	{
		// TODO: limit min/max length so naked pairs / triples / quadruples can be distinguished
		//		 breadth first search?
		fn walk_combinations(
			state: &mut StrategySolver,
			total_poss_digs: Mask<Digit>,
			cells: &[Cell],
			zone: Zone,
			stack: &mut Vec<Cell>,
			stop_after_first: bool,
		) -> bool {
			// subsets of 5 and more numbers always have complementary subsets
			// of 9 - subset_size
			if stack.len() == 5 { return false }
			if stack.len() > 0 && total_poss_digs.n_possibilities() == stack.len() as u8 {
				// found a subset
				let n_eliminated = state.eliminated_entries.len();
				for cell in zone.cells().iter().filter(|cell| !stack.contains(cell)) {
					let conflicts = state.cell_poss_digits.state[cell.0 as usize] & total_poss_digs;
					for num in conflicts.iter() {
						state.eliminated_entries.push(Entry{ cell: cell.0, num});
					}
				}
				let rg_eliminations = n_eliminated..state.eliminated_entries.len();
				if rg_eliminations.len() > 0 {
					state.employed_strategies.push(StrategyResult::NakedSubsets {
						zone,
						cells: stack.clone(),
						digits: total_poss_digs,
						conflicts: rg_eliminations
					});
					if stop_after_first {
						return true
					}
				}
			}

			for (i, &cell) in cells.iter().enumerate() {
				let cell_poss_digits = state.cell_poss_digits.state[cell.0 as usize];
				// solved cell
				if cell_poss_digits == Mask::NONE { continue }
				stack.push(cell);
				let new_total_poss_digs = total_poss_digs | cell_poss_digits;

				// if true, then a subset was found and stop_after_first is set
				// stop recursion
				if walk_combinations(state, new_total_poss_digs, &cells[i+1..], zone, stack, stop_after_first) {
					return true
				};
				stack.pop();
			}
			false
		}
		self.update_cell_poss_zone_solved()?;

		let mut stack = vec![];
		for zone in (0..27).map(Zone) {
			if self.zone_solved_digits.state[zone.0 as usize] == Mask::ALL { continue }
			let cells = zone.cells();
			// if true, then a subset was found and stop_after_first is set
			// stop looking
			if walk_combinations(self, Mask::NONE, &cells, zone, &mut stack, stop_after_first) {
				break
			};
		}
		Ok(())
	}

	fn find_hidden_subsets(&mut self, stop_after_first: bool) -> Result<(), Unsolvable> {
		// TODO: limit min/max length so hidden pairs / triples / quadruples can be distinguished
		//		 breadth first search?
		fn walk_combinations(
			state: &mut StrategySolver,
			zone: u8,
			total_poss_pos: Mask<Position>,
			num_offs: &[u8],
			all_num_offs: &[u8; 9],
			stack: &mut Vec<u8>,
			stop_after_first: bool,
		) -> bool {
			if stack.len() == 5 { return false }
			let zone_poss_positions = state.zone_poss_positions.state[zone as usize];

			if stack.len() > 0 && total_poss_pos.n_possibilities() == stack.len() as u8 {

				let n_eliminated = state.eliminated_entries.len();
				for &num_off in all_num_offs.iter().filter(|num_off| !stack.contains(num_off)) {
					let conflicts = zone_poss_positions[num_off as usize] & total_poss_pos;
					for pos in conflicts.iter() {
						let Cell(cell) = Cell::from_zone_pos(Zone(zone), pos);
						state.eliminated_entries.push(Entry{ cell, num: num_off + 1});
					}
				}
				let rg_eliminations = n_eliminated..state.eliminated_entries.len();
				if rg_eliminations.len() > 0 {
					state.employed_strategies.push(StrategyResult::HiddenSubsets {
						zone: Zone(zone),
						num_offsets: stack.clone(),
						positions: total_poss_pos,
						conflicts: rg_eliminations
					});
					if stop_after_first {
						return true
					}
				}
			}

			for (i, &num_off) in num_offs.iter().enumerate() {
				let num_poss_pos = zone_poss_positions[num_off as usize];
				// solved cell
				if num_poss_pos == Mask::NONE { continue }
				stack.push(num_off);
				let new_total_poss_pos = total_poss_pos | num_poss_pos;
				if walk_combinations(state, zone, new_total_poss_pos, &num_offs[i+1..], all_num_offs, stack, stop_after_first) {
					return true
				};
				stack.pop();
			}
			false
		}

		self.update_zone_poss_positions()?;

		let mut stack = vec![];
		for zone in 0..27 {
			if self.zone_solved_digits.state[zone as usize] == Mask::ALL { continue }
			let num_offs = [0, 1, 2, 3, 4, 5, 6, 7, 8];
			if walk_combinations(self, zone, Mask::NONE, &num_offs, &num_offs, &mut stack, stop_after_first) {
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
		for num_off in 0..9 {
			// 0..9 = rows, 9..18 = cols
			for lines in &[Line::ALL_ROWS, Line::ALL_COLS] {
				if basic_fish_walk_combinations(self, num_off, max_size, &mut stack, lines, lines, Mask::NONE, stop_after_first) {
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

        fn follow_links(num_off: u8, cell: Cell, is_a: bool, sudoku: &StrategySolver, cell_color: &mut [Colour; 81], link_nr: u8, cell_linked: &mut [u8; 81]) {
            if cell_linked[cell.0 as usize] <= link_nr { return }

            for &(con_zone, current_pos) in &[
                (cell.row().zone(), Position::in_row_of_cell(cell.0)),
                (cell.col().zone(), Position::in_col_of_cell(cell.0)),
                (cell.field().zone(), Position::in_field_of_cell(cell.0)),
            ] {
                let zone_poss_positions = sudoku.zone_poss_positions.state[con_zone.0 as usize][num_off as usize];
                if zone_poss_positions.n_possibilities() == 2 {
                    let other_pos = (zone_poss_positions & !Mask::from_pos(current_pos.0)).one_possibility();
                    let other_cell = Cell::from_zone_pos(con_zone, other_pos);

                    match cell_linked[other_cell.0 as usize] <= link_nr {
                        true => continue,
                        false => cell_linked[other_cell.0 as usize] = link_nr,
                    };

                    cell_color[other_cell.0 as usize] = if is_a { Colour::A } else { Colour::B };

                    follow_links(num_off, other_cell, !is_a, sudoku, cell_color, link_nr, cell_linked);
                }
            }
        }

        for num_off in 0..9 {
            let mut cell_touched = [false; N_CELLS];
            let mut link_nr = 0;

            let mut cell_linked = [0; 81];
            let mut cell_color = [Colour::Uncoloured; 81];

            for zone in (0..27).map(Zone) {
                let zone_poss_positions = self.zone_poss_positions.state[zone.0 as usize][num_off as usize];
                if zone_poss_positions.n_possibilities() == 2 {
                    let first = zone_poss_positions.one_possibility();
                    let cell = Cell::from_zone_pos(zone, first);

                    match cell_touched[cell.0 as usize] {
                        true => continue,
                        false => cell_touched[cell.0 as usize] = true,
                    };

                    follow_links(num_off, cell, true, self, &mut cell_color, link_nr, &mut cell_linked);
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
                for zone in (0..27).map(Zone) {
                    // Collect colours in this link chain and this zone
                    let mut zone_colors = [Colour::Uncoloured; 9];
                    for (pos, &cell) in zone.cells()
                        .iter()
                        .enumerate()
                        .filter(|c| cell_linked[c.0 as usize] == link_nr)
                    {
                        zone_colors[pos] = cell_color[cell.0 as usize];
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
                    mark_impossible(num_off+1, link_nr, impossible_colour, cell_color, cell_linked, &mut self.eliminated_entries);
                    // chain handled, go to next
                    // note: as this eagerly marks a colour impossible as soon as a double in any colour is found
                    //       a case of two doubles in some later zone will not always be found
                    //       impossibility is then detected further down the strategy chain
                    break
                }

                // ===== Rule 2 =====
                let mut cell_sees_colour = [(false, false); 81];
                for ((cell, &cell_colour), _) in (0..81).map(Cell).
                    zip(cell_color.iter())
                    .zip(cell_linked.iter())
                    .filter(|&((_, &cell_colour), &cell_link_nr)| link_nr == cell_link_nr && cell_colour != Colour::Uncoloured)
                {
                    for &zone in &cell.zones() {
                        for &neighbour_cell in zone.cells().iter().filter(|&&c| cell != c) {
                            let (sees_a, sees_b) = cell_sees_colour[neighbour_cell.0 as usize];
                            if cell_colour == Colour::A && !sees_a {
                                cell_sees_colour[neighbour_cell.0 as usize].0 = true;
                                if sees_b {
                                    self.eliminated_entries.push( Entry{ cell: neighbour_cell.0, num: num_off+1 })
                                }
                            } else if cell_colour == Colour::B && !sees_b {
                                cell_sees_colour[neighbour_cell.0 as usize].1 = true;
                                if sees_a {
                                    self.eliminated_entries.push( Entry{ cell: neighbour_cell.0, num: num_off+1 })
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
	num_off: usize,
	goal_depth: usize,
	stack: &mut Vec<Line>,
	lines: &[Line],
	all_lines: &[Line; 9],
	union_poss_pos: Mask<Position>,
	stop_after_first: bool,
) -> bool {
	if stack.len() == goal_depth {
		// nothing of interest found
		if union_poss_pos.n_possibilities() != goal_depth as u8 { return false }
		// found xwing, swordfish, jellyfish, whatever-the-name
		let n_eliminated = sudoku.eliminated_entries.len();
		for line in all_lines.iter().filter(|&line| !stack.contains(line)) {
			for pos in union_poss_pos.iter() {
				let cell = Cell::from_zone_pos(line.zone(), pos);
				let cell_mask = sudoku.cell_poss_digits.state[cell.0 as usize];
				if cell_mask & Mask::from_num(num_off as u8 +1) != Mask::NONE {
					sudoku.eliminated_entries.push(Entry{ num: num_off as u8 +1, cell: cell.0 });
				}
			}
		}

		let rg_eliminations = n_eliminated..sudoku.eliminated_entries.len();
		if rg_eliminations.len() > 0 {

			let lines = stack.clone();
			let positions = union_poss_pos;
			let num = num_off as u8 + 1;
			let conflicts = rg_eliminations;

			sudoku.employed_strategies.push( match goal_depth {
				2 => StrategyResult::XWing {
					lines, positions, num, conflicts
				},
				3 => StrategyResult::Swordfish {
					lines, positions, num, conflicts
				},
				4 => StrategyResult::Jellyfish {
					lines, positions, num, conflicts
				},
				_ => unreachable!(),
			});
			if stop_after_first {
				return true
			}
		}
	}
	for (i, &line) in lines.iter().enumerate() {
		let possible_pos = sudoku.zone_poss_positions.state[line.0 as usize][num_off];
		let n_poss = possible_pos.n_possibilities();
		let new_union_poss_pos = union_poss_pos | possible_pos;

		// n_poss == 0 => solved row (or impossible)
		// n_poss == 1 => hidden single
		if n_poss < 2 || new_union_poss_pos.n_possibilities() > goal_depth as u8 { continue }
		stack.push(line);
		if basic_fish_walk_combinations(sudoku, num_off, goal_depth, stack, &lines[i+1..], all_lines, new_union_poss_pos, stop_after_first) {
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
    NakedSubsets,
    HiddenSubsets,
    XWing,
    Swordfish,
    Jellyfish,
    SinglesChain,
    #[doc(hidden)] __NonExhaustive
}

impl Strategy {
	// is_first_strategy is an optimization hint
	// it doesn't need to be used
	fn deduce(&self, state: &mut StrategySolver, stop_after_first: bool, is_first_strategy: bool) -> Result<(), Unsolvable> {
		use self::Strategy::*;
        match *self {
            NakedSingles if !stop_after_first && is_first_strategy => state._update_cell_poss_zone_solved(true),
			NakedSingles => state.find_naked_singles(stop_after_first),
			HiddenSingles => state.find_hidden_singles(stop_after_first),
			LockedCandidates => state.find_locked_candidates(stop_after_first),
			NakedSubsets => state.find_naked_subsets(stop_after_first),
			HiddenSubsets => state.find_hidden_subsets(stop_after_first),
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum ZoneType {
	Row,
	Col,
	Block,
}

#[derive(Debug, Clone)]
pub(crate) enum LineType {
	Row,
	Col,
}

#[derive(Debug, Clone, Copy, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub enum DeductionResult<'a> {
	Forced(Entry),
	Eliminated(&'a [Entry]),
}

#[derive(Debug)]
/// Result of a single strategy
pub struct Deduction<'a> {
	deduction: &'a StrategyResult,
	deduced_entries: &'a [Entry],
	eliminated_entries: &'a [Entry]
}

impl<'a> Deduction<'a> {
	/// Returns the strategy that was used for this deduction.
	pub fn strategy(&self) -> Strategy {
		self.deduction.strategy()
	}

	/// Returns the entries that were entered or ruled out as a result of this strategy application
	pub fn results(&self) -> DeductionResult {
		self.deduction.deductions(&self.eliminated_entries)
	}
}

#[derive(Debug)]
/// Contains the sequence of deductions made to solve / partially solve the sudoku
pub struct Deductions {
	employed_strategies: Vec<StrategyResult>,
	deduced_entries: Vec<Entry>,
	eliminated_entries: Vec<Entry>,
}

pub struct DeductionsIter<'a> {
	employed_strategies: ::std::slice::Iter<'a, StrategyResult>,
	deduced_entries: &'a [Entry],
	eliminated_entries: &'a [Entry]
}

impl<'a> Iterator for DeductionsIter<'a> {
	type Item = Deduction<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		self.employed_strategies.next()
			.map(|strategy_result|
				Deduction {
					deduction: strategy_result,
					deduced_entries: &self.deduced_entries,
					eliminated_entries: &self.eliminated_entries,
				}
			)
	}
}

impl Deductions {
	fn print_deductions(&self) {
		for strategy_result in &self.employed_strategies {
			print!("{:25?}: ", strategy_result.strategy());
			strategy_result.print_reason();
			print!("\n");
			match strategy_result.deductions(&self.eliminated_entries) {
				DeductionResult::Forced(entry) => println!("\tr{}c{} {}", entry.row()+1, entry.col()+1, entry.num),

				DeductionResult::Eliminated(deductions) => {
					for &entry in deductions {
						println!("\tr{}c{} {}", entry.row()+1, entry.col()+1, entry.num);
					}
				}

			}
		}
	}
}

// TODO:
// Expand and clean up
// This should ultimately be a public type exposing all the necessary information on how the deduction was made
// and with methods that can generate a sequence of explanation steps including
// highlighting of cells / houses / cell possibilities, their removal and cell-to-cell chains
// Basically, the GUI should only need to give the basic highlighting operations and we generate the explanation
#[derive(Debug, Clone)]
pub(crate) enum StrategyResult {
    NakedSingles(Entry),
    HiddenSingles(Entry, ZoneType),
    LockedCandidates(Slice, Mask<Digit>, DeductionRange), // which slice is affected and what's unique
    NakedSubsets {
		zone: Zone,
		cells: Vec<Cell>,     // max 4
		digits: Mask<Digit>,  // digits restricted to cells
		conflicts: DeductionRange, // link to impossible entries
	},
    HiddenSubsets {
		zone: Zone,
		num_offsets: Vec<u8>,       // max 4
		positions: Mask<Position>,  // positions restricted to num_offsets
		conflicts: DeductionRange,       // link to impossible entries
	},
    XWing {
		lines: Vec<Line>, // 2 lines, TODO: refactor
		positions: Mask<Position>, // which positions in all lines
		num: u8,
		conflicts: DeductionRange,
	},
    Swordfish {
		lines: Vec<Line>, // 3 lines, TODO: refactor
		positions: Mask<Position>, // which positions in all lines
		num: u8,
		conflicts: DeductionRange,
	},
    Jellyfish {
		lines: Vec<Line>, // 4 lines, TODO: refactor
		positions: Mask<Position>, // which positions in all lines
		num: u8,
		conflicts: DeductionRange,
	},

	// TODO: expand information in variants below
    SinglesChain(DeductionRange),
    #[doc(hidden)] __NonExhaustive
}

macro_rules! map_to_strategy {
	($this:expr; $($variant:ident),*) => {
		match *$this {
			$(
				$variant {..} => Strategy::$variant
			),*
		}
	}
}

impl StrategyResult {
	fn strategy(&self) -> Strategy {
		use self::StrategyResult::*;
		// for each strategy: StrategyName{..} => Strategy::StrategyName
		map_to_strategy!( self;
			NakedSingles, HiddenSingles, LockedCandidates, NakedSubsets, HiddenSubsets,
			XWing, Swordfish, Jellyfish, SinglesChain, __NonExhaustive
		)
	}

	// not really an error, I just want an Either
	fn deductions<'e>(&self, eliminated: &'e [Entry]) -> DeductionResult<'e> {
		use self::StrategyResult::*;
		match self {
			NakedSingles(entry) | HiddenSingles(entry, _) => DeductionResult::Forced(*entry),
			LockedCandidates(.., conflicts) |
			NakedSubsets { conflicts, .. } |
			HiddenSubsets { conflicts, .. } |
			XWing { conflicts, .. } |
			Swordfish { conflicts, .. } |
			Jellyfish { conflicts, .. } |
			SinglesChain(conflicts)

				=> DeductionResult::Eliminated(&eliminated[conflicts.clone()]),

			__NonExhaustive => unreachable!(),
		}
	}

	fn print_reason(&self) {
		use self::StrategyResult::*;
		match *self {
			NakedSingles(entry) => print!("r{}c{} {}", entry.row()+1, entry.col()+1, entry.num),
			HiddenSingles(entry, zone_type) => {
				print!("r{}c{} {} {:?}", entry.row()+1, entry.col()+1, entry.num, zone_type);
			},
			LockedCandidates(slice, mask, _) => {
				let nums = mask.iter().map(|n| n.to_string()).collect::<Vec<_>>();
				print!("slice {} nums: {}", slice.0, nums.join(","))
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

				StrategyResult::NakedSubsets {
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
}


#[inline]
fn find_unique<I: Iterator<Item=Mask<Digit>>>(possibilities: I) -> (Mask<Digit>, Mask<Digit>, Mask<Digit>) {
	let mut unsolved = Mask::NONE;
	let mut multiple_unsolved = Mask::NONE;

	for poss_digits in possibilities {
		multiple_unsolved |= unsolved & poss_digits;
		unsolved |= poss_digits;
	}
	// >= 1, >1, =1 occurences
	(unsolved, multiple_unsolved, unsolved & !multiple_unsolved)
}

#[cfg(test)]
mod test {
    extern crate test;
    use super::*;
    fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    sudokus_str.lines()
            .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
            .collect()
    }

    fn all_strategies() -> Vec<Strategy> {
		use super::Strategy::*;
        vec![
            NakedSingles,
            HiddenSingles,
            LockedCandidates,
            NakedSubsets,
            HiddenSubsets,
            XWing,
            Swordfish,
            Jellyfish,
            //SinglesChain,
        ]
    }


    fn strategy_solver_correct_solution<F>(sudokus: Vec<Sudoku>, solved_sudokus: Vec<Sudoku>, solver: F)
        where F: Fn(StrategySolver, &[Strategy]) -> Result<(Sudoku, Deductions), (Sudoku, Deductions)>,
    {
        let strategies = all_strategies();
        let mut unsolved = vec![];
        for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
            let cache = StrategySolver::from_sudoku(sudoku);
            match solver(cache, &strategies) {
                Ok((solution, _deductions)) => assert_eq!(solution, solved_sudoku),
                Err((part_solved, _deductions)) => unsolved.push((i, sudoku, part_solved, solved_sudoku)),
            }
        }
        if unsolved.len() != 0 {
            println!("Could not solve {} sudokus:\n", unsolved.len());

            for (i, sudoku, part_solution, _solution) in unsolved {
            	println!("\nsudoku nr {}:\n{}\n{}\n{}", i+1, sudoku.to_str_line(), part_solution.to_str_line(), _solution.to_str_line());
            }
            panic!();
        }
    }


	/*
	#[test]
	fn strategy_solve() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_easy_sudokus.txt") );
		let strategies = all_strategies();
		let sudoku = sudokus[5];
		println!("{}\n", sudoku);
		let solved = StrategySolver::from_sudoku(sudoku).solve(&strategies).unwrap();
	}
	*/

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

	/*
    #[test]
    fn strategy_solver_correct_solution_hard_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/hard_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_hard_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve);
    }
	*/

	/*
    #[test]
    fn backtracking_strategy_solver_correct_solution_easy_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_easy_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve_with_backtracking);
    }

    #[test]
    fn backtracking_strategy_solver_correct_solution_medium_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/medium_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_medium_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve_with_backtracking);
    }

    #[test]
    fn backtracking_strategy_solver_correct_solution_hard_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/hard_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_hard_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve_with_backtracking);
    }
	*/

    #[bench]
    fn easy_sudokus_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                StrategySolver::from_sudoku(sudoku).solve(&strategies).unwrap(); //.unwrap();
            }
        })
    }

    #[bench]
    fn medium_sudokus_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/medium_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                // solution not guaranteed yet, discard error.
                let _ = StrategySolver::from_sudoku(sudoku).solve(&strategies); //.unwrap();
            }
        })
    }
	/*
	#[bench]
    fn easy_sudokus_backtracking_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                StrategySolver::from_sudoku(sudoku).solve_with_backtracking(&strategies).unwrap();
            }
        })
    }

    #[bench]
    fn medium_sudokus_backtracking_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/medium_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                // solution not guaranteed yet, discard error.
                let _ = StrategySolver::from_sudoku(sudoku).solve_with_backtracking(&strategies).unwrap();
            }
        })
    }
	*/
}

fn zone_type(zone: u8) -> ZoneType {
	use self::ZoneType::*;
	match zone {
		0...8 => Row,
		9...17 => Col,
		18...26 => Block,
		_ => unreachable!(),
	}
}

fn join_iter<T: Iterator>(t: T) -> String
	where T::Item: ::std::fmt::Display,
{
	t.map(|n| n.to_string()).collect::<Vec<_>>().join(",")
}
