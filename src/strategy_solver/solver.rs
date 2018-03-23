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
//use super::NewStrategy;

// State of Sudoku
// Contains caches of various properties of the grid
// so that arbitrary strategies can be applied efficiently without
// having to scan the entire grid beforehand
//
// Strategies are structs that implement the trait Strategy
// they can access the caches and return deductions
// which can be applied to the State struct through a common interface
// that will keep the caches consistent.
#[derive(Debug, Clone)]
pub struct StrategySolver {
	pub(crate) grid: Sudoku,
	pub(crate) n_solved_cells: u8,
	pub(crate) cell_poss_digits: Array81<Mask<Digit>>,
	pub(crate) zone_solved_digits: [Mask<Digit>; 27],
	pub(crate) zone_poss_positions: [[Mask<Position>; 9]; 27],
	pub(crate) last_cell: u8, // last cell checked in guess routine
}

// This is the link to which links the
/*
#[derive(Debug, Clone)]

pub enum Deduction {
	Forced(::std::ops::Range<usize>),
	Impossible(::std::ops::Range<usize>),
}
use self::Deduction::*;
*/

type DeductionRange = ::std::ops::Range<usize>;

#[derive(Debug, Clone)]
pub(crate) struct SudokuState {
	// keeps a history of all deductions, both filled cells and eliminated possibilities
	// as well as what reasoning was used
	pub employed_strategies: Vec<StrategyResult>,
	pub deduced_entries: Vec<Entry>,
	pub eliminated_entries: Vec<Entry>,
	pub n_solved: u8,
	// current state of the sudoku
	// for when it's faster to recompute from the end state
	// than update through the new entries
	// TODO: lazily updated?
	pub grid: Sudoku,
	// TODO: combine states that are updated together
	pub(crate) cell_poss_digits: State<Array81<Mask<Digit>>>,
	pub(crate) zone_solved_digits: State<[Mask<Digit>; 27]>,
	pub(crate) zone_poss_positions: State<[[Mask<Position>; 9]; 27]>,
}

impl SudokuState {
	pub fn from_sudoku(sudoku: Sudoku) -> SudokuState {
		let deduced_entries = sudoku.iter()
			.enumerate()
			.filter_map(|(cell, opt_num)| {
				opt_num.map(|num| Entry { cell: cell as u8, num })
			}).collect();
		SudokuState {
			employed_strategies: vec![],
			deduced_entries,
			eliminated_entries: vec![],
			n_solved: 0,
			grid: Sudoku([0; 81]),
			cell_poss_digits: State::from(Array81([Mask::ALL; 81])),
			zone_solved_digits: State::from([Mask::NONE; 27]),
			zone_poss_positions: State::from([[Mask::ALL; 9]; 27]),
		}

	}

	fn update_grid(&mut self) {
		for &Entry { cell, num } in &self.deduced_entries {
			self.grid.0[cell as usize] = num;
		}
	}

	pub fn solve(mut self, strategies: &[Strategy]) -> Result<Sudoku, Sudoku> {
		'outer: loop {
			if self.is_solved() {
				self.update_grid();
				//println!("WTF: len: {}, deductions: {}, grid: {}", self.deduced_entries.len(), self.employed_strategies.len(), self.grid.to_str_line());
				return Ok(self.grid)
			}

			// no chance without strategies
			let n_deductions = self.deduced_entries.len();
			let n_eliminated = self.eliminated_entries.len();
			// normally this is .ok_or()?
			let (first, rest) = match strategies.split_first().ok_or(Unsolvable) {
				Ok(tup) => tup,
				Err(_) => break,
			};
			if first.deduce_all(&mut self).is_err() { break };
			if self.deduced_entries.len() > n_deductions {
				continue 'outer
			}

			for strategy in rest {
				if strategy.deduce_one(&mut self).is_err() {
					break;
				};
				if self.deduced_entries.len() > n_deductions || self.eliminated_entries.len() > n_eliminated {
					continue 'outer
				}
			}
			//self.update_grid();
			break // Err(self.grid)
		}
		self.update_grid();
		println!("got this far: {}", self.grid);
		for strategy_result in self.employed_strategies {

			print!("{:25?}: ", strategy_result.strategy());
			strategy_result.print_reason();
			print!("\n");
			match strategy_result.deductions(&self.eliminated_entries) {
				Ok(entry) => println!("\tr{}c{} {}", entry.row()+1, entry.col()+1, entry.num),

				Err(deductions) => {
					for &entry in deductions {
						println!("\tr{}c{} {}", entry.row()+1, entry.col()+1, entry.num);
					}
				}

			}

			/*
			let deductions = match strategy.result {
				Forced(rg) => &self.deduced_entries[rg],
				Impossible(rg) => &self.eliminated_entries[rg],
			};
			for &entry in deductions {
				println!("\tr{}c{} {}", entry.row()+1, entry.col()+1, entry.num);
			}
			*/
		//println!("{:?}", self.employed_strategies);
		}
		assert!(self.zone_poss_positions.last_eliminated == self.eliminated_entries.len() as _);
		assert!(self.zone_poss_positions.next_deduced == self.deduced_entries.len() as _);
		println!("positions for 4 and 9 in col 7:");
		for zone in 0..27 {
			println!("{:?} {}", zone_type(zone as _), 1 + zone % 9 );
			for num in 0..9 {
				let poss_pos = join_iter(self.zone_poss_positions.state[zone][num].iter().map(|pos| pos.0 + 1));
				println!("\t{}: {}", num + 1, poss_pos);

				//println!("4: {}", join_iter(self.zone_poss_positions.state[7+COL_OFFSET - 1][4 - 1].iter().map(|pos| pos.0 + 1)));
				//println!("9: {}", join_iter(self.zone_poss_positions.state[7+COL_OFFSET - 1][9 - 1].iter().map(|pos| pos.0 + 1)));

			}
		}


		Err(self.grid)
	}

	pub fn is_solved(&self) -> bool {
		self.n_solved == 81
	}

	fn update_cell_poss_zone_solved(&mut self) -> Result<(), Unsolvable> {
		{
			let (_, le_cp, cell_poss) = self.cell_poss_digits.get_mut();

			let mut dummy = vec![];
			for &entry in &self.eliminated_entries[*le_cp as _..] {
				let impossibles = Mask::from_num(entry.num());

				// deductions made here may conflict with entries already in the queue
				// in the queue. In that case the sudoku is impossible.
				Self::remove_impossibilities(&mut self.grid, cell_poss, entry.cell, impossibles, &mut self.deduced_entries, &mut dummy, &mut self.employed_strategies)?;
				//let cell_mask = &mut cell_poss[entry.cell as usize];
				//*cell_mask &= !impossibles;
				//if *cell_mask == Mask::NONE {
				//	return Err(Unsolvable)
				//}
			}
			assert!(dummy.len() == 0);
			*le_cp = self.eliminated_entries.len() as _;
		}

		self.insert_entries()
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
	fn insert_entries(&mut self) -> Result<(), Unsolvable> {
		// code hereafter depends on this
		// but it's not necessary in general
		assert!(self.cell_poss_digits.next_deduced == self.zone_solved_digits.next_deduced);

		// TODO: Delete?
		// start off with batch insertion so every cell is visited at least once
		// because other strategies may have touched their possibilities which singly_insertion may miss
		self.batch_insert_entries()?;
		loop {
			match self.deduced_entries.len() - self.cell_poss_digits.next_deduced as usize {
				0 => break Ok(()),
				1...4 => self.insert_entries_singly()?,
				_ => self.batch_insert_entries()?,
			}
		}
	}

	// for each entry in the stack, insert it (if cell is unsolved)
	// and then remove possibility from each cell neighbouring it in all
	// zones (rows, cols, fields) eagerly
	// check for naked singles and impossible cells during this check
	fn insert_entries_singly(&mut self) -> Result<(), Unsolvable> {
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
					Self::remove_impossibilities(&mut self.grid, cell_poss_digits, cell, entry_mask, &mut self.deduced_entries, &mut self.eliminated_entries, &mut self.employed_strategies)?;
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
		// Hidden singles can do it, too
		//employed_strategies.push(Strategy::NakedSingles);
		*n_solved += 1;
		cell_poss_digits[entry.cell()] = Mask::NONE;
		zone_solved_digits[entry.row() as usize +ROW_OFFSET] |= entry.mask();
		zone_solved_digits[entry.col() as usize +COL_OFFSET] |= entry.mask();
		zone_solved_digits[entry.field() as usize +FIELD_OFFSET] |= entry.mask();
	}

	pub fn batch_insert_entries(&mut self) -> Result<(), Unsolvable> {
		let (ld_cp, _, cell_poss_digits) = self.cell_poss_digits.get_mut();
		let (ld_zs, _, zone_solved_digits) = self.zone_solved_digits.get_mut();
		//while self.deduced_entries.len() > *ld_cp as usize {
		loop {
			if self.deduced_entries.len() <= *ld_cp as usize { break }
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

			Self::remove_impossibilities(&mut self.grid, cell_poss_digits, cell, zones_mask, &mut self.deduced_entries, &mut self.eliminated_entries, &mut self.employed_strategies)?;
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
		eliminated_entries: &mut Vec<Entry>,
		employed_strategies: &mut Vec<StrategyResult>,
	) -> Result<(), Unsolvable> {
		let cell_mask = &mut cell_poss_digits[cell as usize];
		cell_mask.remove(impossible);

		/*
		eliminated_entries.extend(
			impossible.iter()
				.map(|num| Entry { cell, num })
		);
		*/
		/*
		for num in cell_mask.iter() {
			eliminated_entries.push( Entry { cell, num });
		}*/

		if let Some(num) = cell_mask.unique_num()? {
			let entry = Entry { cell, num };
			Self::push_new_entry(sudoku, deduced_entries, entry, employed_strategies, StrategyResult::NakedSingles(entry))?;
			//employed_strategies.push(Strategy::NakedSingles);
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
				_ => panic!("Called push_new_entry with wrong strategy type")
			};
		}


		let old_num = &mut sudoku.0[entry.cell()];
		if *old_num == entry.num {
			return Ok(())
		} else if *old_num != 0 {
			return Err(Unsolvable)
		}
		*old_num = entry.num;
		deduced_entries.push(entry);
		employed_strategies.push(strategy);
		//let pos = deduced_entries.len();
		//employed_strategies.push((strategy, Forced(pos..pos+1) ));
		Ok(())
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
					Self::push_new_entry(&mut self.grid, &mut self.deduced_entries, entry, &mut self.employed_strategies, strat_res)?;
					//self.deduced_entries.push(Entry{ cell: cell, num: num } );
					//self.employed_strategies.push(Strategy::HiddenSingles);

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

   				if stop_after_first {
					for &(uniques, neighbours) in &[(line_uniques, &field_neighbours), (field_uniques, &line_neighbours)] {
						let rg_eliminations = find_impossibles(uniques, neighbours);
						if rg_eliminations.len() > 0 {
							self.employed_strategies.push(StrategyResult::LockedCandidates(slice, uniques, rg_eliminations));
							return Ok(());
						}
					}
					/*
					let (uniques, neighbours) = if line_uniques != Mask::NONE {
						(line_uniques, &field_neighbours)
					} else if field_uniques != Mask::NONE {
						(field_uniques, &line_neighbours)
					} else {
						continue
					};
					let rg_eliminations = find_impossibles(uniques, neighbours);
					if rg_eliminations.len() > 0 {
						self.employed_strategies.push(StrategyResult::LockedCandidates(slice, uniques, rg_eliminations));
						return Ok(());
					}
					*/
				} else {
					if line_uniques != Mask::NONE {
						let rg_eliminations = find_impossibles(line_uniques, &field_neighbours);
						if rg_eliminations.len() > 0 {
							self.employed_strategies.push(StrategyResult::LockedCandidates(slice, line_uniques, rg_eliminations));
						}
					}

					if field_uniques != Mask::NONE {
						let rg_eliminations = find_impossibles(field_uniques, &line_neighbours);
						if rg_eliminations.len() > 0 {
							self.employed_strategies.push(StrategyResult::LockedCandidates(slice, field_uniques, rg_eliminations));
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
			state: &mut SudokuState,
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
			state: &mut SudokuState,
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
					//panic!("found fish");
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

        fn follow_links(num_off: u8, cell: Cell, is_a: bool, sudoku: &SudokuState, cell_color: &mut [Colour; 81], link_nr: u8, cell_linked: &mut [u8; 81]) {
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
	sudoku: &mut SudokuState,
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
			//panic!("fishy!");

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
		//panic!("fishyness");
		stack.push(line);
		if basic_fish_walk_combinations(sudoku, num_off, goal_depth, stack, &lines[i+1..], all_lines, new_union_poss_pos, stop_after_first) {
			return true
		};
		stack.pop();
	}
	false
}


#[derive(Debug, Clone)]
pub struct State<T> {
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

// tokens handed out when updating
//struct CellPossibilities;
//struct ZoneSolvedDigits;
//struct ZonePossibilities;

impl<T> State<T> {
	fn get_mut(&mut self) -> (&mut u16, &mut u16, &mut T) {
		let &mut State {
			next_deduced: ref mut ld, last_eliminated: ref mut le, ref mut state
		} = self;
		(ld, le, state)
	}
}
/*
fn find_naked_singles(state: &mut SudokuState) {
	let (ld_cp, le_cp, cell_poss) = state.cell_poss_digits.get_mut();
	let (ld_zp, le_zp, zone_solved) = state.zone_solved_digits.get_mut();

	for &entry in &impossibles[*le as usize + 1 ..] {
		let impossibles = Mask::from_num(entry.num());
		let cell_mask = &mut poss_digits[entry.cell as usize];
		*cell_mask &= !impossibles;
		if *cell_mask == Mask::NONE {
			return Err(Unsolvable)
		}
	}
	*le = impossibles.len() as _;



	unimplemented!()
}*/

/*
impl CellPossibilities {
	fn get_as_is(&self) -> Array81

	// update state, deduce singles during that
	// then return array
	// TODO: how to pass info about number of deductions
	fn deduce_get(&mut self, entries: &mut Vec<Entry>, impossibles: &mut Vec<Entry>, _: Sudoku) -> Result<Array81<Mask<Digit>>, Unsolvable> {
		let &mut CellPossibilities(State {
			next_deduced: ref mut ld, last_eliminated: ref mut le, state: ref mut poss_digits
		}) = self;

		for &entry in &impossibles[*le as usize + 1 ..] {
			let impossibles = Mask::from_num(entry.num());
			let cell_mask = &mut poss_digits[entry.cell as usize];
			*cell_mask &= !impossibles;
			if *cell_mask == Mask::NONE {
				return Err(Unsolvable)
			}
		}
		*le = impossibles.len() as _;



		unimplemented!()
	}
}
*/
/*
impl StrategySolver {
	fn new() -> StrategySolver {
		StrategySolver {
			grid: Sudoku([0; 81]),
			n_solved_cells: 0,
			cell_poss_digits: Array81([Mask::ALL; 81]),
			zone_solved_digits: [Mask::NONE; 27],
			zone_poss_positions: [[Mask::ALL; 9]; 27],
			last_cell: 0,
		}
	}

	pub fn from_sudoku(sudoku: Sudoku) -> Result<StrategySolver, Unsolvable> {
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
		let old_cell_possibilities = self.cell_poss_digits[entry.cell()];
		self.cell_poss_digits[entry.cell()] = Mask::NONE;

		self.zone_solved_digits[entry.row() as usize +ROW_OFFSET] |= entry.mask();
		self.zone_solved_digits[entry.col() as usize +COL_OFFSET] |= entry.mask();
		self.zone_solved_digits[entry.field() as usize +FIELD_OFFSET] |= entry.mask();

		// FIXME: Abstract away, this is used twice
		let cell = entry.cell;
		let row_pos = Mask::row_pos_of_cell(cell);
		let col_pos = Mask::col_pos_of_cell(cell);
		let field_pos = Mask::field_pos_of_cell(cell);
		let row = row_zone(cell);
		let col = col_zone(cell);
		let field = field_zone(cell);
		let impossible = old_cell_possibilities; // & !Mask::from_num(entry.num());
		for num in impossible.iter() {
			self.zone_poss_positions[row][num as usize - 1] &= !row_pos;
			self.zone_poss_positions[col][num as usize - 1] &= !col_pos;
			self.zone_poss_positions[field][num as usize - 1] &= !field_pos;
		}
	}

	//fn batch_insert_entries(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
	fn insert_entries(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		for entry in stack.drain(..) {
			// cell already solved from previous entry in stack, skip
			if self.cell_poss_digits[entry.cell()] == Mask::NONE { continue }

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
			if self.cell_poss_digits[cell as usize] == Mask::NONE { continue }
			let zones_mask = self.zone_solved_digits[row_zone(cell)]
				| self.zone_solved_digits[col_zone(cell)]
				| self.zone_solved_digits[field_zone(cell)];

			self.remove_cell_impossibilities(cell, zones_mask)?;
		}
		Ok(())
	}

	#[inline]
	fn is_solved(&self) -> bool {
		self.n_solved_cells == 81
	}

	// remove impossible digits from masks for given cell
	fn remove_cell_impossibilities(&mut self, cell: u8, impossible: Mask<Digit>) -> Result<(), Unsolvable> {
		let cell_mask = &mut self.cell_poss_digits[cell as usize];
		// *cell_mask &= !impossible;
		//if *cell_mask == Mask::NONE {
		//	return Err(Unsolvable)
		//}

		{ // remove now impossible positions from list
			let row_pos = Mask::row_pos_of_cell(cell);
			let col_pos = Mask::col_pos_of_cell(cell);
			let field_pos = Mask::field_pos_of_cell(cell);
			let row = row_zone(cell);
			let col = col_zone(cell);
			let field = field_zone(cell);
			for num in impossible.iter() {
				self.zone_poss_positions[row][num as usize - 1] &= !row_pos;
				self.zone_poss_positions[col][num as usize - 1] &= !col_pos;
				self.zone_poss_positions[field][num as usize - 1] &= !field_pos;
			}
		}
		Ok(())
	}

	pub(crate) fn possible_digits_in_cell(&self, Cell(cell): Cell) -> Mask<Digit> {
		self.cell_poss_digits[cell as usize]
	}

    /*
	fn possible_digits_in_zone(&self, Zone(zone): Zone) -> Mask<Digit> {
		!self.zone_solved_digits[zone as usize] & Mask::ALL
	}
    */

	fn remove_impossibilities(&mut self, imposs_entrs: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		for entry in imposs_entrs.drain(..) {
			let impossibles = Mask::from_num(entry.num());
			self.remove_cell_impossibilities(entry.cell, impossibles)?;
		}
		Ok(())
	}

    // copied from fast solver
    // TODO: combine them
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
}
*/

///////////////////////////////////////////////////////////////////////////////////////////
//use std::ops::{Generator, GeneratorState};

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
    fn deduce_one(&self, state: &mut SudokuState) -> Result<(), Unsolvable> {
        use self::Strategy::*;
        match *self {
            NakedSingles => unimplemented!(),
			HiddenSingles => state.find_hidden_singles(true),
			LockedCandidates => state.find_locked_candidates(true),
			NakedSubsets => state.find_naked_subsets(true),
			HiddenSubsets => state.find_hidden_subsets(true),
			XWing => { state.find_xwings(true); Ok(()) },
			Swordfish => { state.find_swordfish(true); Ok(()) },
			Jellyfish => { state.find_jellyfish(true); Ok(()) },
			SinglesChain => state.find_singles_chain(), // TODO: Remove eager solver
            _ => unimplemented!(),
        }
    }

    fn deduce_all(&self, state: &mut SudokuState) -> Result<(), Unsolvable> {
        use self::Strategy::*;
        match *self {
            NakedSingles => state.update_cell_poss_zone_solved(),
			HiddenSingles => state.find_hidden_singles(false),
			LockedCandidates => state.find_locked_candidates(false),
			LockedCandidates => state.find_naked_subsets(false),
			HiddenSubsets => state.find_hidden_subsets(false),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ZoneType {
	Row,
	Col,
	Block,
}

#[derive(Debug, Clone)]
pub enum LineType {
	Row,
	Col,
}

#[derive(Debug, Clone)]
struct SinglesChain;

// TODO: Expand
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

/*
		for strategy_result in self.employed_strategies {

			println!("{:?}:", strategy);
			let deductions = match rg {
				Forced(rg) => &self.deduced_entries[rg],
				Impossible(rg) => &self.eliminated_entries[rg],
			};
			for &entry in deductions {
				println!("\tr{}c{} {}", entry.row()+1, entry.col()+1, entry.num);
			}
		//println!("{:?}", self.employed_strategies);
		}

*/

impl StrategyResult {
	fn strategy(&self) -> Strategy {
		use self::StrategyResult::*;
		match *self {
			NakedSingles(..) => Strategy::NakedSingles,
			HiddenSingles(..) => Strategy::HiddenSingles,
			LockedCandidates(..) => Strategy::LockedCandidates,
			NakedSubsets{..} => Strategy::NakedSubsets,
			HiddenSubsets{..} => Strategy::HiddenSubsets,
			XWing{..} => Strategy::XWing,
			Swordfish{..} => Strategy::Swordfish,
			Jellyfish{..} => Strategy::Jellyfish,
			SinglesChain{..} => Strategy::SinglesChain,
			__NonExhaustive => Strategy::__NonExhaustive,
		}
	}

	// not really an error, I just want an Either
	fn deductions<'e>(&self, eliminated: &'e [Entry]) -> Result<Entry, &'e [Entry]> {
		use self::StrategyResult::*;
		match *self {
			NakedSingles(entry) | HiddenSingles(entry, _) => Ok(entry),
			LockedCandidates(.., ref conflicts) |
			NakedSubsets { ref conflicts, .. } |
			HiddenSubsets { ref conflicts, .. } |
			XWing { ref conflicts, .. } |
			Swordfish { ref conflicts, .. } |
			Jellyfish { ref conflicts, .. } |
			SinglesChain(ref conflicts)

				=> Err(&eliminated[conflicts.clone()]),

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
				//print!("slice {} nums: {}", slice.0, nums.join(","))
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

//#[cfg(test)]
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
        where F: Fn(SudokuState, &[Strategy]) -> Result<Sudoku, Sudoku>,
    {
        let strategies = all_strategies();
        //let mut n_skip = 1; // FIXME: Improve solver, so the 7th sudoku can be solved without backtracking
        let mut unsolved = vec![];
        for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
            let cache = SudokuState::from_sudoku(sudoku);
            //print!("\nn_sudoku = {} ", i);
            match solver(cache, &strategies) {
                Ok(solution) => assert_eq!(solution, solved_sudoku),
                Err(part_solved) => unsolved.push((i, sudoku, part_solved, solved_sudoku)), // panic!("Found multiple solutions to sudoku with unique solution or none at all for {}th sudoku:\n{}", i, sudoku),
                //_ => n_skip -= 1,
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
		let solved = SudokuState::from_sudoku(sudoku).solve(&strategies).unwrap();
	}
	*/

    #[test]
    fn strategy_solver_correct_solution_easy_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_easy_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve);
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
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve);
    }

	/*
    #[test]
    fn strategy_solver_correct_solution_hard_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/hard_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_hard_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve);
    }
	*/

	/*
    #[test]
    fn backtracking_strategy_solver_correct_solution_easy_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_easy_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve_with_backtracking);
    }

    #[test]
    fn backtracking_strategy_solver_correct_solution_medium_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/medium_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_medium_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve_with_backtracking);
    }

    #[test]
    fn backtracking_strategy_solver_correct_solution_hard_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/hard_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_hard_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve_with_backtracking);
    }
	*/

    #[bench]
    fn easy_sudokus_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                SudokuState::from_sudoku(sudoku).solve(&strategies).unwrap(); //.unwrap();
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
                let _ = SudokuState::from_sudoku(sudoku).solve(&strategies); //.unwrap();
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
                SudokuState::from_sudoku(sudoku).solve_with_backtracking(&strategies).unwrap();
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
                let _ = SudokuState::from_sudoku(sudoku).solve_with_backtracking(&strategies).unwrap();
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
