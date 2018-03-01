#![allow(unused)]
#![allow(missing_docs)]
use sudoku::Sudoku;
use types::{Array81, Mask, Digit, Position, Unsolvable, Entry};
use consts::*;
use positions::{
    row_zone, col_zone, field_zone, cells_of_zone, neighbours,
    Cell, Line, Zone, Slice, Band,
};
use super::NewStrategy;

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

#[derive(Debug, Clone)]
pub enum Deduction {
	Forced(Entry),
	Impossible(Entry),
}

#[derive(Debug, Clone)]
pub struct SudokuState {
	// keep a history of all deductions, both filled cells and eliminated possibilities
	// as well as which deduction
	pub employed_strategies: Vec<Strategy>,
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
	fn from_sudoku(sudoku: Sudoku) -> SudokuState {
		let mut deduced_entries = sudoku.iter()
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

	fn solve(mut self, strategies: &[Strategy]) -> Result<Sudoku, Unsolvable> {
		'outer: loop {
			if self.is_solved() {
				self.update_grid();
				println!("WTF: len: {}, deductions: {}, grid: {}", self.deduced_entries.len(), self.employed_strategies.len(), self.grid.to_str_line());
				return Ok(self.grid)
			}

			// no chance without strategies
			let (first, rest) = strategies.split_first().ok_or(Unsolvable)?;

			first.deduce_all(&mut self)?;

			let n_deductions = self.deduced_entries.len();
			for strategy in rest {
				strategy.deduce_one(&mut self);
				if self.deduced_entries.len() > n_deductions {
					continue 'outer
				}
			}
			break Err(Unsolvable)
		}
	}

	fn is_solved(&self) -> bool {
		self.n_solved == 81
	}

	fn update_cell_poss_zone_solved(&mut self) -> Result<(), Unsolvable> {
		{
			let (ld_cp, le_cp, cell_poss) = self.cell_poss_digits.get_mut();
			let (ld_zp, le_zp, zone_solved) = self.zone_solved_digits.get_mut();

			for &entry in &self.eliminated_entries[*le_cp as _..] {
				let impossibles = Mask::from_num(entry.num());
				let cell_mask = &mut cell_poss[entry.cell as usize];
				*cell_mask &= !impossibles;
				if *cell_mask == Mask::NONE {
					return Err(Unsolvable)
				}
			}
			*le_cp = self.eliminated_entries.len() as _;
		}

		self.insert_entries()
	}

	fn update_zone_poss_positions(&mut self) -> Result<(), Unsolvable> {
		let (nd, ne, zone_poss_positions) = self.zone_poss_positions.get_mut();
		// remove now impossible positions from list
		for entry in &self.eliminated_entries[*ne as usize ..] {
			let cell = entry.cell;
			let row_pos = Mask::row_pos_of_cell(cell);
			let col_pos = Mask::col_pos_of_cell(cell);
			let field_pos = Mask::field_pos_of_cell(cell);
			let row = row_zone(cell);
			let col = col_zone(cell);
			let field = field_zone(cell);
			for num in entry.mask().iter() {
				zone_poss_positions[row][num as usize - 1] &= !row_pos;
				zone_poss_positions[col][num as usize - 1] &= !col_pos;
				zone_poss_positions[field][num as usize - 1] &= !field_pos;
			}
		}
		*ne = self.eliminated_entries.len() as _;

		for entry in &self.deduced_entries[*nd as usize ..] {
			let cell = entry.cell;
			let row_pos = Mask::row_pos_of_cell(cell);
			let col_pos = Mask::col_pos_of_cell(cell);
			let field_pos = Mask::field_pos_of_cell(cell);
			let row = row_zone(cell);
			let col = col_zone(cell);
			let field = field_zone(cell);
			for num in 0..9 {
				zone_poss_positions[row][num] &= !row_pos;
				zone_poss_positions[col][num] &= !col_pos;
				zone_poss_positions[field][num] &= !field_pos;
			}
		}
		*nd = self.deduced_entries.len() as _;
		Ok(())
	}

	#[inline(always)]
	fn insert_entries(&mut self) -> Result<(), Unsolvable> {
		// code hereafter depends on this
		// but it's not necessary in general
		assert!(self.cell_poss_digits.next_deduced == self.zone_solved_digits.next_deduced);
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
		let (nd_cp, ne_cp, cell_poss_digits) = self.cell_poss_digits.get_mut();
		let (nd_zs, ne_zs, zone_solved_digits) = self.zone_solved_digits.get_mut();

		loop {
			if self.deduced_entries.len() <= *nd_cp as usize { break }
			let entry = self.deduced_entries[*nd_cp as usize];
			*nd_cp += 1;
			*nd_zs += 1;
			let entry_mask = entry.mask();
			// cell already solved from previous entry in stack, skip
			if cell_poss_digits[entry.cell()] == Mask::NONE { continue }

			// is entry still possible?
			if cell_poss_digits[entry.cell()] & entry_mask == Mask::NONE {
				return Err(Unsolvable);
			}

			Self::_insert_entry_cp_zs(entry, &mut self.n_solved, &mut self.employed_strategies, cell_poss_digits, zone_solved_digits);
			for &cell in neighbours(entry.cell) {
				if entry_mask & cell_poss_digits[cell as usize] != Mask::NONE {
					Self::remove_impossibilities(cell_poss_digits, cell, entry_mask, &mut self.deduced_entries, &mut self.employed_strategies)?;
				};
			}

			// found a lot of naked singles, switch to batch insertion
			if self.deduced_entries.len() - *nd_cp as usize > 4 { return Ok(()) }
		}
		Ok(())
	}

	#[inline]
	fn _insert_entry_cp_zs(
		entry: Entry,
		n_solved: &mut u8,
		employed_strategies: &mut Vec<Strategy>,
		cell_poss_digits: &mut Array81<Mask<Digit>>,
		zone_solved_digits: &mut [Mask<Digit>; 27]
	) {
		employed_strategies.push(Strategy::NakedSingles);
		*n_solved += 1;
		cell_poss_digits[entry.cell()] = Mask::NONE;
		zone_solved_digits[entry.row() as usize +ROW_OFFSET] |= entry.mask();
		zone_solved_digits[entry.col() as usize +COL_OFFSET] |= entry.mask();
		zone_solved_digits[entry.field() as usize +FIELD_OFFSET] |= entry.mask();
	}

	pub fn batch_insert_entries(&mut self) -> Result<(), Unsolvable> {
		let (nd_cp, ne_cp, cell_poss_digits) = self.cell_poss_digits.get_mut();
		let (nd_zs, ne_zs, zone_solved_digits) = self.zone_solved_digits.get_mut();
		//while self.deduced_entries.len() > *nd_cp as usize {
		loop {
			if self.deduced_entries.len() <= *nd_cp as usize { break }
			let entry = self.deduced_entries[*nd_cp as usize];
			*nd_cp += 1;
			*nd_zs += 1;
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

			Self::_insert_entry_cp_zs(entry, &mut self.n_solved, &mut self.employed_strategies, cell_poss_digits, zone_solved_digits);
		}

		// update cell possibilities from zone masks
		for cell in 0..81 {
			if cell_poss_digits[cell as usize] == Mask::NONE { continue }
			let zones_mask = zone_solved_digits[row_zone(cell)]
				| zone_solved_digits[col_zone(cell)]
				| zone_solved_digits[field_zone(cell)];

			Self::remove_impossibilities(cell_poss_digits, cell, zones_mask, &mut self.deduced_entries, &mut self.employed_strategies)?;
		}
		Ok(())
	}

	// remove impossible digits from masks for given cell
	// also check for naked singles and impossibility of sudoku
	fn remove_impossibilities(
		cell_poss_digits: &mut Array81<Mask<Digit>>,
		cell: u8, impossible: Mask<Digit>,
		deduced_entries: &mut Vec<Entry>,
		employed_strategies: &mut Vec<Strategy>
	) -> Result<(), Unsolvable> {
		let cell_mask = &mut cell_poss_digits[cell as usize];
		cell_mask.remove(impossible);
		if let Some(num) = cell_mask.unique_num()? {
			deduced_entries.push(Entry{ cell, num });
			employed_strategies.push(Strategy::NakedSingles);
		}
		Ok(())
	}

	fn find_hidden_singles(&mut self, stop_after_first: bool) -> Result<(), Unsolvable> {
		// TODO: remove auto-deducing naked singles inside update procedure
		self.update_cell_poss_zone_solved();

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
					self.deduced_entries.push(Entry{ cell: cell, num: num } );
					self.employed_strategies.push(Strategy::HiddenSingles);

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
		self.update_cell_poss_zone_solved();
		let (nd_cp, ne_cp, cell_poss_digits) = self.cell_poss_digits.get_mut();
		//let (nd_zs, ne_zs, zone_solved_digits) = self.zone_solved_digits.get_mut();

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
				let mut remove_impossibles = |uniques, neighbours: &[Slice; 2]| {
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
				};


				if stop_after_first {
					if line_uniques != Mask::NONE {
						remove_impossibles(line_uniques, &field_neighbours);
					} else if field_uniques != Mask::NONE {
						remove_impossibles(field_uniques, &line_neighbours);
					} else {
						continue
					}
					self.employed_strategies.push(Strategy::LockedCandidates);
					return Ok(());
				} else {
					if line_uniques != Mask::NONE {
						self.employed_strategies.push(Strategy::LockedCandidates);
						remove_impossibles(line_uniques, &field_neighbours);
					}

					if field_uniques != Mask::NONE {
						self.employed_strategies.push(Strategy::LockedCandidates);
						remove_impossibles(field_uniques, &line_neighbours);
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
			all_cells: &[Cell; 9],
			stack: &mut Vec<Cell>,
			stop_after_first: bool,
		) -> bool {
			// subsets of 5 and more numbers always have complementary subsets
			// of 9 - subset_size
			if stack.len() == 5 { return false }
			if stack.len() > 0 && total_poss_digs.n_possibilities() == stack.len() as u8 {
				// found a subset

				for cell in all_cells.iter().filter(|cell| !stack.contains(cell)) {
					let conflicts = state.cell_poss_digits.state[cell.0 as usize] & total_poss_digs;
					for num in conflicts.iter() {
						state.eliminated_entries.push(Entry{ cell: cell.0, num});
					}
				}
				state.employed_strategies.push(Strategy::NakedSubsets);
				if stop_after_first {
					return true
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
				if walk_combinations(state, new_total_poss_digs, &cells[i+1..], all_cells, stack, stop_after_first) {
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
			if walk_combinations(self, Mask::NONE, &cells, &cells, &mut stack, stop_after_first) {
				break
			};
		}
		Ok(())
	}

	fn find_hidden_subsets(&mut self, stop_after_first: bool) -> Result<(), Unsolvable> 	{
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
				for &num_off in all_num_offs.iter().filter(|num_off| !stack.contains(num_off)) {
					let conflicts = zone_poss_positions[num_off as usize] & total_poss_pos;
					for pos in conflicts.iter() {
						let Cell(cell) = Cell::from_zone_pos(Zone(zone), pos);
						state.eliminated_entries.push(Entry{ cell, num: num_off + 1});
					}
				}
				state.employed_strategies.push(Strategy::HiddenSubsets);
				if stop_after_first {
					return true
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
use std::ops::{Generator, GeneratorState};

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
/*
fn naked_singles<'a>(state: &'a StrategySolver) -> impl Generator<Yield=Entry, Return=Result<(), Unsolvable>> + 'a {
    move || {
        for cell in 0..81 {
            let poss_digits = state.cell_poss_digits.0[cell];
            if let Ok(Some(num)) = poss_digits.unique_num() {
                yield Entry { num, cell: cell as u8 }
			}
		}
		Ok(())
    }
}

fn hidden_singles<'a>(state: &'a StrategySolver) -> impl Generator<Yield=Entry, Return=Result<(), Unsolvable>> + 'a  {
    move || {
        for zone in 0..27 {
            let cells = cells_of_zone(zone);
            let (unsolved, _, mut singles) = {
                let possible_digits_in_cells = cells.iter()
                    .map(|&cell| Cell(cell))
                    .map(|cell| state.possible_digits_in_cell(cell));

                find_unique(possible_digits_in_cells)
            };


            if unsolved | state.zone_solved_digits[zone as usize] != Mask::ALL {
                return Err(Unsolvable);
            }

            if singles.is_empty() { continue }

            for &cell in cells {
                let mask = state.possible_digits_in_cell(Cell(cell));
                if mask & singles != Mask::NONE {
                    let num = (mask & singles).unique_num().expect("unexpected empty mask").ok_or(Unsolvable)?;
                    yield Entry { cell, num };

                    // remove single from mask
                    singles &= !Mask::from_num(num);
                    // everything in this zone found
                    if singles == Mask::NONE { break }
                }
            }

        }
        Ok(())
    }
}*/

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
            NakedSubsets,
            HiddenSubsets,
            LockedCandidates,
            //XWing,
            //Swordfish,
            //Jellyfish,
            //SinglesChain,
        ]
    }

    fn strategy_solver_correct_solution<F>(sudokus: Vec<Sudoku>, solved_sudokus: Vec<Sudoku>, solver: F)
        where F: Fn(SudokuState, &[Strategy]) -> Result<Sudoku, Unsolvable>,
    {
        let strategies = all_strategies();
        //let mut n_skip = 1; // FIXME: Improve solver, so the 7th sudoku can be solved without backtracking
        let mut unsolved = vec![];
        for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
            let cache = SudokuState::from_sudoku(sudoku);
            //print!("\nn_sudoku = {} ", i);
            match cache.solve(&strategies) {
                Ok(solution) => assert_eq!(solution, solved_sudoku),
                Err(Unsolvable) => unsolved.push((i, sudoku, solved_sudoku)), // panic!("Found multiple solutions to sudoku with unique solution or none at all for {}th sudoku:\n{}", i, sudoku),
                //_ => n_skip -= 1,
            }
        }
        if unsolved.len() != 0 {
            println!("Could not solve {} sudokus:\n", unsolved.len());

            //for (i, sudoku, _solution) in unsolved {
            //	println!("\nsudoku nr {}:\n\n{}", i+1, sudoku);
            //}
            panic!();
        }
    }

    #[test]
    fn strategy_solver_correct_solution_easy_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_easy_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve);
    }

    #[test]
    fn strategy_solver_correct_solution_medium_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/medium_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_medium_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve);
    }

    #[test]
    fn strategy_solver_correct_solution_hard_sudokus() {
        let sudokus = read_sudokus( include_str!("../../sudokus/Lines/hard_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../../sudokus/Lines/solved_hard_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, SudokuState::solve);
    }
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
                SudokuState::from_sudoku(sudoku).solve(&strategies).unwrap();
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
                let _ = SudokuState::from_sudoku(sudoku).solve(&strategies).unwrap();
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
