#![allow(missing_docs)]
use sudoku::Sudoku;
use types::{Array81, Mask, Digit, Position, Unsolvable, Entry};
use consts::*;
use positions::{
    row_zone, col_zone, field_zone, cells_of_zone,
    Cell, Line, Zone, Slice, Band,
};

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
		*cell_mask &= !impossible;
		if *cell_mask == Mask::NONE {
			return Err(Unsolvable)
		}

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

	fn possible_digits_in_cell(&self, Cell(cell): Cell) -> Mask<Digit> {
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

	pub fn solve(mut self, strategies: &[Box<Strategy>]) -> Result<Sudoku, Unsolvable> {
		let mut deduced_entries = vec![];
		let mut impossible_entries = vec![];

		//println!("start:\n{}", self.grid);
		'outer: loop {
			for strategy in strategies.iter() {
				strategy.apply_strategy(&self, &mut deduced_entries, &mut impossible_entries)?;
				let found_sth = deduced_entries.len() != 0 || impossible_entries.len() != 0;

				//println!("applying {:?}. strategy => {:2} entries deduced, {:2} possibilities removed", i, deduced_entries.len(), impossible_entries.len());

				if deduced_entries.len() != 0 {
					self.insert_entries(&mut deduced_entries)?;
				}
				if impossible_entries.len() != 0 {
					self.remove_impossibilities(&mut impossible_entries)?;
				}
				if found_sth { continue 'outer }
				if self.is_solved() {
					return Ok(self.grid)
				}
			}

			//println!("Failure, but got this far: \n{}\n", self.grid);

			break Err(Unsolvable)
		}
	}

    pub fn solve_with_backtracking(self, strategies: &[Box<Strategy>]) -> Result<Sudoku, Unsolvable> {
		self._solve_with_backtracking(strategies)
    }

    fn _solve_with_backtracking(mut self, strategies: &[Box<Strategy>]) -> Result<Sudoku, Unsolvable> {
		let mut deduced_entries = vec![];
		let mut impossible_entries = vec![];
		let mut solver_stack = vec![];

		'outer: loop {
			'solve: loop {
				// deduce as much as possible
				for strategy in strategies.iter() {
					if strategy.apply_strategy(&self, &mut deduced_entries, &mut impossible_entries).is_err() {
						break 'solve
					}
					let found_sth = deduced_entries.len() != 0 || impossible_entries.len() != 0;

					//println!("applying {:?}. strategy => {:2} entries deduced, {:2} possibilities removed", i, deduced_entries.len(), impossible_entries.len());

					if deduced_entries.len() != 0 {
						if self.insert_entries(&mut deduced_entries).is_err() {
							break 'solve
						}
					}
					if impossible_entries.len() != 0 {
						if self.remove_impossibilities(&mut impossible_entries).is_err() {
							break 'solve
						}
					}
					if found_sth { continue 'solve }
					if self.is_solved() {
						return Ok(self.grid)
					}
				}

				// start backtracking
				let entry = self.find_good_guess();
				deduced_entries.push(entry);
				solver_stack.push((self.clone(), entry));
				self.insert_entries(&mut deduced_entries).unwrap();
			}

			'backtrack: loop {
				deduced_entries.clear();
				impossible_entries.clear();
				let (solver, entry): (_, _) = solver_stack.pop().ok_or(Unsolvable)?;
				self = solver;
				impossible_entries.push(entry);
				if self.remove_impossibilities(&mut impossible_entries).is_err() {
					continue
				};
				continue 'outer
			}
		}

		/*

		//println!("start:\n{}", self.grid);
		'outer: loop {
			for strategy in strategies.iter() {
				strategy.apply_strategy(&self, &mut deduced_entries, &mut impossible_entries)?;
				let found_sth = deduced_entries.len() != 0 || impossible_entries.len() != 0;

				//println!("applying {:?}. strategy => {:2} entries deduced, {:2} possibilities removed", i, deduced_entries.len(), impossible_entries.len());

				if deduced_entries.len() != 0 {
					self.insert_entries(&mut deduced_entries)?;
				}
				if impossible_entries.len() != 0 {
					self.remove_impossibilities(&mut impossible_entries)?;
				}
				if found_sth { continue 'outer }
				if self.is_solved() {
					return Ok(self.grid)
				}
			}

			let entry = self.find_good_guess();
            deduced_entries.push(entry);
			let mut clone = self.clone();
			clone.insert_entries(&mut deduced_entries).unwrap();
			match clone._solve_with_backtracking(strategies) {
                 solution @ Ok(_) => return solution,
				 Err(Unsolvable)  => {
					deduced_entries.clear();
					impossible_entries.clear();
					self.remove_cell_impossibilities(entry.cell, Mask::from_num(entry.num))?;
					continue
				 }
            };

			//println!("Failure, but got this far: \n{}\n", self.grid);

			//break Err(Unsolvable)
		}
		*/
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

pub trait Strategy: Sync {
	fn apply_strategy(
		&self, // for object safety only, whyever that's necessary
		sudoku: &StrategySolver,
		deduced_entries: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>)
		-> Result<(), Unsolvable>;
}

#[derive(Debug, Clone, Copy)]
pub struct HiddenSingles;

impl Strategy for HiddenSingles {
	fn apply_strategy(&self, sudoku: &StrategySolver, new_entrs: &mut Vec<Entry>, _: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		for zone in 0..27 {
			/* equivalent but slower
			for num_off in 0..8 {
				let poss_pos = sudoku.zone_poss_positions[zone as usize][num_off as usize];
				if let Some(unique_pos) = poss_pos.unique_pos().unwrap_or(None) {
					let cell = Cell::from_zone_pos(Zone(zone), unique_pos).0;
					new_entrs.push( Entry { cell, num: num_off + 1 })
				}
			}
			*/

			let cells = cells_of_zone(zone);
			let possible_digits_in_cells = cells.iter()
				.map(|&cell| Cell(cell))
				.map(|cell| sudoku.possible_digits_in_cell(cell));

			let (unsolved, _, mut singles) = find_unique(possible_digits_in_cells);

			if unsolved | sudoku.zone_solved_digits[zone as usize] != Mask::ALL {
				return Err(Unsolvable);
			}

			if singles == Mask::NONE { continue }

			for &cell in cells {
				let mask = sudoku.possible_digits_in_cell(Cell(cell));
				if mask & singles != Mask::NONE {
					let num = (mask & singles).unique_num().expect("unexpected empty mask").ok_or(Unsolvable)?;
					new_entrs.push(Entry{ cell: cell, num: num } );

					// remove single from mask
					singles &= !Mask::from_num(num);
					// everything in this zone found
					if singles == Mask::NONE { break }
				}
			}

		}
		Ok(())
	}
}

#[derive(Debug, Clone, Copy)]
pub struct NakedSingles;

impl Strategy for NakedSingles {
	fn apply_strategy(
		&self,
		sudoku: &StrategySolver,
		deduced_entries: &mut Vec<Entry>,
		_: &mut Vec<Entry>)
		-> Result<(), Unsolvable>
	{
		for (cell, poss_digits) in sudoku.cell_poss_digits.iter()
			.enumerate()
			// skip filled cells
			.filter(|&(cell, _)| sudoku.grid.0[cell] == 0)
		{
			if let Some(num) = poss_digits.unique_num()? {
				deduced_entries.push(Entry { num, cell: cell as u8 })
			}
		}
		Ok(())
	}
}

#[derive(Debug, Clone, Copy)]
pub struct LockedCandidates;

impl Strategy for LockedCandidates {
	fn apply_strategy(
		&self,
		sudoku: &StrategySolver,
		_: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>)
		-> Result<(), Unsolvable>
	{
		for band in (0..6).map(Band) {
			let mut slice_poss_digits: [Mask<Digit>; 9] = [Mask::NONE; 9];

			{ // compute possible digits for each slice
				let slices = band.slices();
				for (&slice, poss_digs) in slices.iter().zip(slice_poss_digits.iter_mut()) {
					for &cell in &slice.cells() {
						*poss_digs |= sudoku.possible_digits_in_cell(cell);
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

				let mut remove_impossibles = |uniques, neighbours: &[Slice; 2]| {
					for &neighbour in neighbours {
						let conflicts = slice_poss_digits[neighbour.band_idx()] & uniques;
						if conflicts == Mask::NONE { continue }

						for &cell in neighbour.cells().iter() {
							let conflicts = sudoku.possible_digits_in_cell(cell) & uniques;
							for num in conflicts.iter() {
								impossible_entries.push( Entry { cell: cell.0, num: num } )
							}
						}
					}
				};
				if line_uniques != Mask::NONE {
					remove_impossibles(line_uniques, &field_neighbours);
				}

				if field_uniques != Mask::NONE {
					remove_impossibles(field_uniques, &line_neighbours)
				}
			}
		}
		Ok(())
	}
}

pub struct NakedSubsets;

impl Strategy for NakedSubsets {
	fn apply_strategy(
		&self,
		sudoku: &StrategySolver,
		_: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>)
		-> Result<(), Unsolvable>
	{
		fn walk_combinations(
			sudoku: &StrategySolver,
			total_poss_digs: Mask<Digit>,
			cells: &[Cell],
			all_cells: &[Cell; 9],
			stack: &mut Vec<Cell>,
			impossible_entries: &mut Vec<Entry>,
		) {
			if stack.len() == 5 { return }
			if stack.len() > 0 && total_poss_digs.n_possibilities() == stack.len() as u8 {
				//println!("depth: {} => {:?}, commons: {:b}", stack.len(), stack, total_poss_digs.0);
				for cell in all_cells.iter().filter(|cell| !stack.contains(cell)) {
					let conflicts = sudoku.cell_poss_digits[cell.0 as usize] & total_poss_digs;
					for num in conflicts.iter() {
						impossible_entries.push(Entry{ cell: cell.0, num});
					}
				}
				return
			}

			for (i, &cell) in cells.iter().enumerate() {
				let cell_poss_digits =  sudoku.cell_poss_digits[cell.0 as usize];
				// solved cell
				if cell_poss_digits == Mask::NONE { continue }
				stack.push(cell);
				let new_total_poss_digs = total_poss_digs | cell_poss_digits;
				walk_combinations(sudoku, new_total_poss_digs, &cells[i+1..], all_cells, stack, impossible_entries);
				stack.pop();
			}
		}


		let mut stack = vec![];
		for zone in (0..27).map(Zone) {
			if sudoku.zone_solved_digits[zone.0 as usize] == Mask::ALL { continue }
			let cells = zone.cells();
			walk_combinations(sudoku, Mask::NONE, &cells, &cells, &mut stack, impossible_entries);
		}
		//if impossible_entries.len() != 0 {
		//	println!("{}", sudoku.grid);
		//}
		Ok(())
	}
}

pub struct HiddenSubsets;

impl Strategy for HiddenSubsets {
	fn apply_strategy(
		&self,
		sudoku: &StrategySolver,
		_: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>)
		-> Result<(), Unsolvable>
	{
		fn walk_combinations(
			sudoku: &StrategySolver,
			zone_poss_positions: [Mask<Position>; 9],
			zone: u8,
			total_poss_pos: Mask<Position>,
			num_offs: &[u8],
			all_num_offs: &[u8; 9],
			stack: &mut Vec<u8>,
			impossible_entries: &mut Vec<Entry>,
		) {
			if stack.len() == 5 { return }
			if stack.len() > 0 && total_poss_pos.n_possibilities() == stack.len() as u8 {
				//println!("depth: {} => {:?}, zone: {}, commons: {:b}", stack.len(), stack, zone, total_poss_pos.0);
				for &num_off in all_num_offs.iter().filter(|num_off| !stack.contains(num_off)) {
					//panic!("hi there");
					let conflicts = zone_poss_positions[num_off as usize] & total_poss_pos;
					for pos in conflicts.iter() {
						let Cell(cell) = Cell::from_zone_pos(Zone(zone), pos);
						impossible_entries.push(Entry{ cell, num: num_off + 1});
					}
				}
				return
			}

			for (i, &num_off) in num_offs.iter().enumerate() {
				let num_poss_pos = zone_poss_positions[num_off as usize];
				// solved cell
				if num_poss_pos == Mask::NONE { continue }
				stack.push(num_off);
				let new_total_poss_pos = total_poss_pos | num_poss_pos;
				walk_combinations(sudoku, zone_poss_positions, zone, new_total_poss_pos, &num_offs[i+1..], all_num_offs, stack, impossible_entries);
				stack.pop();
			}
		}


		let mut stack = vec![];
		for zone in 0..27 {
			if sudoku.zone_solved_digits[zone as usize] == Mask::ALL { continue }
			let num_offs = [0, 1, 2, 3, 4, 5, 6, 7, 8];
			walk_combinations(sudoku, sudoku.zone_poss_positions[zone as usize], zone, Mask::NONE, &num_offs, &num_offs, &mut stack, impossible_entries);
		}
		//if impossible_entries.len() != 0 {
		//	panic!("{}", sudoku.grid);
		//}
		Ok(())
	}
}

pub struct XWing;

impl Strategy for XWing {
	fn apply_strategy(
		&self,
		sudoku: &StrategySolver,
		_: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>)
		-> Result<(), Unsolvable>
	{
		// if for any number
		// 2 rows have only 2 possible positions each and these positions are identical
		// then no other row can have the number in the same cols
		// likewise applicable for columns
		let mut stack = vec![];
		for num_off in 0..8 {
			// 0..9 = rows, 9..18 = cols
			for lines in &[Line::ALL_ROWS, Line::ALL_COLS] {
				basic_fish_walk_combinations(sudoku, num_off, 2, &mut stack, lines, lines, Mask::NONE, impossible_entries);
				/*
				for (i, line1) in lines.iter().enumerate() {
					let poss_pos1 = sudoku.zone_poss_positions[line1.0 as usize][num_off];
					if poss_pos1.n_possibilities() != 2 { continue }

					//let boundary = if line1 < 9 { 9 } else { 18 };
					for line2 in (&lines[i+1..]).iter() {
						let poss_pos2 = sudoku.zone_poss_positions[line2.0 as usize][num_off];

						if poss_pos1 == poss_pos2 {
							// loop through other rows
							for line in lines.iter().filter(|&line| line != line1 && line != line2 ) {
								for pos in poss_pos1 {
									let cell = Cell::from_zone_pos(line.zone(), pos);
									//Cell::from_zone_pos()
									//let cell = row*9 + col;
									let cell_mask = sudoku.possible_digits_in_cell(cell);
									if cell_mask & Mask::from_num(num_off as u8 +1) != Mask::NONE {
										impossible_entries.push(Entry{ num: num_off as u8 +1, cell: cell.0 });
									}
								}
							}
						}


					}
				}*/
			}
		}
		Ok(())
	}
}

pub struct Swordfish;

impl Strategy for Swordfish {
	fn apply_strategy(
		&self,
		sudoku: &StrategySolver,
		_: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>)
		-> Result<(), Unsolvable>
	{
		let mut stack = vec![];
		for num_off in 0..8 {
			// 0..9 = rows, 9..18 = cols
			for lines in &[Line::ALL_ROWS, Line::ALL_COLS] {
				basic_fish_walk_combinations(sudoku, num_off, 3, &mut stack, lines, lines, Mask::NONE, impossible_entries);
			}
		}
		Ok(())
	}
}

pub struct Jellyfish;

impl Strategy for Jellyfish {
	fn apply_strategy(
		&self,
		sudoku: &StrategySolver,
		_: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>)
		-> Result<(), Unsolvable>
	{
		let mut stack = vec![];
		for num_off in 0..8 {
			// 0..9 = rows, 9..18 = cols
			for lines in &[Line::ALL_ROWS, Line::ALL_COLS] {
				basic_fish_walk_combinations(sudoku, num_off, 4, &mut stack, lines, lines, Mask::NONE, impossible_entries);
			}
		}
		Ok(())
	}
}

pub struct SinglesChain;

impl Strategy for SinglesChain {
    	fn apply_strategy(
		&self,
		sudoku: &StrategySolver,
		_deduced_entries: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>)
		-> Result<(), Unsolvable>
	{
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
                let zone_poss_positions = sudoku.zone_poss_positions[con_zone.0 as usize][num_off as usize];
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
                let zone_poss_positions = sudoku.zone_poss_positions[zone.0 as usize][num_off as usize];
                if zone_poss_positions.n_possibilities() == 2 {
                    let first = zone_poss_positions.one_possibility();
                    let cell = Cell::from_zone_pos(zone, first);

                    match cell_touched[cell.0 as usize] {
                        true => continue,
                        false => cell_touched[cell.0 as usize] = true,
                    };

                    follow_links(num_off, cell, true, sudoku, &mut cell_color, link_nr, &mut cell_linked);
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
                    mark_impossible(num_off+1, link_nr, impossible_colour, cell_color, cell_linked, impossible_entries);
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
                                    impossible_entries.push( Entry{ cell: neighbour_cell.0, num: num_off+1 })
                                }
                            } else if cell_colour == Colour::B && !sees_b {
                                cell_sees_colour[neighbour_cell.0 as usize].1 = true;
                                if sees_a {
                                    impossible_entries.push( Entry{ cell: neighbour_cell.0, num: num_off+1 })
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
// x-wing          2
// swordfish       3
// jellyfish       4
fn basic_fish_walk_combinations(
	sudoku: &StrategySolver,
	num_off: usize,
	goal_depth: usize,
	stack: &mut Vec<Line>,
	lines: &[Line],
	all_lines: &[Line; 9],
	union_poss_pos: Mask<Position>,
	impossible_entries: &mut Vec<Entry>
) {
	if stack.len() == goal_depth {
		// nothing of interest found
		if union_poss_pos.n_possibilities() != goal_depth as u8 { return }

		// found xwing, swordfish, jellyfish, whatever-the-name
		for line in all_lines.iter().filter(|&line| !stack.contains(line)) {
			for pos in union_poss_pos.iter() {
				let cell = Cell::from_zone_pos(line.zone(), pos);
				let cell_mask = sudoku.possible_digits_in_cell(cell);
				if cell_mask & Mask::from_num(num_off as u8 +1) != Mask::NONE {
					impossible_entries.push(Entry{ num: num_off as u8 +1, cell: cell.0 });
				}
			}
		}
		return
	}
	for (i, &line) in lines.iter().enumerate() {
		let possible_pos = sudoku.zone_poss_positions[line.0 as usize][num_off];
		let n_poss = possible_pos.n_possibilities();
		let new_union_poss_pos = union_poss_pos | possible_pos;
		if n_poss < 2 || new_union_poss_pos.n_possibilities() > goal_depth as u8 { continue }

		stack.push(line);
		basic_fish_walk_combinations(sudoku, num_off, goal_depth, stack, &lines[i+1..], all_lines, new_union_poss_pos, impossible_entries);
		stack.pop();
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
    #[cfg(bench)] extern crate test;
    use super::*;
    fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
    sudokus_str.lines()
            .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
            .collect()
    }

    fn all_strategies() -> Vec<Box<Strategy>> {
        vec![
            Box::new(NakedSingles),
            Box::new(HiddenSingles),
            Box::new(NakedSubsets),
            Box::new(HiddenSubsets),
            Box::new(LockedCandidates),
            Box::new(XWing),
            Box::new(Swordfish),
            Box::new(Jellyfish),
            Box::new(SinglesChain),
        ]
    }

    fn strategy_solver_correct_solution<F>(sudokus: Vec<Sudoku>, solved_sudokus: Vec<Sudoku>, solver: F)
        where F: Fn(StrategySolver, &[Box<Strategy>]) -> Result<Sudoku, Unsolvable>,
    {
        let strategies = all_strategies();
        //let mut n_skip = 1; // FIXME: Improve solver, so the 7th sudoku can be solved without backtracking
        let mut unsolved = vec![];
        for (i, (sudoku, solved_sudoku)) in sudokus.into_iter().zip(solved_sudokus).enumerate() {
            let cache = StrategySolver::from_sudoku(sudoku).unwrap();
            //print!("\nn_sudoku = {} ", i);
            match solver(cache, &strategies) {
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
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_easy_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve);
    }

    #[test]
    fn strategy_solver_correct_solution_medium_sudokus() {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_medium_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve);
    }

    #[test]
    fn strategy_solver_correct_solution_hard_sudokus() {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_hard_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve);
    }

    #[test]
    fn backtracking_strategy_solver_correct_solution_easy_sudokus() {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_easy_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve_with_backtracking);
    }

    #[test]
    fn backtracking_strategy_solver_correct_solution_medium_sudokus() {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_medium_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve_with_backtracking);
    }

    #[test]
    fn backtracking_strategy_solver_correct_solution_hard_sudokus() {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/hard_sudokus.txt") );
        let solved_sudokus = read_sudokus( include_str!("../sudokus/Lines/solved_hard_sudokus.txt") );
        strategy_solver_correct_solution(sudokus, solved_sudokus, StrategySolver::solve_with_backtracking);
    }

    #[bench]
    #[cfg(bench)]
    fn easy_sudokus_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                StrategySolver::from_sudoku(sudoku).unwrap().solve(&strategies).unwrap();
            }
        })
    }

    #[bench]
    #[cfg(bench)]
    fn medium_sudokus_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                // solution not guaranteed yet, discard error.
                let _ = StrategySolver::from_sudoku(sudoku).unwrap().solve(&strategies).unwrap();
            }
        })
    }

	#[bench]
    #[cfg(bench)]
    fn easy_sudokus_backtracking_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/easy_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                StrategySolver::from_sudoku(sudoku).unwrap().solve_with_backtracking(&strategies).unwrap();
            }
        })
    }

    #[bench]
    #[cfg(bench)]
    fn medium_sudokus_backtracking_strategy_solver(b: &mut test::Bencher) {
        let sudokus = read_sudokus( include_str!("../sudokus/Lines/medium_sudokus.txt") );
        let sudokus_100 = sudokus.iter().cycle().cloned().take(100).collect::<Vec<_>>();
        let strategies = all_strategies();
        b.iter(|| {
            for sudoku in sudokus_100.iter().cloned() {
                // solution not guaranteed yet, discard error.
                let _ = StrategySolver::from_sudoku(sudoku).unwrap().solve_with_backtracking(&strategies).unwrap();
            }
        })
    }
}

pub mod strategies {
    pub use super::{NakedSingles, HiddenSingles, NakedSubsets, HiddenSubsets, LockedCandidates,
                    XWing, Swordfish, Jellyfish, SinglesChain};
	pub use super::Strategy;
}
