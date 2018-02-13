use rand::Rng;

use consts::*;
use positions::*;
use types::{Mask, Digit, Array81, Entry, Unsolvable};
use sudoku::Sudoku;

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
	#[inline]
	pub fn new() -> SudokuSolver {
		SudokuSolver {
			grid: Sudoku([0; 81]),
			n_solved_cells: 0,
			cell_poss_digits: Array81([Mask::ALL; 81]),
			zone_solved_digits: [Mask::NONE; 27],
			last_cell: 0,
		}
	}

	#[inline]
	fn _insert_entry(&mut self, entry: Entry) {
		self.n_solved_cells += 1;
		self.grid.0[entry.cell()] = entry.num;
		self.cell_poss_digits[entry.cell()] = Mask::NONE;
		self.zone_solved_digits[entry.row() as usize +ROW_OFFSET] |= entry.mask();
		self.zone_solved_digits[entry.col() as usize +COL_OFFSET] |= entry.mask();
		self.zone_solved_digits[entry.field() as usize +FIELD_OFFSET] |= entry.mask();
	}

	#[inline(always)]
	fn insert_entries(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
		loop {
			match stack.len() {
				0 => break Ok(()),
				1...4 => self.insert_entries_singly(stack)?,
				_ => self.batch_insert_entries(stack)?,
			}
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
			if self.cell_poss_digits[entry.cell()] == Mask::NONE { continue }

			// is entry still possible?
			if self.cell_poss_digits[entry.cell()] & entry_mask == Mask::NONE {
				return Err(Unsolvable);
			}

			self._insert_entry(entry);
			for &cell in neighbours(entry.cell) {
				if entry_mask & self.cell_poss_digits[cell as usize] != Mask::NONE {
					self.remove_impossibilities(cell, entry_mask, stack)?;
				};
			}

			// found a lot of naked singles, switch to batch insertion
			if stack.len() > 4 { return Ok(()) }
		}
		Ok(())
	}

	pub fn batch_insert_entries(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
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

			self.remove_impossibilities(cell, zones_mask, stack)?;
		}
		Ok(())
	}

	#[inline]
	pub fn is_solved(&self) -> bool {
		self.n_solved_cells == 81
	}

	#[inline(always)]
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

	// and save where the search ended up last time
	// to have a better chance of finding minimal cells quickly
	// on the next round
	#[inline]
	fn find_cell_min_poss(&mut self) -> u8 {
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
		best_cell
	}

	#[inline(always)]
	fn find_good_random_guess(&mut self) -> Entry {
		let best_cell = self.find_cell_min_poss();
		let poss_digits = self.cell_poss_digits[best_cell as usize];
		let choice = ::rand::thread_rng().gen_range(0, poss_digits.n_possibilities());
		let num = poss_digits.iter().nth(choice as usize).unwrap();
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

	// for generation of random, filled sudokus
	pub fn randomized_solve_one(mut self, stack: &mut Vec<Entry>) -> Result<Sudoku, Unsolvable> {
		// insert and deduce in a loop
		// do a random guess when no more deductions are found
		// backtrack on error (via recursion)
		loop {
			self.insert_entries(stack)?;
			if self.is_solved() {
				return Ok(self.grid)
			}

			self.find_hidden_singles(stack)?;
			if !stack.is_empty() { continue }

			let entry = self.find_good_random_guess();
			stack.push(entry);
			if let filled_sudoku @ Ok(_) = self.clone().randomized_solve_one(stack) {
				return filled_sudoku;
			}
			stack.clear();

			self.remove_impossibilities(entry.cell, entry.mask(), stack)?;
		}
	}
}

// 27 bits set
const ALL: u32 = 0o777_777_777;
const LOW9: u32 = 0o000_000_777;

#[derive(Clone, Copy)]
struct UnsafeArray3([u32; 3]);

impl ::std::ops::Index<usize> for UnsafeArray3 {
	type Output = u32;
	fn index(&self, idx: usize) -> &Self::Output {
		index(&self.0, idx)
	}
}

impl ::std::ops::IndexMut<usize> for UnsafeArray3 {
	fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
		index_mut(&mut self.0, idx)
	}
}

#[derive(Clone, Copy)]
struct UnsafeArray27([u32; 27]);

impl ::std::ops::Index<usize> for UnsafeArray27 {
	type Output = u32;
	fn index(&self, idx: usize) -> &Self::Output {
		index(&self.0, idx)
	}
}

impl ::std::ops::IndexMut<usize> for UnsafeArray27 {
	fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
		index_mut(&mut self.0, idx)
	}
}

#[derive(Debug)]
enum Solutions {
	Count(usize),
	Vector(Vec<Sudoku>),
}

impl Solutions {
	fn len(&self) -> usize {
		match *self {
			Solutions::Count(len) => len,
			Solutions::Vector(ref v) => v.len(),
		}
	}

	fn into_vec(self) -> Option<Vec<Sudoku>> {
		match self {
			Solutions::Vector(v) => Some(v),
			Solutions::Count(_) => None,
		}
	}
}

#[derive(Clone, Copy)]
pub(crate) struct SudokuSolver2 {
	bands: UnsafeArray27, // 9 digits, 3 rows each
	prev_bands: UnsafeArray27,
	unsolved_cells: UnsafeArray3, // 81 bits used
	unsolved_rows: UnsafeArray3, // 27 slices, 3 bits per slice
	pairs: UnsafeArray3, // cells with only 2 possibilites, 81 bits used
}

type SolvStack = Vec<SudokuSolver2>;

impl SudokuSolver2 {
	// InitSudoku equivalent
	pub fn from_sudoku(sudoku: Sudoku) -> Result<Self, Unsolvable> {
		let mut solver = SudokuSolver2 {
			bands: UnsafeArray27([ALL; 27]),
			prev_bands: UnsafeArray27([0; 27]),
			unsolved_cells: UnsafeArray3([ALL; 3]),
			unsolved_rows: UnsafeArray3([ALL; 3]),
			pairs: UnsafeArray3([0; 3]),
		};
		for (cell, num) in (0..81).zip(sudoku.iter()) {
			if let Some(num) = num {
				solver._insert_entry(Entry { cell, num })?;
			}
		}
		Ok(solver)
	}

	// SetSolvedDigit equivalent
	fn _insert_entry(&mut self, entry: Entry) -> Result<(), Unsolvable> {
		let band = band_of_cell(entry.cell);
		let slice = digit_to_base(entry.num) + band;
		let cell_mask = mask_of_cell(entry.cell);

		if self.bands[slice as usize] & cell_mask == 0 {
			return Err(Unsolvable)
		}

		self.bands[slice as usize] &= self_mask(entry.cell);
		let other_mask = other_mask(entry.cell);
		let (ns1, ns2) = neighbour_slices(slice);
		self.bands[ns1 as usize] &= other_mask;
		self.bands[ns2 as usize] &= other_mask;

		let mask = !cell_mask;
		self.unsolved_cells[band as usize] &= mask;
		let row_bit = (entry.num()-1)*9 + row_of_cell(entry.cell); //entry.row();
		self.unsolved_rows[row_bit as usize /27] &= !(1 << mod27(row_bit));

		let mut band = band as usize;
		for _ in 0..9 {
			self.bands[band] &= mask;
			band += 3;
		}

		// add solution back
		self.bands[slice as usize] |= cell_mask;
		Ok(())
	}

	fn extract_solution(&self) -> Sudoku {
		let mut sudoku = [0; 81];
		for (slice, &mask) in (0u8..27).zip(self.bands.0.iter()) {
			let mut mask = mask;
			let digit = slice / 3;
			let base_cell_in_band = mod3(slice)*27;
			while mask != 0 {
				let lowest_bit = mask & (!mask + 1);
				// lowest bit == cell mask == 1 << (cell % 27)
				let cell_in_band = bit_pos(lowest_bit) as u8;
				*index_mut(&mut sudoku, (cell_in_band + base_cell_in_band) as usize) = digit + 1;

				// guaranteed no overlap between mask and lowest_bit
				mask ^= lowest_bit;
			}
		}
		Sudoku(sudoku)
	}

	fn _set_solved_mask(&mut self, slice: u8, mask: u32) -> Result<(), Unsolvable> {
		if self.bands[slice as usize] & mask == 0 {
			return Err(Unsolvable);
		}
		let band = mod3(slice);
		let cell = band*27 + bit_pos(mask);

		self.bands[slice as usize] &= self_mask(cell);
		Ok(())
	}

	// hidden? naked?
	fn find_singles(&mut self) -> Result<bool, Unsolvable> {
		let mut single_applied = false;

		for band in 0..3 {
			let mut r1 = 0; // exists
			let mut r2 = 0; // exists twice
			let mut r3 = 0; // exists thrice or more
			let mut slice = band;
			if false {
				////////////// loop
				while slice < 27 {
					let band_mask = self.bands[slice as usize];

					r3 |= r2 & band_mask;
					r2 |= r1 & band_mask;
					r1 |= band_mask;

					slice += 3;
				}
			} else {
				///////////// unrolled loop
				let mut band_mask = self.bands[band as usize];
				r1 |= band_mask;
				band_mask = self.bands[3 + band as usize];
				r2 |= r1 & band_mask;
				r1 |= band_mask;
				band_mask = self.bands[6 + band as usize];
				r3 |= r2 & band_mask;
				r2 |= r1 & band_mask;
				r1 |= band_mask;
				band_mask = self.bands[9 + band as usize];
				r3 |= r2 & band_mask;
				r2 |= r1 & band_mask;
				r1 |= band_mask;
				band_mask = self.bands[12 + band as usize];
				r3 |= r2 & band_mask;
				r2 |= r1 & band_mask;
				r1 |= band_mask;
				band_mask = self.bands[15 + band as usize];
				r3 |= r2 & band_mask;
				r2 |= r1 & band_mask;
				r1 |= band_mask;
				band_mask = self.bands[18 + band as usize];
				r3 |= r2 & band_mask;
				r2 |= r1 & band_mask;
				r1 |= band_mask;
				band_mask = self.bands[21 + band as usize];
				r3 |= r2 & band_mask;
				r2 |= r1 & band_mask;
				r1 |= band_mask;
				band_mask = self.bands[24 + band as usize];
				r3 |= r2 & band_mask;
				r2 |= r1 & band_mask;
				r1 |= band_mask;
			}

			///////////////////
			if r1 != ALL {
				return Err(Unsolvable);
			}

			// store doubles
			// equivalent to `r2 & !r3` because every bit in r3 is also in r2
			self.pairs[band as usize] = r2 ^ r3;

			// singles
			r1 ^= r2;
			// new singles, ignore previously solved ones
			r1 &= self.unsolved_cells[band as usize];

			'r1: while r1 != 0 {
				single_applied = true;
				let lowest_bit = r1 & (!r1 + 1);
				r1 ^= lowest_bit;
				for digit in 0..9 {
					if self.bands[(digit*3 + band) as usize] & lowest_bit != 0 {

                        // FIXME: find out whether it's possible for this to err
                        // ORIGBUG: no early return? 2 other places
						let _ = self._set_solved_mask(digit*3 + band, lowest_bit);
						continue 'r1;
					}
				}
				// forced empty cell
				return Err(Unsolvable);
			}
		}

		Ok(single_applied)
	}

	#[inline(always)]
	fn updn(
		&mut self,
		shrink: &mut u32,
		a: &mut u32,
		b: &mut u32,
		c: &mut u32,
		s: &mut u32,
		i: u32,
		j: u32,
		k: u32,
		l: u32,
	) -> Result<(), Unsolvable> {
		*a = self.bands[(i * 3 + j) as usize];
		*shrink = shrink_mask(*a & LOW9) | shrink_mask(*a >> 9 & LOW9) << 3 | shrink_mask(*a >> 18 & LOW9) << 6;
		*a &= complex_mask(*shrink);
		if *a == 0 {
			return Err(Unsolvable);
		}
		*b = self.bands[(i * 3 + k) as usize];
		*c = self.bands[(i * 3 + l) as usize];
		*s = (*a | *a >> 9 | *a >> 18) & LOW9;
		self.bands[(i*3 + l) as usize] &= mask_single(*s);
		self.bands[(i*3 + k) as usize] &= mask_single(*s);
		*s = row_uniq(
			shrink_single(*shrink) & column_single(*s)
		);

		self.prev_bands[(i * 3 + j) as usize] = *a;
		self.bands[(i * 3 + j) as usize] = *a;
		Ok(())
	}

	#[inline(always)]
	fn upwcl(&mut self, cl: &mut u32, a: u32, s: u32, i: u32, p: u32, q: u32, r: u32, t: u32, u: u32, v: u32, w: u32, x: u32) {
		*cl = !(a & row_mask(s));
		self.unsolved_cells[i as usize] &= *cl;
		self.bands[p as usize] &= *cl;
		self.bands[q as usize] &= *cl;
		self.bands[r as usize] &= *cl;
		self.bands[t as usize] &= *cl;
		self.bands[u as usize] &= *cl;
		self.bands[v as usize] &= *cl;
		self.bands[w as usize] &= *cl;
		self.bands[x as usize] &= *cl;
	}

	/*
	#[inline(always)]
	fn upwcl_slice(&mut self, cl: &mut u32, a: u32, s: u32, args: [u32; 9]) {
		*cl = !(a & *index(&ROW_MASK, s as usize));
		self.unsolved_cells[args[0] as usize] &= *cl;
		for &idx in &args[1..] {
			self.bands[idx as usize] &= *cl;
		}
	}
	*/

	fn update(&mut self) -> Result<(), Unsolvable> {
		let mut shrink: u32 = 1;
		let (mut s, mut a, mut b, mut c, mut cl) = (0, 0, 0, 0, 0);
		while shrink != 0 {
			shrink = 0;
			// -------------------- loop --------------------------
			// part is band? maybe, the real bands not's called band elsewhere (as of when the terms band, slice are still used)
			/*
			for part in 0..3u32 {
				if self.unsolved_rows[part as usize] == 0 { continue }
				let mut ar = self.unsolved_rows[part as usize];
				for digit_offset in 0..3u32 {
					let digit = part*3 + digit_offset;
					if (ar >> (part*3)) & LOW9 == 0 { continue }
					for slice_offset in 0..3u32 {
						let idx = (digit * 3 + slice_offset) as usize;
						if self.bands[idx] != self.prev_bands[idx] {
							self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit,
								slice_offset, (slice_offset + 1) % 3, (slice_offset + 2) % 3)?;

							let ar_offset = (digit_offset * 3 + slice_offset)*3;
							if (ar >> ar_offset) & 0b111 != s {
								ar &= (ALL ^ (0b111 << ar_offset)) | s;

								// actually, first argument (left out here) is not that special
								let mut args = [
									slice_offset,
									slice_offset + 3,
									slice_offset + 6,
									slice_offset + 9,
									slice_offset + 12,
									slice_offset + 15,
									slice_offset + 18,
									slice_offset + 21,
								];
								args[digit as usize..].iter_mut()
									.for_each(|num| {
										*num += 3;
									});
								self.upwcl(&mut cl, a, s,
									slice_offset,
									args[0],
									args[1],
									args[2],
									args[3],
									args[4],
									args[5],
									args[6],
									args[7],
								);
							}
						}
					}
				}
				self.unsolved_rows[part as usize] = ar;
			}
			*/
			/*
			for part in 0..3 {
				if self.unsolved_rows[part as usize] == 0 { continue }
				let mut ar = self.unsolved_rows[part as usize];
				////////////////// DIGIT 0
				if ar & LOW9 != 0 {
					let digit = 0 + part*3;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if ar & 0b111 != s {
							ar &= 0o777_777_770 | s;
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 0]);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 3) & 0b111 != s {
							ar &= 0o777_777_707 | (s << 3);
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 1]);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 6) & 0b111 != s {
							ar &= 0o777_777_077 | (s << 6);
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 2]);
						}
					}
				}

				////////////////// DIGIT 1
				if (ar >> 9) & LOW9 != 0 {
					let digit = 1 + part*3;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if (ar >> 9) & 0b111 != s {
							ar &= 0o777_770_777 | (s << 9);
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 0]);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 12) & 0b111 != s {
							ar &= 0o777_707_777 | (s << 12);
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 1]);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 15) & 0b111 != s {
							ar &= 0o777_077_777 | (s << 15);
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 2]);
						}
					}
				}

				////////////////// DIGIT 2
				if (ar >> 18) & LOW9 != 0 {
					let digit = 2 + part*3;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if (ar >> 18) & 0b111 != s {
							ar &= 0o770_777_777 | (s << 18);
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 0]);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 21) & 0b111 != s {
							ar &= 0o707_777_777 | (s << 21);
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 1]);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 24) & 0b111 != s {
							ar &= 0o077_777_777 | (s << 24);
							self.upwcl_slice(&mut cl, a, s, UPWCL_ARGS[3*digit as usize + 2]);
						}
					}
				}

				self.unsolved_rows[part as usize] = ar;
			}
			*/
			// ------------------ unrolled ------------------------

			if self.unsolved_rows[0] != 0 {
				let mut ar = self.unsolved_rows[0];
				////////////////// DIGIT 0
				if ar & LOW9 != 0 {
					let digit = 0;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if ar & 0b111 != s {
							ar &= 0o777_777_770 | s;
							self.upwcl(&mut cl, a, s, 0, 3, 6, 9, 12, 15, 18, 21, 24);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 3) & 0b111 != s {
							ar &= 0o777_777_707 | (s << 3);
							self.upwcl(&mut cl, a, s, 1, 4, 7, 10, 13, 16, 19, 22, 25);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 6) & 0b111 != s {
							ar &= 0o777_777_077 | (s << 6);
							self.upwcl(&mut cl, a, s, 2, 5, 8, 11, 14, 17, 20, 23, 26);
						}
					}
				}

				////////////////// DIGIT 1
				if (ar >> 9) & LOW9 != 0 {
					let digit = 1;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if (ar >> 9) & 0b111 != s {
							ar &= 0o777_770_777 | (s << 9);
							self.upwcl(&mut cl, a, s, 0, 0, 6, 9, 12, 15, 18, 21, 24);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 12) & 0b111 != s {
							ar &= 0o777_707_777 | (s << 12);
							self.upwcl(&mut cl, a, s, 1, 1, 7, 10, 13, 16, 19, 22, 25);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 15) & 0b111 != s {
							ar &= 0o777_077_777 | (s << 15);
							self.upwcl(&mut cl, a, s, 2, 2, 8, 11, 14, 17, 20, 23, 26);
						}
					}
				}

				////////////////// DIGIT 2
				if (ar >> 18) & LOW9 != 0 {
					let digit = 2;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if (ar >> 18) & 0b111 != s {
							ar &= 0o770_777_777 | (s << 18);
							self.upwcl(&mut cl, a, s, 0, 0, 3, 9, 12, 15, 18, 21, 24);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 21) & 0b111 != s {
							ar &= 0o707_777_777 | (s << 21);
							self.upwcl(&mut cl, a, s, 1, 1, 4, 10, 13, 16, 19, 22, 25);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 24) & 0b111 != s {
							ar &= 0o077_777_777 | (s << 24);
							self.upwcl(&mut cl, a, s, 2, 2, 5, 11, 14, 17, 20, 23, 26);
						}
					}
				}

				self.unsolved_rows[0] = ar;
			}

			if self.unsolved_rows[1] != 0 {
				let mut ar = self.unsolved_rows[1];
				////////////////// DIGIT 3
				if ar & LOW9 != 0 {
					let digit = 3;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if ar & 0b111 != s {
							ar &= 0o777_777_770 | s;
							self.upwcl(&mut cl, a, s, 0, 0, 3, 6, 12, 15, 18, 21, 24);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 3) & 0b111 != s {
							ar &= 0o777_777_707 | (s << 3);
							self.upwcl(&mut cl, a, s, 1, 1, 4, 7, 13, 16, 19, 22, 25);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 6) & 0b111 != s {
							ar &= 0o777_777_077 | (s << 6);
							self.upwcl(&mut cl, a, s, 2, 2, 5, 8, 14, 17, 20, 23, 26);
						}
					}
				}

				////////////////// DIGIT 4
				if (ar >> 9) & LOW9 != 0 {
					let digit = 4;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if (ar >> 9) & 0b111 != s {
							ar &= 0o777_770_777 | (s << 9);
							self.upwcl(&mut cl, a, s, 0, 0, 3, 6, 9, 15, 18, 21, 24);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 12) & 0b111 != s {
							ar &= 0o777_707_777 | (s << 12);
							self.upwcl(&mut cl, a, s, 1, 1, 4, 7, 10, 16, 19, 22, 25);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 15) & 0b111 != s {
							ar &= 0o777_077_777 | (s << 15);
							self.upwcl(&mut cl, a, s, 2, 2, 5, 8, 11, 17, 20, 23, 26);
						}
					}
				}

				////////////////// DIGIT 5
				if (ar >> 18) & LOW9 != 0 {
					let digit = 5;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if (ar >> 18) & 0b111 != s {
							ar &= 0o770_777_777 | (s << 18);
							self.upwcl(&mut cl, a, s, 0, 0, 3, 6, 9, 12, 18, 21, 24);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 21) & 0b111 != s {
							ar &= 0o707_777_777 | (s << 21);
							self.upwcl(&mut cl, a, s, 1, 1, 4, 7, 10, 13, 19, 22, 25);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 24) & 0b111 != s {
							ar &= 0o077_777_777 | (s << 24);
							self.upwcl(&mut cl, a, s, 2, 2, 5, 8, 11, 14, 20, 23, 26);
						}
					}
				}

				self.unsolved_rows[1] = ar;
			}

			if self.unsolved_rows[2] != 0 {
				let mut ar = self.unsolved_rows[2];
				////////////////// DIGIT 6
				if ar & LOW9 != 0 {
					let digit = 6;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if ar & 0b111 != s {
							ar &= 0o777_777_770 | s;
							self.upwcl(&mut cl, a, s, 0, 0, 3, 6, 9, 12, 15, 21, 24);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 3) & 0b111 != s {
							ar &= 0o777_777_707 | (s << 3);
							self.upwcl(&mut cl, a, s, 1, 1, 4, 7, 10, 13, 16, 22, 25);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 6) & 0b111 != s {
							ar &= 0o777_777_077 | (s << 6);
							self.upwcl(&mut cl, a, s, 2, 2, 5, 8, 11, 14, 17, 23, 26);
						}
					}
				}

				////////////////// DIGIT 7
				if (ar >> 9) & LOW9 != 0 {
					let digit = 7;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if (ar >> 9) & 0b111 != s {
							ar &= 0o777_770_777 | (s << 9);
							self.upwcl(&mut cl, a, s, 0, 0, 3, 6, 9, 12, 15, 18, 24);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 12) & 0b111 != s {
							ar &= 0o777_707_777 | (s << 12);
							self.upwcl(&mut cl, a, s, 1, 1, 4, 7, 10, 13, 16, 19, 25);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 15) & 0b111 != s {
							ar &= 0o777_077_777 | (s << 15);
							self.upwcl(&mut cl, a, s, 2, 2, 5, 8, 11, 14, 17, 20, 26);
						}
					}
				}

				////////////////// DIGIT 8
				if (ar >> 18) & LOW9 != 0 {
					let digit = 8;
					if self.bands[digit as usize * 3+0] != self.prev_bands[digit as usize * 3+0] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 0, 1, 2)?;
						if (ar >> 18) & 0b111 != s {
							ar &= 0o770_777_777 | (s << 18);
							self.upwcl(&mut cl, a, s, 0, 0, 3, 6, 9, 12, 15, 18, 21);
						}
					}

					if self.bands[digit as usize * 3+1] != self.prev_bands[digit as usize * 3+1] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 1, 0, 2)?;
						if (ar >> 21) & 0b111 != s {
							ar &= 0o707_777_777 | (s << 21);
							self.upwcl(&mut cl, a, s, 1, 1, 4, 7, 10, 13, 16, 19, 22);
						}
					}

					if self.bands[digit as usize * 3+2] != self.prev_bands[digit as usize * 3+2] {
						self.updn(&mut shrink, &mut a, &mut b, &mut c, &mut s, digit, 2, 0, 1)?;
						if (ar >> 24) & 0b111 != s {
							ar &= 0o077_777_777 | (s << 24);
							self.upwcl(&mut cl, a, s, 2, 2, 5, 8, 11, 14, 17, 20, 23);
						}
					}
				}

				self.unsolved_rows[2] = ar;
			}

		// ----------------------------------------------------
		}
		Ok(())
	}

	fn full_update(&mut self, limit: usize, solutions: &mut Solutions) -> Result<(), Unsolvable> {
		debug_assert!(solutions.len() <= limit);
		if solutions.len() == limit {
			return Err(Unsolvable); // not really, but it forces a recursion stop
		}
		loop {
			self.update()?;
			if self.is_solved() {
				return Ok(());
			}
			// if singles found, go again
			if self.find_singles()? {
				continue
			}
			return Ok(())
		}
	}

	fn is_solved(&self) -> bool {
		self.unsolved_cells.0 == [0; 3]
	}

	fn guess(&mut self, solver_stack: &mut SolvStack, limit: usize, solutions: &mut Solutions) {
		if self.is_solved() {
			debug_assert!(solutions.len() < limit, "too many solutions in guess: limit: {}, len: {}", limit, solutions.len());
			match *solutions {
				Solutions::Count(ref mut count) => *count += 1,
				Solutions::Vector(ref mut vec) => vec.push( self.extract_solution() )
			}
		} else if self.guess_bivalue_in_cell(solver_stack, limit, solutions).is_ok() {
			// .is_ok() == found nothing
			self.guess_first_cell(solver_stack, limit, solutions);
		}
	}

	fn guess_bivalue_in_cell(&mut self, solver_stack: &mut SolvStack, limit: usize, solutions: &mut Solutions) -> Result<(), Unsolvable> {
		for band in 0..3 {
			let mut pairs = self.pairs[band as usize];
			if pairs != 0 {
				let cell_mask = pairs & (!pairs + 1);
				let mut slice = band;
				//let mut digit = 0;

				// Both of the next two loops repeat until they find
				// a digit in the cell
				// by construction there are guaranteed to be exactly 2 digits
				// the first loop will try the digit on a clone of the current state
				// the second one will try on the current state
				// because all possibilities will be exhausted after that
				loop {
					if self.bands[slice as usize] & cell_mask != 0 {
						let mut solver = self.clone();

                        // FIXME: find out whether it's possible for this to err
                        let _ = solver._set_solved_mask(slice, cell_mask);
						if solver.full_update(limit, solutions).is_ok() {
							solver.guess(solver_stack, limit, solutions);
						}
						self.bands[slice as usize] ^= cell_mask;
						break
					}

					//digit += 1;
					slice += 3;
					//debug_assert!(digit < 8);
					debug_assert!(slice < 24);
				}

				// No need to backtrack on second number. Possibilities are exhausted
				loop {
					// increment immediately because previous loop
					// ended without incrementation
					//digit += 1;
					slice += 3;
					//debug_assert!(digit < 9);
					debug_assert!(slice < 27);
					if self.bands[slice as usize] & cell_mask != 0 {

                        // FIXME: find out whether it's possible for this to err
						let _ = self._set_solved_mask(slice, cell_mask);
						if self.full_update(limit, solutions).is_ok() {
							self.guess(solver_stack, limit, solutions);
						}
						// no possibilities left
						return Err(Unsolvable);
					}
				}
			}
		}
		// no pairs found
		Ok(())
	}

	// iterates through every possibility and calls guess()
	// guess() short circuits when enough solutions were found and therefore
	// so does this
	fn guess_first_cell(&mut self, solver_stack: &mut SolvStack, limit: usize, solutions: &mut Solutions) {
		// guess all possibilities in the first unsolved cell encountered
		for band in 0..3 {
			let mut unsolved_cells = self.unsolved_cells[band as usize];
			if unsolved_cells == 0 {
				continue
			}
			let one_unsolved_cell = unsolved_cells & (!unsolved_cells + 1);
			let mut slice = band;
			// check every digit
			for _ in 0..9 {
				if self.bands[slice as usize] & one_unsolved_cell != 0 {
					let mut solver = self.clone();

                    // FIXME: find out whether it's possible for this to err
					let _ = solver._set_solved_mask(slice, one_unsolved_cell);
					if solver.full_update(limit, solutions).is_ok() {
						solver.guess(solver_stack, limit, solutions);
					}
					self.bands[slice as usize] ^= one_unsolved_cell;
				}

				slice += 3;
			}
			break
		}
	}

	fn _solve_at_most(mut self, limit: usize, solutions: &mut Solutions) {
		if self.find_singles().is_err() { return }

		// either solved or impossible
		// ORIGBUG: jczsolver fails on solved sudokus?
		if self.full_update(limit, solutions).is_err() { return }
		self.guess(&mut vec![], limit, solutions);
	}

	pub fn solve_at_most(self, limit: usize) -> Vec<Sudoku> {
		let mut solutions = Solutions::Vector(vec![]);
		self._solve_at_most(limit, &mut solutions);
		solutions.into_vec().unwrap()
	}

	pub fn count_at_most(self, limit: usize) -> usize {
		let mut solutions = Solutions::Count(0);
		self._solve_at_most(limit, &mut solutions);
		solutions.len()
	}
}

#[inline]
fn mask_of_cell(cell: u8) -> u32 {
	/*
	static MASK_OF_CELL: [u32; 81] = [
		0x1,	0x2,	0x4,	0x8,	0x10,	0x20,	0x40,	0x80,	0x100,
		0x200,	0x400,	0x800,	0x1000,	0x2000,	0x4000,	0x8000,	0x10000,	0x20000,
		0x40000,	0x80000,	0x100000,	0x200000,	0x400000,	0x800000,	0x1000000,	0x2000000,	0x4000000,
		0x1,	0x2,	0x4,	0x8,	0x10,	0x20,	0x40,	0x80,	0x100,
		0x200,	0x400,	0x800,	0x1000,	0x2000,	0x4000,	0x8000,	0x10000,	0x20000,
		0x40000,	0x80000,	0x100000,	0x200000,	0x400000,	0x800000,	0x1000000,	0x2000000,	0x4000000,
		0x1,	0x2,	0x4,	0x8,	0x10,	0x20,	0x40,	0x80,	0x100,
		0x200,	0x400,	0x800,	0x1000,	0x2000,	0x4000,	0x8000,	0x10000,	0x20000,
		0x40000,	0x80000,	0x100000,	0x200000,	0x400000,	0x800000,	0x1000000,	0x2000000,	0x4000000,
	];
	*index(&MASK_OF_CELL, cell as usize)
	*/
	1 << mod27(cell)
}

#[inline]
fn digit_to_base(digit: u8) -> u8 {
	/*
	static DIGIT_TO_BASE: [u8; 9] = [
		0,	3,	6,	9,	12,	15,	18,	21,	24,
	];
	*index(&DIGIT_TO_BASE, (digit - 1) as usize)
	*/
	(digit - 1) * 3
}

#[inline]
fn row_of_cell(cell: u8) -> u8 {
	/*
	static ROW_OF_CELL: [u8; 81] = [
		0,	0,	0,	0,	0,	0,	0,	0,	0,
		1,	1,	1,	1,	1,	1,	1,	1,	1,
		2,	2,	2,	2,	2,	2,	2,	2,	2,
		3,	3,	3,	3,	3,	3,	3,	3,	3,
		4,	4,	4,	4,	4,	4,	4,	4,	4,
		5,	5,	5,	5,	5,	5,	5,	5,	5,
		6,	6,	6,	6,	6,	6,	6,	6,	6,
		7,	7,	7,	7,	7,	7,	7,	7,	7,
		8,	8,	8,	8,	8,	8,	8,	8,	8,
	];
	ROW_OF_CELL[cell as usize]
	*/
	cell / 9
}


#[inline]
fn band_of_cell(cell: u8) -> u8 {
	/*
	static BAND_OF_CELL: [u8; 81] = [
		0,	0,	0,	0,	0,	0,	0,	0,	0,
		0,	0,	0,	0,	0,	0,	0,	0,	0,
		0,	0,	0,	0,	0,	0,	0,	0,	0,
		1,	1,	1,	1,	1,	1,	1,	1,	1,
		1,	1,	1,	1,	1,	1,	1,	1,	1,
		1,	1,	1,	1,	1,	1,	1,	1,	1,
		2,	2,	2,	2,	2,	2,	2,	2,	2,
		2,	2,	2,	2,	2,	2,	2,	2,	2,
		2,	2,	2,	2,	2,	2,	2,	2,	2,
	];
	*index(&BAND_OF_CELL, cell as usize)
	*/
	cell / 27
}

#[inline]
fn self_mask(cell: u8) -> u32 {
	// ???
	static SELF_MASK: [u32; 81] = [
		0x37E3F001,	0x37E3F002,	0x37E3F004,	0x371F8E08,	0x371F8E10,	0x371F8E20,	0x30FC7E40,	0x30FC7E80,
		0x30FC7F00,	0x2FE003F8,	0x2FE005F8,	0x2FE009F8,	0x2F1C11C7,	0x2F1C21C7,	0x2F1C41C7,	0x28FC803F,
		0x28FD003F,	0x28FE003F,	0x1807F1F8,	0x180BF1F8,	0x1813F1F8,	0x18238FC7,	0x18438FC7,	0x18838FC7,
		0x19007E3F,	0x1A007E3F,	0x1C007E3F,	0x37E3F001,	0x37E3F002,	0x37E3F004,	0x371F8E08,	0x371F8E10,
		0x371F8E20,	0x30FC7E40,	0x30FC7E80,	0x30FC7F00,	0x2FE003F8,	0x2FE005F8,	0x2FE009F8,	0x2F1C11C7,
		0x2F1C21C7,	0x2F1C41C7,	0x28FC803F,	0x28FD003F,	0x28FE003F,	0x1807F1F8,	0x180BF1F8,	0x1813F1F8,
		0x18238FC7,	0x18438FC7,	0x18838FC7,	0x19007E3F,	0x1A007E3F,	0x1C007E3F,	0x37E3F001,	0x37E3F002,
		0x37E3F004,	0x371F8E08,	0x371F8E10,	0x371F8E20,	0x30FC7E40,	0x30FC7E80,	0x30FC7F00,	0x2FE003F8,
		0x2FE005F8,	0x2FE009F8,	0x2F1C11C7,	0x2F1C21C7,	0x2F1C41C7,	0x28FC803F,	0x28FD003F,	0x28FE003F,
		0x1807F1F8,	0x180BF1F8,	0x1813F1F8,	0x18238FC7,	0x18438FC7,	0x18838FC7,	0x19007E3F,	0x1A007E3F,
		0x1C007E3F,
	];
	*index(&SELF_MASK, cell as usize)
}

// ----------------------------------------------------------------
//  					solver indexing
// ----------------------------------------------------------------
// These functions are only for use in the solver to conditionally
// compile bounds checks in array accesses
// the value space for indexes is limited enough that any error
// is likely to immediately show up in tests
// ----------------------------------------------------------------

#[inline(always)]
fn index<T>(slice: &[T], idx: usize) -> &T {
	if cfg!(feature = "unchecked_indexing") {
		debug_assert!(idx < slice.len());
		unsafe { slice.get_unchecked(idx) }
	} else {
		&slice[idx]
	}
}

#[inline(always)]
fn index_mut<T>(slice: &mut [T], idx: usize) -> &mut T {
	if cfg!(feature = "unchecked_indexing") {
		debug_assert!(idx < slice.len());
		unsafe { slice.get_unchecked_mut(idx) }
	} else {
		&mut slice[idx]
	}
}
// ----------------------------------------------------------------

#[inline]
fn other_mask(cell: u8) -> u32 {
	// ???
	static OTHER_MASK: [u32; 81] = [
		0x3FFBFDFE,	0x3FF7FBFD,	0x3FEFF7FB,	0x3FDFEFF7,	0x3FBFDFEF,	0x3F7FBFDF,	0x3EFF7FBF,	0x3DFEFF7F,
		0x3BFDFEFF,	0x3FFBFDFE,	0x3FF7FBFD,	0x3FEFF7FB,	0x3FDFEFF7,	0x3FBFDFEF,	0x3F7FBFDF,	0x3EFF7FBF,
		0x3DFEFF7F,	0x3BFDFEFF,	0x3FFBFDFE,	0x3FF7FBFD,	0x3FEFF7FB,	0x3FDFEFF7,	0x3FBFDFEF,	0x3F7FBFDF,
		0x3EFF7FBF,	0x3DFEFF7F,	0x3BFDFEFF,	0x3FFBFDFE,	0x3FF7FBFD,	0x3FEFF7FB,	0x3FDFEFF7,	0x3FBFDFEF,
		0x3F7FBFDF,	0x3EFF7FBF,	0x3DFEFF7F,	0x3BFDFEFF,	0x3FFBFDFE,	0x3FF7FBFD,	0x3FEFF7FB,	0x3FDFEFF7,
		0x3FBFDFEF,	0x3F7FBFDF,	0x3EFF7FBF,	0x3DFEFF7F,	0x3BFDFEFF,	0x3FFBFDFE,	0x3FF7FBFD,	0x3FEFF7FB,
		0x3FDFEFF7,	0x3FBFDFEF,	0x3F7FBFDF,	0x3EFF7FBF,	0x3DFEFF7F,	0x3BFDFEFF,	0x3FFBFDFE,	0x3FF7FBFD,
		0x3FEFF7FB,	0x3FDFEFF7,	0x3FBFDFEF,	0x3F7FBFDF,	0x3EFF7FBF,	0x3DFEFF7F,	0x3BFDFEFF,	0x3FFBFDFE,
		0x3FF7FBFD,	0x3FEFF7FB,	0x3FDFEFF7,	0x3FBFDFEF,	0x3F7FBFDF,	0x3EFF7FBF,	0x3DFEFF7F,	0x3BFDFEFF,
		0x3FFBFDFE,	0x3FF7FBFD,	0x3FEFF7FB,	0x3FDFEFF7,	0x3FBFDFEF,	0x3F7FBFDF,	0x3EFF7FBF,	0x3DFEFF7F,
		0x3BFDFEFF,
	];
	*index(&OTHER_MASK, cell as usize)
}

#[inline]
fn shrink_mask(thing: u32) -> u32 {
	*index(&SHRINK_MASK, thing as usize) as u32
}

#[inline]
fn complex_mask(thing: u32) -> u32 {
	*index(&COMPLEX_MASK, thing as usize)
}

#[inline]
fn mask_single(thing: u32) -> u32 {
	*index(&MASK_SINGLE, thing as usize)
}

#[inline]
fn column_single(thing: u32) -> u32 {
	*index(&COLUMN_SINGLE, thing as usize) as u32
}

#[inline]
fn shrink_single(thing: u32) -> u32 {
	*index(&SHRINK_SINGLE, thing as usize)
}

#[inline]
fn row_uniq(thing: u32) -> u32 {
	*index(&ROW_UNIQ, thing as usize)  as u32
}

#[inline]
fn row_mask(thing: u32) -> u32 {
	static ROW_MASK: [u32; 8] = [	// rows where single  found _000 to 111
		0o777777777, 0o777777000, 0o777000777, 0o777000000, 0o777777, 0o777000, 0o777, 0o0,
	];
	*index(&ROW_MASK, thing as usize)
	//(!thing & 0b1) * 511 + (!thing & 0b10) * 130816 + (!thing & 0b100) * 33488896
}

#[inline]
fn mod3(num: u8) -> u8 {
	/*
	static MOD3: [u8; 27] = [
		0,	1,	2,	0,	1,	2,	0,	1,	2,
		0,	1,	2,	0,	1,	2,	0,	1,	2,
		0,	1,	2,	0,	1,	2,	0,	1,	2,
	];
	MOD3[num as usize]
	*/
	num % 3
}

#[inline]
fn mod27(num: u8) -> u8 {
	/*
	static MOD27: [u8; 81] = [
		0,	1,	2,	3,	4,	5,	6,	7,	8,
		9,	10,	11,	12,	13,	14,	15,	16,	17,
		18,	19,	20,	21,	22,	23,	24,	25,	26,
		0,	1,	2,	3,	4,	5,	6,	7,	8,
		9,	10,	11,	12,	13,	14,	15,	16,	17,
		18,	19,	20,	21,	22,	23,	24,	25,	26,
		0,	1,	2,	3,	4,	5,	6,	7,	8,
		9,	10,	11,	12,	13,	14,	15,	16,	17,
		18,	19,	20,	21,	22,	23,	24,	25,	26,
	];
	MOD27[num as usize]
	*/
	num % 27
}

#[inline]
fn neighbour_slices(slice: u8) -> (u8, u8) {
	static NEIGHBOUR_SLICES: [(u8, u8); 27] = [
		(1, 2), (2, 0), (0, 1),
		(4, 5), (5, 3), (3, 4),
		(7, 8), (8, 6), (6, 7),
		(10, 11), (11, 9), (9, 10),
		(13, 14), (14, 12), (12, 13),
		(16, 17), (17, 15), (15, 16),
		(19, 20), (20, 18), (18, 19),
		(22, 23), (23, 21), (21, 22),
		(25, 26), (26, 24), (24, 25),
	];
	*index(&NEIGHBOUR_SLICES, slice as usize)
}

static SHRINK_MASK: [u8; 512] = [
	0, 1, 1, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3,
	2, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3, 3,
	4, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	4, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	4, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	4, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	4, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	4, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	4, 5, 5, 5, 5, 5, 5, 5, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
	6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7,
];

static COMPLEX_MASK: [u32; 512] = [	// keep mini rows still valid
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0o7007070700, 0o7707070700, 0o7007770700, 0o7707770700,
	0, 0, 0, 0, 0o7077070700, 0o7777070700, 0o7777770700, 0o7777770700,
	0, 0, 0o7007700070, 0o7077700070, 0, 0, 0o7007770070, 0o7077770070,
	0, 0, 0o7707700070, 0o7777700070, 0, 0, 0o7777770070, 0o7777770070,
	0, 0, 0o7007700770, 0o7777700770, 0o7007070770, 0o7777070770, 0o7007770770, 0o7777770770,
	0, 0, 0o7707700770, 0o7777700770, 0o7077070770, 0o7777070770, 0o7777770770, 0o7777770770,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0o7070007700, 0o7070707700, 0o7770007700, 0o7770707700,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0o7077007700, 0o7777707700, 0o7777007700, 0o7777707700,
	0, 0o7070700007, 0, 0o7077700007, 0, 0o7070707007, 0, 0o7077707007,
	0, 0o7070700707, 0, 0o7777700707, 0o7070007707, 0o7070707707, 0o7777007707, 0o7777707707,
	0, 0o7770700007, 0, 0o7777700007, 0, 0o7777707007, 0, 0o7777707007,
	0, 0o7770700707, 0, 0o7777700707, 0o7077007707, 0o7777707707, 0o7777007707, 0o7777707707,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0o7070077700, 0o7070777700, 0o7770777700, 0o7770777700,
	0, 0, 0, 0, 0o7007077700, 0o7707777700, 0o7007777700, 0o7707777700,
	0, 0, 0, 0, 0o7077077700, 0o7777777700, 0o7777777700, 0o7777777700,
	0, 0o7070700077, 0o7007700077, 0o7077700077, 0, 0o7070777077, 0o7007777077, 0o7077777077,
	0, 0o7070700777, 0o7707700777, 0o7777700777, 0o7070077777, 0o7070777777, 0o7777777777, 0o7777777777,
	0, 0o7770700777, 0o7007700777, 0o7777700777, 0o7007077777, 0o7777777777, 0o7007777777, 0o7777777777,
	0, 0o7770700777, 0o7707700777, 0o7777700777, 0o7077077777, 0o7777777777, 0o7777777777, 0o7777777777,
	0, 0, 0, 0, 0, 0, 0, 0,
	0o0, 0, 0o7700007070, 0o7700077070, 0, 0, 0o7770007070, 0o7770077070,
	0o0, 0o7700070007, 0, 0o7700077007, 0, 0o7707070007, 0, 0o7707077007,
	0o0, 0o7700070077, 0o7700007077, 0o7700077077, 0, 0o7777070077, 0o7777007077, 0o7777077077,
	0o0, 0, 0, 0, 0, 0, 0, 0,
	0o0, 0, 0o7707007070, 0o7777077070, 0, 0, 0o7777007070, 0o7777077070,
	0o0, 0o7770070007, 0, 0o7777077007, 0, 0o7777070007, 0, 0o7777077007,
	0o0, 0o7770070077, 0o7707007077, 0o7777077077, 0, 0o7777070077, 0o7777007077, 0o7777077077,
	0o0, 0, 0, 0, 0, 0, 0, 0,
	0o0, 0, 0o7700707070, 0o7700777070, 0, 0, 0o7770777070, 0o7770777070,
	0o0, 0o7700070707, 0, 0o7700777707, 0o7007070707, 0o7707070707, 0o7007777707, 0o7707777707,
	0o0, 0o7700070777, 0o7700707777, 0o7700777777, 0o7077070777, 0o7777070777, 0o7777777777, 0o7777777777,
	0o0, 0, 0o7007707070, 0o7077777070, 0, 0, 0o7007777070, 0o7077777070,
	0o0, 0, 0o7707707070, 0o7777777070, 0, 0, 0o7777777070, 0o7777777070,
	0o0, 0o7770070777, 0o7007707777, 0o7777777777, 0o7007070777, 0o7777070777, 0o7007777777, 0o7777777777,
	0o0, 0o7770070777, 0o7707707777, 0o7777777777, 0o7077070777, 0o7777070777, 0o7777777777, 0o7777777777,
	0o0, 0, 0, 0, 0, 0, 0, 0,
	0o0, 0, 0o7700007770, 0o7700777770, 0o7070007770, 0o7070777770, 0o7770007770, 0o7770777770,
	0o0, 0o7700770007, 0, 0o7700777007, 0, 0o7707777007, 0, 0o7707777007,
	0o0, 0o7700770777, 0o7700007777, 0o7700777777, 0o7077007777, 0o7777777777, 0o7777007777, 0o7777777777,
	0o0, 0o7070770007, 0, 0o7077777007, 0, 0o7070777007, 0, 0o7077777007,
	0o0, 0o7070770777, 0o7707007777, 0o7777777777, 0o7070007777, 0o7070777777, 0o7777007777, 0o7777777777,
	0o0, 0o7770770007, 0, 0o7777777007, 0, 0o7777777007, 0, 0o7777777007,
	0o0, 0o7770770777, 0o7707007777, 0o7777777777, 0o7077007777, 0o7777777777, 0o7777007777, 0o7777777777,
	0o0, 0, 0, 0, 0, 0, 0, 0,
	0o0, 0, 0o7700707770, 0o7700777770, 0o7070077770, 0o7070777770, 0o7770777770, 0o7770777770,
	0o0, 0o7700770707, 0, 0o7700777707, 0o7007077707, 0o7707777707, 0o7007777707, 0o7707777707,
	0o0, 0o7700770777, 0o7700707777, 0o7700777777, 0o7077077777, 0o7777777777, 0o7777777777, 0o7777777777,
	0o0, 0o7070770077, 0o7007707077, 0o7077777077, 0, 0o7070777077, 0o7007777077, 0o7077777077,
	0o0, 0o7070770777, 0o7707707777, 0o7777777777, 0o7070077777, 0o7070777777, 0o7777777777, 0o7777777777,
	0o0, 0o7770770777, 0o7007707777, 0o7777777777, 0o7007077777, 0o7777777777, 0o7007777777, 0o7777777777,
	0o0, 0o7770770777, 0o7707707777, 0o7777777777, 0o7077077777, 0o7777777777, 0o7777777777, 0o7777777777,
];

static MASK_SINGLE: [u32; 512] = [	// kill in other blocks locked column /box
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7767767767, 0o7766766766, 0o7765765765, 0o7767767767, 0o7763763763, 0o7767767767, 0o7767767767, 0o7767767767,
	0o7757757757, 0o7756756756, 0o7755755755, 0o7757757757, 0o7753753753, 0o7757757757, 0o7757757757, 0o7757757757,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7737737737, 0o7736736736, 0o7735735735, 0o7737737737, 0o7733733733, 0o7737737737, 0o7737737737, 0o7737737737,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7677677677, 0o7676676676, 0o7675675675, 0o7677677677, 0o7673673673, 0o7677677677, 0o7677677677, 0o7677677677,
	0o7667667667, 0o7666666666, 0o7665665665, 0o7667667667, 0o7663663663, 0o7667667667, 0o7667667667, 0o7667667667,
	0o7657657657, 0o7656656656, 0o7655655655, 0o7657657657, 0o7653653653, 0o7657657657, 0o7657657657, 0o7657657657,
	0o7677677677, 0o7676676676, 0o7675675675, 0o7677677677, 0o7673673673, 0o7677677677, 0o7677677677, 0o7677677677,
	0o7637637637, 0o7636636636, 0o7635635635, 0o7637637637, 0o7633633633, 0o7637637637, 0o7637637637, 0o7637637637,
	0o7677677677, 0o7676676676, 0o7675675675, 0o7677677677, 0o7673673673, 0o7677677677, 0o7677677677, 0o7677677677,
	0o7677677677, 0o7676676676, 0o7675675675, 0o7677677677, 0o7673673673, 0o7677677677, 0o7677677677, 0o7677677677,
	0o7677677677, 0o7676676676, 0o7675675675, 0o7677677677, 0o7673673673, 0o7677677677, 0o7677677677, 0o7677677677,
	0o7577577577, 0o7576576576, 0o7575575575, 0o7577577577, 0o7573573573, 0o7577577577, 0o7577577577, 0o7577577577,
	0o7567567567, 0o7566566566, 0o7565565565, 0o7567567567, 0o7563563563, 0o7567567567, 0o7567567567, 0o7567567567,
	0o7557557557, 0o7556556556, 0o7555555555, 0o7557557557, 0o7553553553, 0o7557557557, 0o7557557557, 0o7557557557,
	0o7577577577, 0o7576576576, 0o7575575575, 0o7577577577, 0o7573573573, 0o7577577577, 0o7577577577, 0o7577577577,
	0o7537537537, 0o7536536536, 0o7535535535, 0o7537537537, 0o7533533533, 0o7537537537, 0o7537537537, 0o7537537537,
	0o7577577577, 0o7576576576, 0o7575575575, 0o7577577577, 0o7573573573, 0o7577577577, 0o7577577577, 0o7577577577,
	0o7577577577, 0o7576576576, 0o7575575575, 0o7577577577, 0o7573573573, 0o7577577577, 0o7577577577, 0o7577577577,
	0o7577577577, 0o7576576576, 0o7575575575, 0o7577577577, 0o7573573573, 0o7577577577, 0o7577577577, 0o7577577577,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7767767767, 0o7766766766, 0o7765765765, 0o7767767767, 0o7763763763, 0o7767767767, 0o7767767767, 0o7767767767,
	0o7757757757, 0o7756756756, 0o7755755755, 0o7757757757, 0o7753753753, 0o7757757757, 0o7757757757, 0o7757757757,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7737737737, 0o7736736736, 0o7735735735, 0o7737737737, 0o7733733733, 0o7737737737, 0o7737737737, 0o7737737737,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7377377377, 0o7376376376, 0o7375375375, 0o7377377377, 0o7373373373, 0o7377377377, 0o7377377377, 0o7377377377,
	0o7367367367, 0o7366366366, 0o7365365365, 0o7367367367, 0o7363363363, 0o7367367367, 0o7367367367, 0o7367367367,
	0o7357357357, 0o7356356356, 0o7355355355, 0o7357357357, 0o7353353353, 0o7357357357, 0o7357357357, 0o7357357357,
	0o7377377377, 0o7376376376, 0o7375375375, 0o7377377377, 0o7373373373, 0o7377377377, 0o7377377377, 0o7377377377,
	0o7337337337, 0o7336336336, 0o7335335335, 0o7337337337, 0o7333333333, 0o7337337337, 0o7337337337, 0o7337337337,
	0o7377377377, 0o7376376376, 0o7375375375, 0o7377377377, 0o7373373373, 0o7377377377, 0o7377377377, 0o7377377377,
	0o7377377377, 0o7376376376, 0o7375375375, 0o7377377377, 0o7373373373, 0o7377377377, 0o7377377377, 0o7377377377,
	0o7377377377, 0o7376376376, 0o7375375375, 0o7377377377, 0o7373373373, 0o7377377377, 0o7377377377, 0o7377377377,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7767767767, 0o7766766766, 0o7765765765, 0o7767767767, 0o7763763763, 0o7767767767, 0o7767767767, 0o7767767767,
	0o7757757757, 0o7756756756, 0o7755755755, 0o7757757757, 0o7753753753, 0o7757757757, 0o7757757757, 0o7757757757,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7737737737, 0o7736736736, 0o7735735735, 0o7737737737, 0o7733733733, 0o7737737737, 0o7737737737, 0o7737737737,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7767767767, 0o7766766766, 0o7765765765, 0o7767767767, 0o7763763763, 0o7767767767, 0o7767767767, 0o7767767767,
	0o7757757757, 0o7756756756, 0o7755755755, 0o7757757757, 0o7753753753, 0o7757757757, 0o7757757757, 0o7757757757,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7737737737, 0o7736736736, 0o7735735735, 0o7737737737, 0o7733733733, 0o7737737737, 0o7737737737, 0o7737737737,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7767767767, 0o7766766766, 0o7765765765, 0o7767767767, 0o7763763763, 0o7767767767, 0o7767767767, 0o7767767767,
	0o7757757757, 0o7756756756, 0o7755755755, 0o7757757757, 0o7753753753, 0o7757757757, 0o7757757757, 0o7757757757,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7737737737, 0o7736736736, 0o7735735735, 0o7737737737, 0o7733733733, 0o7737737737, 0o7737737737, 0o7737737737,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
	0o7777777777, 0o7776776776, 0o7775775775, 0o7777777777, 0o7773773773, 0o7777777777, 0o7777777777, 0o7777777777,
];

static SHRINK_SINGLE: [u32; 512] = [	// keep only rows with single
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o124, 0o124, 0o124, 0o124, 0, 0, 0, 0, 0o124, 0o124, 0o124, 0o124,
	0, 0, 0o142, 0o142, 0, 0, 0o142, 0o142, 0, 0, 0o142, 0o142, 0, 0, 0o142, 0o142, 0, 0, 0o142, 0o142, 0o124, 0o124, 0o100, 0o100, 0, 0, 0o142, 0o142, 0o124, 0o124, 0o100, 0o100,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o214, 0o214, 0o214, 0o214, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o214, 0o214, 0o214, 0o214,
	0, 0o241, 0, 0o241, 0, 0o241, 0, 0o241, 0, 0o241, 0, 0o241, 0o214, 0o200, 0o214, 0o200, 0, 0o241, 0, 0o241, 0, 0o241, 0, 0o241, 0, 0o241, 0, 0o241, 0o214, 0o200, 0o214, 0o200,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o214, 0o214, 0o214, 0o214, 0, 0, 0, 0, 0o124, 0o124, 0o124, 0o124, 0, 0, 0, 0, 0o4, 0o4, 0o4, 0o4,
	0, 0o241, 0o142, 0o40, 0, 0o241, 0o142, 0o40, 0, 0o241, 0o142, 0o40, 0o214, 0o200, 0, 0, 0, 0o241, 0o142, 0o40, 0o124, 0, 0o100, 0, 0, 0o241, 0o142, 0o40, 0o4, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o412, 0o412, 0, 0, 0o412, 0o412, 0, 0o421, 0, 0o421, 0, 0o421, 0, 0o421, 0, 0o421, 0o412, 0o400, 0, 0o421, 0o412, 0o400,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o412, 0o412, 0, 0, 0o412, 0o412, 0, 0o421, 0, 0o421, 0, 0o421, 0, 0o421, 0, 0o421, 0o412, 0o400, 0, 0o421, 0o412, 0o400,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o412, 0o412, 0, 0, 0o412, 0o412, 0, 0o421, 0, 0o421, 0o124, 0o20, 0o124, 0o20, 0, 0o421, 0o412, 0o400, 0o124, 0o20, 0, 0,
	0, 0, 0o142, 0o142, 0, 0, 0o142, 0o142, 0, 0, 0o2, 0o2, 0, 0, 0o2, 0o2, 0, 0o421, 0o142, 0, 0o124, 0o20, 0o100, 0, 0, 0o421, 0o2, 0, 0o124, 0o20, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o412, 0o412, 0o214, 0o214, 0o10, 0o10, 0, 0o421, 0, 0o421, 0, 0o421, 0, 0o421, 0, 0o421, 0o412, 0o400, 0o214, 0, 0o10, 0,
	0, 0o241, 0, 0o241, 0, 0o241, 0, 0o241, 0, 0o241, 0o412, 0, 0o214, 0o200, 0o10, 0, 0, 0o1, 0, 0o1, 0, 0o1, 0, 0o1, 0, 0o1, 0o412, 0, 0o214, 0, 0o10, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0o412, 0o412, 0o214, 0o214, 0o10, 0o10, 0, 0o421, 0, 0o421, 0o124, 0o20, 0o124, 0o20, 0, 0o421, 0o412, 0o400, 0o4, 0, 0, 0,
	0, 0o241, 0o142, 0o40, 0, 0o241, 0o142, 0o40, 0, 0o241, 0o2, 0, 0o214, 0o200, 0, 0, 0, 0o1, 0o142, 0, 0o124, 0, 0o100, 0, 0, 0o1, 0o2, 0, 0o4, 0, 0, 0,
];

static ROW_UNIQ: [u8; 512] = [	// 1 is row not defined in block  mode  1 to 111
	7, 6, 6, 6, 6, 6, 6, 6, 5, 4, 4, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4,
	5, 4, 4, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 5, 4, 4, 4, 4, 4, 4, 4,
	3, 2, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	3, 2, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	3, 2, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	3, 2, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	3, 2, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	3, 2, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	3, 2, 2, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
];

static COLUMN_SINGLE: [u32; 512] = [	// single in column applied to shrinked bloc
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666,
	0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666,
	0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222,
	0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666,
	0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444, 0o0, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222,
	0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222,
	0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222,
	0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
	0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0, 0o0, 0o111, 0o111, 0o0, 0o111, 0o0, 0o0, 0o0,
];

/*
static UPWCL_ARGS: [[u32; 9]; 27] = [
    [0, 3, 6, 9, 12, 15, 18, 21, 24],
    [1, 4, 7, 10, 13, 16, 19, 22, 25],
    [2, 5, 8, 11, 14, 17, 20, 23, 26],
    [0, 0, 6, 9, 12, 15, 18, 21, 24],
    [1, 1, 7, 10, 13, 16, 19, 22, 25],
    [2, 2, 8, 11, 14, 17, 20, 23, 26],
    [0, 0, 3, 9, 12, 15, 18, 21, 24],
    [1, 1, 4, 10, 13, 16, 19, 22, 25],
    [2, 2, 5, 11, 14, 17, 20, 23, 26],
    [0, 0, 3, 6, 12, 15, 18, 21, 24],
    [1, 1, 4, 7, 13, 16, 19, 22, 25],
    [2, 2, 5, 8, 14, 17, 20, 23, 26],
    [0, 0, 3, 6, 9, 15, 18, 21, 24],
    [1, 1, 4, 7, 10, 16, 19, 22, 25],
    [2, 2, 5, 8, 11, 17, 20, 23, 26],
    [0, 0, 3, 6, 9, 12, 18, 21, 24],
    [1, 1, 4, 7, 10, 13, 19, 22, 25],
    [2, 2, 5, 8, 11, 14, 20, 23, 26],
    [0, 0, 3, 6, 9, 12, 15, 21, 24],
    [1, 1, 4, 7, 10, 13, 16, 22, 25],
    [2, 2, 5, 8, 11, 14, 17, 23, 26],
    [0, 0, 3, 6, 9, 12, 15, 18, 24],
    [1, 1, 4, 7, 10, 13, 16, 19, 25],
    [2, 2, 5, 8, 11, 14, 17, 20, 26],
    [0, 0, 3, 6, 9, 12, 15, 18, 21],
    [1, 1, 4, 7, 10, 13, 16, 19, 22],
    [2, 2, 5, 8, 11, 14, 17, 20, 23],
];
*/

#[inline(always)]
fn bit_pos(mask: u32) -> u8 {
	/*
	static DE_BRUIJN_FACTOR: [u32; 32] = [
		0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
		31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
	];
	*index(&DE_BRUIJN_FACTOR, (mask.wrapping_mul(0x077CB531)) as usize >> 27)   as u8
	*/
	mask.trailing_zeros() as u8
}
