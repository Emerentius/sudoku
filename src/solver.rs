//	This solver is based on JCZsolve,
//  which is currently (2018-06-26) and to the best of my (emerentius) knowledge the
//  fastest sudoku solver algorithm.
//
//  The first version of jczsolve was published by original author zhouyundong_2012 in:
//      http://forum.enjoysudoku.com/3-77us-solver-2-8g-cpu-testcase-17sodoku-t30470.html
//
//  It was then further improved in collaboration with champagne and JasonLion
//  (these are usernames on forum.enjoysudoku.com)
//
//  The source code used as base for the port was the version by JasonLion from this post:
//      http://forum.enjoysudoku.com/3-77us-solver-2-8g-cpu-testcase-17sodoku-t30470-210.html#p249309
//
//	The main insights and contributions lie in:
//  (First three as stated in the original C source)
//
//	zhouyundong_2012:
//		Original authorship
//  	Key insights:
//      	Storing bands by digit and the Update routine
//			  (here named find_locked_candidates_and_update)
//
//  champagne:
//		Update for 128 bit registers and speed optimization
//		Key insights:
//			Pairing of ApplySingleOrEmptyCells and GuessBiValueInCell
//
//  JasonLion:
//		Conversion back to 32 bit data with further speed optimizations
//
//  emerentius:
//		Ported to Rust, simplified, commented and further optimized, ~10-30%
//		Key insights:
//			Checking up to three cells if no bivalue cell exists
//			Avoid unpredictable branch before UPWCL
//
// 	zhouyundong, champagne and JasonLion have all given permission
//  for a port under the AGPLv3 license in the forum thread
//		http://forum.enjoysudoku.com/3-77us-solver-2-8g-cpu-testcase-17sodoku-t30470-270.html#p262718

use sudoku::Sudoku;
use types::{Entry, Unsolvable};

// masks of 27 bits
const NONE: u32 = 0;
const ALL: u32 = 0o777_777_777;
const LOW9: u32 = 0o000_000_777;

// When the solver finds a solution it can save it or just count
// the latter is marginally faster
// the inner types should really be mutable references
// but reborrowing doesn't work with that
enum Solutions<'a> {
    Count(usize),
    Vector(&'a mut Vec<Sudoku>),
    Buffer(&'a mut [[u8; 81]], usize),
}

impl<'a> Solutions<'a> {
    fn len(&self) -> usize {
        match self {
            Solutions::Vector(v) => v.len(),
            Solutions::Count(len) => *len,
            Solutions::Buffer(_, len) => *len,
        }
    }
}
// Bands  Rows                   Columns
//
//               0    1    2    3    4    5    6    7    8
//            ┏━━━━┯━━━━┯━━━━┳━━━━┯━━━━┯━━━━┳━━━━┯━━━━┯━━━━┓
//     ┏   0  ┃ 00 │ 01 │ 02 ┃ 03 │ 04 │ 05 ┃ 06 │ 07 │ 08 ┃
//     ┃      ┠────┼────┼────╂────┼────┼────╂────┼────┼────┨
//   0 ┫   1  ┃ 09 │ 10 │ 11 ┃ 12 │ 13 │ 14 ┃ 15 │ 16 │ 17 ┃
//     ┃      ┠────┼────┼────╂────┼────┼────╂────┼────┼────┨
//     ┗   2  ┃ 18 │ 19 │ 20 ┃ 21 │ 22 │ 23 ┃ 24 │ 25 │ 26 ┃
//            ┣━━━━┿━━━━┿━━━━╋━━━━┿━━━━┿━━━━╋━━━━┿━━━━┿━━━━┫
//     ┏   3  ┃ 27 │ 28 │ 29 ┃ 30 │ 31 │ 32 ┃ 33 │ 34 │ 35 ┃
//     ┃      ┠────┼────┼────╂────┼────┼────╂────┼────┼────┨
//   1 ┫   4  ┃ 36 │ 37 │ 38 ┃ 39 │ 40 │ 41 ┃ 42 │ 43 │ 44 ┃
//     ┃      ┠────┼────┼────╂────┼────┼────╂────┼────┼────┨
//     ┗   5  ┃ 45 │ 46 │ 47 ┃ 48 │ 49 │ 50 ┃ 51 │ 52 │ 53 ┃
//            ┣━━━━┿━━━━┿━━━━╋━━━━┿━━━━┿━━━━╋━━━━┿━━━━┿━━━━┫
//     ┏   6  ┃ 54 │ 55 │ 56 ┃ 57 │ 58 │ 59 ┃ 60 │ 61 │ 62 ┃
//     ┃      ┠────┼────┼────╂────┼────┼────╂────┼────┼────┨
//   2 ┫   7  ┃ 63 │ 64 │ 65 ┃ 66 │ 67 │ 68 ┃ 69 │ 70 │ 71 ┃
//     ┃      ┠────┼────┼────╂────┼────┼────╂────┼────┼────┨
//     ┗   8  ┃ 72 │ 73 │ 74 ┃ 75 │ 76 │ 77 ┃ 78 │ 79 │ 80 ┃
//            ┗━━━━┷━━━━┷━━━━┻━━━━┷━━━━┷━━━━┻━━━━┷━━━━┷━━━━┛
//
// The solver is based on a band-oriented data structure
//
// All bitmasks are laid out as
// 1 bit per cell for each of the 27 cells in a band
// counting from least to most significant, the nth bit corresponds
// to the nth cell in the band (see diagram above for cell ordering)
// this forms 3 groups of 9 bits each, 1 group per row. This is useful
// for the strategy of Locked Candidates.
//
// A subband is the set of possible cells in a band for a single digit
// represented by one u32 with up to 27 bits set.
// they are enumerated as
// subband = digit * 3 + band
#[derive(Clone, Copy)]
pub(crate) struct SudokuSolver {
    // possible_cells_in_subband = subbands[digit*3 + band]
    poss_cells: UncheckedIndexArray27,
    prev_poss_cells: UncheckedIndexArray27,
    // empty_cells = unsolved_cells[band]
    unsolved_cells: UncheckedIndexArray3,
    requirement_for_weird_optimization: UncheckedIndexArray3,
    // bivalue_cells = pairs[band]
    pairs: UncheckedIndexArray3,
}

impl SudokuSolver {
    // jczsolve equivalent: InitSudoku
    pub fn from_sudoku(sudoku: Sudoku) -> Result<Self, Unsolvable> {
        let mut solver = SudokuSolver {
            poss_cells: UncheckedIndexArray27([ALL; 27]),
            prev_poss_cells: UncheckedIndexArray27([0; 27]),
            unsolved_cells: UncheckedIndexArray3([ALL; 3]),
            requirement_for_weird_optimization: UncheckedIndexArray3([ALL; 3]),
            pairs: UncheckedIndexArray3([0; 3]),
        };
        for (cell, num) in (0..81).zip(sudoku.iter()) {
            if let Some(num) = num {
                solver.insert_entry(Entry { cell, num })?;
            }
        }
        Ok(solver)
    }

    // jczsolve equivalent: SetSolvedDigit
    fn insert_entry(&mut self, entry: Entry) -> Result<(), Unsolvable> {
        let band = (entry.cell / 27) as usize;
        let subband = (entry.num as usize - 1) * 3 + band;
        let cell_mask = 1 << (entry.cell % 27);

        if self.poss_cells[subband] & cell_mask == NONE {
            return Err(Unsolvable);
        }

        // set cell and row of digit to solved
        self.unsolved_cells[band] &= !cell_mask;

        // remove digit possibility from cell neighbors by row, column and box
        self.poss_cells[subband] &= nonconflicting_cells_same_band(entry.cell as _);
        let nonconflicting_other = nonconflicting_cells_neighbor_bands(entry.cell);
        let (ns1, ns2) = neighbor_subbands(subband);
        self.poss_cells[ns1] &= nonconflicting_other;
        self.poss_cells[ns2] &= nonconflicting_other;

        // remove possibilities of other digits in same cell
        {
            // first, remove cell possibility for all digits
            let mut subband = band;
            while subband < 27 {
                self.poss_cells[subband] &= !cell_mask;
                subband += 3;
            }
        }
        // then, add correct digit back
        self.poss_cells[subband] |= cell_mask;
        Ok(())
    }

    // jczsolve equivalent: ExtractSolution
    fn extract_solution(&self) -> Sudoku {
        let mut sudoku = [0; 81];
        for (subband, &mask) in (0..27).zip(self.poss_cells.0.iter()) {
            let mut mask = mask;
            let digit = subband / 3;
            let base_cell_in_band = subband % 3 * 27;
            for cell_mask in mask_iter(mask) {
                let cell_in_band = bit_pos(cell_mask);
                *index_mut(&mut sudoku, cell_in_band + base_cell_in_band) = digit as u8 + 1;
            }
        }
        Sudoku(sudoku)
    }

    // This is called when the digit corresponding to the subband
    // is entered at the position given by the mask (must have 1 position only)
    // all conflicting cells (row and box neighbors) in the subband
    //
    // Other digit candidates digits and other bands are not touched (too expensive)
    //
    // jczsolve equivalent: SetSolvedMask
    fn insert_entry_by_mask(&mut self, subband: usize, mask: u32) {
        debug_assert!(mask.count_ones() == 1);
        debug_assert!(self.poss_cells[subband] & mask != 0);
        let band = subband % 3;
        let cell = band * 27 + bit_pos(mask);

        self.poss_cells[subband] &= nonconflicting_cells_same_band(cell);
    }

    // Search for cells that can contain only 1 digit and enter them
    // also search for cells that have a possibilities count of 0 (sudoku unsolvable)
    // 2 (good guess locations) or >=3 (bad guess locations)
    //
    // jczsolve equivalent: ApplySingleOrEmptyCells
    fn find_naked_singles(&mut self) -> Result<bool, Unsolvable> {
        let mut single_applied = false;

        for band in 0..3 {
            // mask of cells with >= 1, >= 2 or >= 3 candidates
            let mut cells1 = NONE;
            let mut cells2 = NONE;
            let mut cells3 = NONE;

            let mut subband = band;
            for _ in 0..9 {
                let band_mask = self.poss_cells[subband];
                cells3 |= cells2 & band_mask;
                cells2 |= cells1 & band_mask;
                cells1 |= band_mask;
                subband += 3;
            }

            if cells1 != ALL {
                return Err(Unsolvable);
            }

            // store doubles
            // equivalent to `cells2 & !cells3` because every bit in cells3 is also in cells2
            self.pairs[band] = cells2 ^ cells3;

            // new singles, ignore previously solved ones
            let mut singles = (cells1 ^ cells2) & self.unsolved_cells[band];

            'singles: for cell_mask_single in mask_iter(singles) {
                single_applied = true;
                for digit in 0..9 {
                    if self.poss_cells[digit * 3 + band] & cell_mask_single != NONE {
                        self.insert_entry_by_mask(digit * 3 + band, cell_mask_single);
                        continue 'singles;
                    }
                }
                // forced empty cell
                return Err(Unsolvable);
            }
        }

        Ok(single_applied)
    }

    // jczsolve equivalent: updn and upwcl macros
    //                      where upwcl is called conditionally only if needed
    //                      here, it's unconditional to avoid hard to predict branches
    #[inline(always)]
    fn _find_locked_candidates_and_update(&mut self, subband: usize) -> Result<(), Unsolvable> {
        let old_poss_cells = self.poss_cells[subband];

        // find all locked candidates in the band, both claiming and pointing type
        // using a LUT to condense each row of 9 bits down to 3 bits, 1 for each minirow
        // note: shrink_mask() only takes the lower 9 bits (1 row) into account
        // saving the results in a 9 bit mask for another LUT to find impossible entries
        let shrink = shrink_mask(old_poss_cells & LOW9)
            | shrink_mask(old_poss_cells >> 9 & LOW9) << 3
            | shrink_mask(old_poss_cells >> 18) << 6;
        let poss_cells = old_poss_cells & nonconflicting_cells_same_band_by_locked_candidates(shrink);
        if poss_cells == NONE {
            return Err(Unsolvable);
        }
        self.prev_poss_cells[subband] = poss_cells;
        self.poss_cells[subband] = poss_cells;

        // possible columns in subband, including already solved ones
        let poss_cols = (poss_cells | poss_cells >> 9 | poss_cells >> 18) & LOW9;

        // check for locked candidates of the columns (pointing type)
        // this is also what's enforcing that a column cannot contain
        // more than once
        let nonconflicting_other = nonconflicting_cells_neighbor_bands_by_locked_candidates(poss_cols);
        let (ns1, ns2) = neighbor_subbands(subband);
        self.poss_cells[ns1] &= nonconflicting_other;
        self.poss_cells[ns2] &= nonconflicting_other;

        // minirows that are locked have no neighboring minirows in the same
        // row or the same box
        // if are inside a box where only 1 column is possible, then only 1 cell is possible
        // and the row is solved
        // solved_rows is a 3-bit mask of the rows in the subband
        // mapping from solved minirows to solved rows happens
        // to need the same mask as shrinking
        // jczsolve equivalent: s , but lower 3 bits inversed
        //                      s_jczsolve = 7 ^ solved_rows
        //                      jczsolve used a 2nd, inverted lookup table
        let locked_candidates_intersection = locked_minirows(shrink) & column_single(poss_cols);
        let solved_rows = shrink_mask(locked_candidates_intersection);
        let solved_cells = row_mask(solved_rows) & poss_cells;

        // -------------- jczsolve equivalent: upwcl ---------------------------
        // delete other digit candidates from all cells in band
        // that are guaranteed to be the current digit
        let band = subband % 3;
        let nonconflicting_cells = !solved_cells;
        self.unsolved_cells[band] &= nonconflicting_cells;
        // remove from every entry but the current one
        let mut other_subband = band;
        while other_subband < 27 {
            if other_subband != subband {
                self.poss_cells[other_subband] &= nonconflicting_cells;
            }
            other_subband += 3;
        }
        // ----------------------- end upwcl -----------------------------------

        Ok(())
    }

    // jczsolve equivalent: Update
    fn find_locked_candidates_and_update(&mut self) -> Result<(), Unsolvable> {
        loop {
            // repeat until nothing can be found / updated anymore
            // this is the hottest piece of code in the solver
            let mut found_nothing = true;

            unroll!{
                for subband in 0..27 {
                    // the first condition is always true
                    // but the optimizer doesn't get that
                    // which causes it to be less aggressive in applying optimizations
                    // which would, in this rare case, cause the code to run slower
                    if (self.requirement_for_weird_optimization[0] >> subband / 3) & LOW9 != NONE
                    && self.poss_cells[subband] != self.prev_poss_cells[subband]
                    {
                        found_nothing = false;
                        self._find_locked_candidates_and_update(subband)?;
                    }
                }
            }

            if found_nothing {
                return Ok(());
            }
        }
    }

    // jczsolve equivalent: FullUpdate
    fn full_update(&mut self, limit: usize, solutions: &mut Solutions) -> Result<(), Unsolvable> {
        debug_assert!(solutions.len() <= limit);
        if solutions.len() == limit {
            return Err(Unsolvable); // not really, but it forces a recursion stop
        }
        loop {
            self.find_locked_candidates_and_update()?;
            if self.is_solved() {
                return Ok(());
            }
            // if singles found, go again
            if self.find_naked_singles()? {
                continue;
            }
            return Ok(());
        }
    }

    pub(crate) fn is_solved(&self) -> bool {
        self.unsolved_cells.0 == [NONE; 3]
    }

    // jczsolve equivalent: Guess
    fn guess(&mut self, limit: usize, solutions: &mut Solutions) {
        if self.is_solved() {
            debug_assert!(solutions.len() < limit);
            match solutions {
                Solutions::Count(count) => *count += 1,
                Solutions::Vector(vec) => vec.push(self.extract_solution()),
                Solutions::Buffer(buf, len) => {
                    if let Some(sudoku_slot) = buf.get_mut(*len) {
                        *sudoku_slot = self.extract_solution().to_bytes();
                    }
                    *len += 1;
                }
            }
        } else if self.guess_bivalue_in_cell(limit, solutions).is_ok() {
            // .is_ok() == found nothing
            self.guess_some_cell(limit, solutions);
        }
    }

    // Find some cell with only 2 possible values and try both in order
    // Whenever a guess has to be taken, there is virtually always a cell
    // with only 2 possibilities. These positions are found and saved when
    // looking for naked singles.
    // For that reason, finding such a cell is practically just a lookup.
    fn guess_bivalue_in_cell(&mut self, limit: usize, solutions: &mut Solutions) -> Result<(), Unsolvable> {
        for band in 0..3 {
            // get first bivalue cell, if it exists
            let cell_mask = match mask_iter(self.pairs[band]).next() {
                Some(mask) => mask,
                None => continue,
            };
            let mut subband = band;

            // loop through all 9 digits and check if that digit is possible in
            // the cell set in cell_mask. If so, try it.
            let mut first = true;
            loop {
                debug_assert!(subband < 27);

                if self.poss_cells[subband] & cell_mask != NONE {
                    if first {
                        first = false;
                        let mut solver = *self;
                        solver.insert_entry_by_mask(subband, cell_mask);
                        if solver.full_update(limit, solutions).is_ok() {
                            solver.guess(limit, solutions);
                        }
                        self.poss_cells[subband] ^= cell_mask;
                    } else {
                        self.insert_entry_by_mask(subband, cell_mask);
                        if self.full_update(limit, solutions).is_ok() {
                            self.guess(limit, solutions);
                        }
                        return Err(Unsolvable);
                    }
                }

                subband += 3;
            }
        }
        // no pairs found
        Ok(())
    }

    // find an unsolved cell and attempt to solve sudoku with all remaining candidates
    // in the vast majority of cases there is a cell with only 2 candidates
    // which means that guess_bivalue() will be called instead of this
    // It comes up only with harder sudokus, typically early during the solving
    // finding a cell with fewer candidates is very valuable in those cases
    // but an exhaustive search is too expensive
    // as a compromise, up to 3 cells are searched and the one with the fewest
    // candidates is used
    // jczsolve_equivalent: GuessFirstCell, sort of
    //                      jczsolve picks the first unsolved cell it can find
    //                      This fn checks up to 3 cells as explained above
    fn guess_some_cell(&mut self, limit: usize, solutions: &mut Solutions) {
        let best_guess = (0..3)
            .flat_map(|band| {
                // get first unsolved cell, if it exists
                let one_unsolved_cell = mask_iter(self.unsolved_cells[band]).next()?;
                let n_candidates = (0..9)
                    .map(|offset| band + 3 * offset)
                    .filter(|&subband| self.poss_cells[subband] & one_unsolved_cell != NONE)
                    .count();
                Some((n_candidates, band, one_unsolved_cell))
            })
            .min();
        let (_, band, unsolved_cell) = match best_guess {
            Some(min) => min,
            None => return,
        };

        let mut subband = band;
        // check every digit
        while subband < 27 {
            if self.poss_cells[subband] & unsolved_cell != NONE {
                let mut solver = self.clone();
                solver.insert_entry_by_mask(subband, unsolved_cell);
                if solver.full_update(limit, solutions).is_ok() {
                    solver.guess(limit, solutions);
                }
                if solutions.len() == limit {
                    return;
                }
                self.poss_cells[subband] ^= unsolved_cell;
            }

            subband += 3;
        }
    }

    fn _solve_at_most(mut self, limit: usize, solutions: &mut Solutions) {
        if self.find_naked_singles().is_err() {
            return;
        }

        // either solved or impossible
        if self.full_update(limit, solutions).is_err() {
            return;
        }
        self.guess(limit, solutions);
    }

    // find and return up to `limit` solutions
    pub fn solve_at_most(self, limit: usize) -> Vec<Sudoku> {
        let mut solutions = vec![];
        self._solve_at_most(limit, &mut Solutions::Vector(&mut solutions));
        solutions
    }

    // count up to `limit` solutions and save up to buffer.len() of them
    // in `buffer`. Return number of solutions.
    pub fn solve_at_most_buffer(self, buffer: &mut [[u8; 81]], limit: usize) -> usize {
        let mut solutions = Solutions::Buffer(buffer, 0);
        self._solve_at_most(limit, &mut solutions);
        solutions.len()
    }

    // find up to `limit` solutions and return count
    pub fn count_at_most(self, limit: usize) -> usize {
        let mut solutions = Solutions::Count(0);
        self._solve_at_most(limit, &mut solutions);
        solutions.len()
    }
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

// jczsolve equivalent: TblSelfMask
#[inline]
fn nonconflicting_cells_same_band(cell: usize) -> u32 {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    static SELF_MASK: [u32; 81] = [
        0x37E3F001,	0x37E3F002,	0x37E3F004,	0x371F8E08,	0x371F8E10,	0x371F8E20,	0x30FC7E40,	0x30FC7E80,	0x30FC7F00,
        0x2FE003F8,	0x2FE005F8,	0x2FE009F8,	0x2F1C11C7,	0x2F1C21C7,	0x2F1C41C7,	0x28FC803F, 0x28FD003F,	0x28FE003F,
        0x1807F1F8,	0x180BF1F8,	0x1813F1F8,	0x18238FC7,	0x18438FC7,	0x18838FC7, 0x19007E3F,	0x1A007E3F,	0x1C007E3F,
        0x37E3F001,	0x37E3F002,	0x37E3F004,	0x371F8E08,	0x371F8E10, 0x371F8E20,	0x30FC7E40,	0x30FC7E80,	0x30FC7F00,
        0x2FE003F8,	0x2FE005F8,	0x2FE009F8,	0x2F1C11C7,	0x2F1C21C7,	0x2F1C41C7,	0x28FC803F,	0x28FD003F,	0x28FE003F,
        0x1807F1F8,	0x180BF1F8,	0x1813F1F8, 0x18238FC7,	0x18438FC7,	0x18838FC7,	0x19007E3F,	0x1A007E3F,	0x1C007E3F,
        0x37E3F001,	0x37E3F002,	0x37E3F004,	0x371F8E08,	0x371F8E10,	0x371F8E20,	0x30FC7E40,	0x30FC7E80,	0x30FC7F00,
        0x2FE003F8, 0x2FE005F8,	0x2FE009F8,	0x2F1C11C7,	0x2F1C21C7,	0x2F1C41C7,	0x28FC803F,	0x28FD003F,	0x28FE003F,
        0x1807F1F8,	0x180BF1F8,	0x1813F1F8,	0x18238FC7,	0x18438FC7,	0x18838FC7,	0x19007E3F,	0x1A007E3F,	0x1C007E3F,
    ];
    *index(&SELF_MASK, cell)
}

// jczsolve equivalent: TblOtherfMask
#[inline]
fn nonconflicting_cells_neighbor_bands(cell: u8) -> u32 {
    // only 3 cells in a column conflict with an entry in another band
    // and they are all in one column, each 9 steps apart in the mask
    ALL ^ (0o_001_001_001 << cell % 9)
}

// compress the 3 bit groups into 3 bits
// 0b_abc_def_geh => 0b_xyz
// x = any(abc) = a | b | c
// etc.
// jczsolve equivalent: TblShrinkMask, TblRowUniq (inversed TblShrinkMask)
//                      TblRowUniq was replaced by shrink_mask here
#[inline]
fn shrink_mask(cell_mask: u32) -> u32 {
    *index(&SHRINK_MASK, (cell_mask) as usize) as u32
}

// returns mask of cells that are compatible with locked candidates
// in shrunk mask, both claiming and pointing type
// masks without at least 1 possible minirow in each row and column
// are mapped to 0 (unsolvable sudoku)
// jczsolve equivalent: TblComplexMask
#[inline]
fn nonconflicting_cells_same_band_by_locked_candidates(shrink: u32) -> u32 {
    *index(&LOCKED_CANDIDATES_MASK_SAME_BAND, shrink as usize)
}

// mask to remove impossible entries that conflict with a locked column/box
// jczsolve equivalent: TblMaskSingle
#[inline]
fn nonconflicting_cells_neighbor_bands_by_locked_candidates(row_shrink: u32) -> u32 {
    *index(&LOCKED_CANDIDATES_MASK_NEIGHBOR_BAND, row_shrink as usize)
}

// takes mask of possible columns in band:
//        876 543 210  column numbers
//     0b_ihg_fed_cba  bits
// and maps it to mask of minirows in box with unique column
//     0b_zyxzyxzyx
//
//     0..3  3..6  6..9            cols
//      x     y     z      0
//      x     y     z      1       rows
//      x     y     z      2
// where
// x = true iff one and only one of (a, b, c)
// same with y, z and def, ghi
//
// if not at least 1 possibility exists in each box, the sudoku is impossible and
// this table maps to 0
// jczsolve equivalent: TblColumnSingle
#[inline]
fn column_single(row_shrink: u32) -> u32 {
    *index(&COLUMN_SINGLE, row_shrink as usize) as u32
}

// maps from mask of possible minirows to mask of locked minirows (locked candidates)
// both claiming and pointing type
// includes locked candidates that would only appear after applying the other locked candidates
// jczsolve equivalent: TblShrinkSingle
#[inline]
fn locked_minirows(shrink: u32) -> u32 {
    *index(&LOCKED_MINIROWS, shrink as usize)
}

// expands the mask of slices with solved cells to a cell mask
// of possible locations
// jczsolve equivalent: reversed TblRowMask
#[inline]
fn row_mask(thing: u32) -> u32 {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    static ROW_MASK: [u32; 8] = [	// rows where single  found _000 to 111
        0o000000000, 0o000000777, 0o000777000, 0o000777777,
        0o777000000, 0o777000777, 0o777777000, 0o777777777,
    ];
    *index(&ROW_MASK, thing as usize)
    //(thing & 0b1) * 511 + (thing & 0b10) * 130816 + (thing & 0b100) * 33488896
}

// jczsolve equivalent: TblAnother1 and TblAnother2
#[inline]
fn neighbor_subbands(subband: usize) -> (usize, usize) {
    #[cfg_attr(rustfmt, rustfmt_skip)]
    static NEIGHBOR_SUBBANDS: [(usize, usize); 27] = [
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
    *index(&NEIGHBOR_SUBBANDS, subband)
}

// jczsolve equivalent: BitPos
#[inline(always)]
fn bit_pos(mask: u32) -> usize {
    mask.trailing_zeros() as usize
}

#[cfg_attr(rustfmt, rustfmt_skip)]
static SHRINK_MASK: [u32; 512] = [
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

#[cfg_attr(rustfmt, rustfmt_skip)]
static LOCKED_CANDIDATES_MASK_SAME_BAND: [u32; 512] = [
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o007070700, 0o707070700, 0o007770700, 0o707770700,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o077070700, 0o777070700, 0o777770700, 0o777770700,
    0o000000000, 0o000000000, 0o007700070, 0o077700070, 0o000000000, 0o000000000, 0o007770070, 0o077770070,
    0o000000000, 0o000000000, 0o707700070, 0o777700070, 0o000000000, 0o000000000, 0o777770070, 0o777770070,
    0o000000000, 0o000000000, 0o007700770, 0o777700770, 0o007070770, 0o777070770, 0o007770770, 0o777770770,
    0o000000000, 0o000000000, 0o707700770, 0o777700770, 0o077070770, 0o777070770, 0o777770770, 0o777770770,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o070007700, 0o070707700, 0o770007700, 0o770707700,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o077007700, 0o777707700, 0o777007700, 0o777707700,
    0o000000000, 0o070700007, 0o000000000, 0o077700007, 0o000000000, 0o070707007, 0o000000000, 0o077707007,
    0o000000000, 0o070700707, 0o000000000, 0o777700707, 0o070007707, 0o070707707, 0o777007707, 0o777707707,
    0o000000000, 0o770700007, 0o000000000, 0o777700007, 0o000000000, 0o777707007, 0o000000000, 0o777707007,
    0o000000000, 0o770700707, 0o000000000, 0o777700707, 0o077007707, 0o777707707, 0o777007707, 0o777707707,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o070077700, 0o070777700, 0o770777700, 0o770777700,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o007077700, 0o707777700, 0o007777700, 0o707777700,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o077077700, 0o777777700, 0o777777700, 0o777777700,
    0o000000000, 0o070700077, 0o007700077, 0o077700077, 0o000000000, 0o070777077, 0o007777077, 0o077777077,
    0o000000000, 0o070700777, 0o707700777, 0o777700777, 0o070077777, 0o070777777, 0o777777777, 0o777777777,
    0o000000000, 0o770700777, 0o007700777, 0o777700777, 0o007077777, 0o777777777, 0o007777777, 0o777777777,
    0o000000000, 0o770700777, 0o707700777, 0o777700777, 0o077077777, 0o777777777, 0o777777777, 0o777777777,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o700007070, 0o700077070, 0o000000000, 0o000000000, 0o770007070, 0o770077070,
    0o000000000, 0o700070007, 0o000000000, 0o700077007, 0o000000000, 0o707070007, 0o000000000, 0o707077007,
    0o000000000, 0o700070077, 0o700007077, 0o700077077, 0o000000000, 0o777070077, 0o777007077, 0o777077077,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o707007070, 0o777077070, 0o000000000, 0o000000000, 0o777007070, 0o777077070,
    0o000000000, 0o770070007, 0o000000000, 0o777077007, 0o000000000, 0o777070007, 0o000000000, 0o777077007,
    0o000000000, 0o770070077, 0o707007077, 0o777077077, 0o000000000, 0o777070077, 0o777007077, 0o777077077,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o700707070, 0o700777070, 0o000000000, 0o000000000, 0o770777070, 0o770777070,
    0o000000000, 0o700070707, 0o000000000, 0o700777707, 0o007070707, 0o707070707, 0o007777707, 0o707777707,
    0o000000000, 0o700070777, 0o700707777, 0o700777777, 0o077070777, 0o777070777, 0o777777777, 0o777777777,
    0o000000000, 0o000000000, 0o007707070, 0o077777070, 0o000000000, 0o000000000, 0o007777070, 0o077777070,
    0o000000000, 0o000000000, 0o707707070, 0o777777070, 0o000000000, 0o000000000, 0o777777070, 0o777777070,
    0o000000000, 0o770070777, 0o007707777, 0o777777777, 0o007070777, 0o777070777, 0o007777777, 0o777777777,
    0o000000000, 0o770070777, 0o707707777, 0o777777777, 0o077070777, 0o777070777, 0o777777777, 0o777777777,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o700007770, 0o700777770, 0o070007770, 0o070777770, 0o770007770, 0o770777770,
    0o000000000, 0o700770007, 0o000000000, 0o700777007, 0o000000000, 0o707777007, 0o000000000, 0o707777007,
    0o000000000, 0o700770777, 0o700007777, 0o700777777, 0o077007777, 0o777777777, 0o777007777, 0o777777777,
    0o000000000, 0o070770007, 0o000000000, 0o077777007, 0o000000000, 0o070777007, 0o000000000, 0o077777007,
    0o000000000, 0o070770777, 0o707007777, 0o777777777, 0o070007777, 0o070777777, 0o777007777, 0o777777777,
    0o000000000, 0o770770007, 0o000000000, 0o777777007, 0o000000000, 0o777777007, 0o000000000, 0o777777007,
    0o000000000, 0o770770777, 0o707007777, 0o777777777, 0o077007777, 0o777777777, 0o777007777, 0o777777777,
    0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000, 0o000000000,
    0o000000000, 0o000000000, 0o700707770, 0o700777770, 0o070077770, 0o070777770, 0o770777770, 0o770777770,
    0o000000000, 0o700770707, 0o000000000, 0o700777707, 0o007077707, 0o707777707, 0o007777707, 0o707777707,
    0o000000000, 0o700770777, 0o700707777, 0o700777777, 0o077077777, 0o777777777, 0o777777777, 0o777777777,
    0o000000000, 0o070770077, 0o007707077, 0o077777077, 0o000000000, 0o070777077, 0o007777077, 0o077777077,
    0o000000000, 0o070770777, 0o707707777, 0o777777777, 0o070077777, 0o070777777, 0o777777777, 0o777777777,
    0o000000000, 0o770770777, 0o007707777, 0o777777777, 0o007077777, 0o777777777, 0o007777777, 0o777777777,
    0o000000000, 0o770770777, 0o707707777, 0o777777777, 0o077077777, 0o777777777, 0o777777777, 0o777777777,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
static LOCKED_CANDIDATES_MASK_NEIGHBOR_BAND: [u32; 512] = [
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o767767767, 0o766766766, 0o765765765, 0o767767767, 0o763763763, 0o767767767, 0o767767767, 0o767767767,
    0o757757757, 0o756756756, 0o755755755, 0o757757757, 0o753753753, 0o757757757, 0o757757757, 0o757757757,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o737737737, 0o736736736, 0o735735735, 0o737737737, 0o733733733, 0o737737737, 0o737737737, 0o737737737,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o677677677, 0o676676676, 0o675675675, 0o677677677, 0o673673673, 0o677677677, 0o677677677, 0o677677677,
    0o667667667, 0o666666666, 0o665665665, 0o667667667, 0o663663663, 0o667667667, 0o667667667, 0o667667667,
    0o657657657, 0o656656656, 0o655655655, 0o657657657, 0o653653653, 0o657657657, 0o657657657, 0o657657657,
    0o677677677, 0o676676676, 0o675675675, 0o677677677, 0o673673673, 0o677677677, 0o677677677, 0o677677677,
    0o637637637, 0o636636636, 0o635635635, 0o637637637, 0o633633633, 0o637637637, 0o637637637, 0o637637637,
    0o677677677, 0o676676676, 0o675675675, 0o677677677, 0o673673673, 0o677677677, 0o677677677, 0o677677677,
    0o677677677, 0o676676676, 0o675675675, 0o677677677, 0o673673673, 0o677677677, 0o677677677, 0o677677677,
    0o677677677, 0o676676676, 0o675675675, 0o677677677, 0o673673673, 0o677677677, 0o677677677, 0o677677677,
    0o577577577, 0o576576576, 0o575575575, 0o577577577, 0o573573573, 0o577577577, 0o577577577, 0o577577577,
    0o567567567, 0o566566566, 0o565565565, 0o567567567, 0o563563563, 0o567567567, 0o567567567, 0o567567567,
    0o557557557, 0o556556556, 0o555555555, 0o557557557, 0o553553553, 0o557557557, 0o557557557, 0o557557557,
    0o577577577, 0o576576576, 0o575575575, 0o577577577, 0o573573573, 0o577577577, 0o577577577, 0o577577577,
    0o537537537, 0o536536536, 0o535535535, 0o537537537, 0o533533533, 0o537537537, 0o537537537, 0o537537537,
    0o577577577, 0o576576576, 0o575575575, 0o577577577, 0o573573573, 0o577577577, 0o577577577, 0o577577577,
    0o577577577, 0o576576576, 0o575575575, 0o577577577, 0o573573573, 0o577577577, 0o577577577, 0o577577577,
    0o577577577, 0o576576576, 0o575575575, 0o577577577, 0o573573573, 0o577577577, 0o577577577, 0o577577577,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o767767767, 0o766766766, 0o765765765, 0o767767767, 0o763763763, 0o767767767, 0o767767767, 0o767767767,
    0o757757757, 0o756756756, 0o755755755, 0o757757757, 0o753753753, 0o757757757, 0o757757757, 0o757757757,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o737737737, 0o736736736, 0o735735735, 0o737737737, 0o733733733, 0o737737737, 0o737737737, 0o737737737,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o377377377, 0o376376376, 0o375375375, 0o377377377, 0o373373373, 0o377377377, 0o377377377, 0o377377377,
    0o367367367, 0o366366366, 0o365365365, 0o367367367, 0o363363363, 0o367367367, 0o367367367, 0o367367367,
    0o357357357, 0o356356356, 0o355355355, 0o357357357, 0o353353353, 0o357357357, 0o357357357, 0o357357357,
    0o377377377, 0o376376376, 0o375375375, 0o377377377, 0o373373373, 0o377377377, 0o377377377, 0o377377377,
    0o337337337, 0o336336336, 0o335335335, 0o337337337, 0o333333333, 0o337337337, 0o337337337, 0o337337337,
    0o377377377, 0o376376376, 0o375375375, 0o377377377, 0o373373373, 0o377377377, 0o377377377, 0o377377377,
    0o377377377, 0o376376376, 0o375375375, 0o377377377, 0o373373373, 0o377377377, 0o377377377, 0o377377377,
    0o377377377, 0o376376376, 0o375375375, 0o377377377, 0o373373373, 0o377377377, 0o377377377, 0o377377377,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o767767767, 0o766766766, 0o765765765, 0o767767767, 0o763763763, 0o767767767, 0o767767767, 0o767767767,
    0o757757757, 0o756756756, 0o755755755, 0o757757757, 0o753753753, 0o757757757, 0o757757757, 0o757757757,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o737737737, 0o736736736, 0o735735735, 0o737737737, 0o733733733, 0o737737737, 0o737737737, 0o737737737,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o767767767, 0o766766766, 0o765765765, 0o767767767, 0o763763763, 0o767767767, 0o767767767, 0o767767767,
    0o757757757, 0o756756756, 0o755755755, 0o757757757, 0o753753753, 0o757757757, 0o757757757, 0o757757757,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o737737737, 0o736736736, 0o735735735, 0o737737737, 0o733733733, 0o737737737, 0o737737737, 0o737737737,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o767767767, 0o766766766, 0o765765765, 0o767767767, 0o763763763, 0o767767767, 0o767767767, 0o767767767,
    0o757757757, 0o756756756, 0o755755755, 0o757757757, 0o753753753, 0o757757757, 0o757757757, 0o757757757,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o737737737, 0o736736736, 0o735735735, 0o737737737, 0o733733733, 0o737737737, 0o737737737, 0o737737737,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
    0o777777777, 0o776776776, 0o775775775, 0o777777777, 0o773773773, 0o777777777, 0o777777777, 0o777777777,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
static LOCKED_MINIROWS: [u32; 512] = [
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o124, 0o124, 0o124, 0o124, 0o000, 0o000, 0o000, 0o000, 0o124, 0o124, 0o124, 0o124,
    0o000, 0o000, 0o142, 0o142, 0o000, 0o000, 0o142, 0o142, 0o000, 0o000, 0o142, 0o142, 0o000, 0o000, 0o142, 0o142,
    0o000, 0o000, 0o142, 0o142, 0o124, 0o124, 0o100, 0o100, 0o000, 0o000, 0o142, 0o142, 0o124, 0o124, 0o100, 0o100,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o214, 0o214, 0o214, 0o214,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o214, 0o214, 0o214, 0o214,
    0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o214, 0o200, 0o214, 0o200,
    0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o214, 0o200, 0o214, 0o200,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o214, 0o214, 0o214, 0o214,
    0o000, 0o000, 0o000, 0o000, 0o124, 0o124, 0o124, 0o124, 0o000, 0o000, 0o000, 0o000, 0o004, 0o004, 0o004, 0o004,
    0o000, 0o241, 0o142, 0o040, 0o000, 0o241, 0o142, 0o040, 0o000, 0o241, 0o142, 0o040, 0o214, 0o200, 0o000, 0o000,
    0o000, 0o241, 0o142, 0o040, 0o124, 0o000, 0o100, 0o000, 0o000, 0o241, 0o142, 0o040, 0o004, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o412, 0o412, 0o000, 0o000, 0o412, 0o412,
    0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o412, 0o400, 0o000, 0o421, 0o412, 0o400,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o412, 0o412, 0o000, 0o000, 0o412, 0o412,
    0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o412, 0o400, 0o000, 0o421, 0o412, 0o400,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o412, 0o412, 0o000, 0o000, 0o412, 0o412,
    0o000, 0o421, 0o000, 0o421, 0o124, 0o020, 0o124, 0o020, 0o000, 0o421, 0o412, 0o400, 0o124, 0o020, 0o000, 0o000,
    0o000, 0o000, 0o142, 0o142, 0o000, 0o000, 0o142, 0o142, 0o000, 0o000, 0o002, 0o002, 0o000, 0o000, 0o002, 0o002,
    0o000, 0o421, 0o142, 0o000, 0o124, 0o020, 0o100, 0o000, 0o000, 0o421, 0o002, 0o000, 0o124, 0o020, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o412, 0o412, 0o214, 0o214, 0o010, 0o010,
    0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o000, 0o421, 0o412, 0o400, 0o214, 0o000, 0o010, 0o000,
    0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o000, 0o241, 0o412, 0o000, 0o214, 0o200, 0o010, 0o000,
    0o000, 0o001, 0o000, 0o001, 0o000, 0o001, 0o000, 0o001, 0o000, 0o001, 0o412, 0o000, 0o214, 0o000, 0o010, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o412, 0o412, 0o214, 0o214, 0o010, 0o010,
    0o000, 0o421, 0o000, 0o421, 0o124, 0o020, 0o124, 0o020, 0o000, 0o421, 0o412, 0o400, 0o004, 0o000, 0o000, 0o000,
    0o000, 0o241, 0o142, 0o040, 0o000, 0o241, 0o142, 0o040, 0o000, 0o241, 0o002, 0o000, 0o214, 0o200, 0o000, 0o000,
    0o000, 0o001, 0o142, 0o000, 0o124, 0o000, 0o100, 0o000, 0o000, 0o001, 0o002, 0o000, 0o004, 0o000, 0o000, 0o000,
];

#[cfg_attr(rustfmt, rustfmt_skip)]
static COLUMN_SINGLE: [u32; 512] = [
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666,
    0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666,
    0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222,
    0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666,
    0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o777, 0o777, 0o666, 0o777, 0o666, 0o666, 0o666, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444, 0o000, 0o555, 0o555, 0o444, 0o555, 0o444, 0o444, 0o444,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222,
    0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222,
    0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222,
    0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o333, 0o333, 0o222, 0o333, 0o222, 0o222, 0o222, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
    0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000, 0o000, 0o111, 0o111, 0o000, 0o111, 0o000, 0o000, 0o000,
];

#[derive(Clone, Copy)]
struct UncheckedIndexArray3([u32; 3]);
#[derive(Clone, Copy)]
struct UncheckedIndexArray27([u32; 27]);

impl ::std::ops::Index<usize> for UncheckedIndexArray3 {
    type Output = u32;
    fn index(&self, idx: usize) -> &Self::Output {
        index(&self.0, idx)
    }
}

impl ::std::ops::IndexMut<usize> for UncheckedIndexArray3 {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        index_mut(&mut self.0, idx)
    }
}

impl ::std::ops::Index<usize> for UncheckedIndexArray27 {
    type Output = u32;
    fn index(&self, idx: usize) -> &Self::Output {
        index(&self.0, idx)
    }
}

impl ::std::ops::IndexMut<usize> for UncheckedIndexArray27 {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        index_mut(&mut self.0, idx)
    }
}

// for each set bit in mask, return a mask with only that bit set
fn mask_iter(mask: u32) -> impl Iterator<Item = u32> {
    ::std::iter::repeat(()).scan(mask, |mask, ()| {
        if *mask == 0 {
            return None;
        }
        let lowest_bit = *mask & !*mask + 1;
        *mask ^= lowest_bit;
        Some(lowest_bit)
    })
}
