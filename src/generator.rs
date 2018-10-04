use rand::Rng;

use consts::*;
use positions::*;
use sudoku::Sudoku;
use types::{Array81, Entry, Unsolvable};
use positions2::{Digit, Set};

// Sudoku generation is done via randomized solving of empty grids
// the solver is based on jsolve
// Helper struct for recursive solving
#[derive(Clone, Debug)]
pub(crate) struct SudokuGenerator {
    pub grid: Sudoku,
    pub n_solved_cells: u8,
    pub cell_poss_digits: Array81<Set<Digit>>,
    pub zone_solved_digits: [Set<Digit>; 27],
    pub last_cell: u8, // last cell checked in guess routine
}

impl SudokuGenerator {
    #[inline]
    pub fn new() -> SudokuGenerator {
        SudokuGenerator {
            grid: Sudoku([0; 81]),
            n_solved_cells: 0,
            cell_poss_digits: Array81([Set::ALL; 81]),
            zone_solved_digits: [Set::NONE; 27],
            last_cell: 0,
        }
    }

    #[inline]
    fn _insert_entry(&mut self, entry: Entry) {
        self.n_solved_cells += 1;
        self.grid.0[entry.cell().as_index()] = entry.num;
        self.cell_poss_digits[entry.cell().as_index()] = Set::NONE;
        self.zone_solved_digits[entry.row().as_index()] |= entry.digit_set();
        self.zone_solved_digits[entry.col().as_index()] |= entry.digit_set();
        self.zone_solved_digits[entry.field().as_index()] |= entry.digit_set();
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
            let entry_mask = entry.digit_set();
            // cell already solved from previous entry in stack, skip
            if self.cell_poss_digits[entry.cell().as_index()].is_empty() {
                continue;
            }

            // is entry still possible?
            if (self.cell_poss_digits[entry.cell().as_index()] & entry_mask).is_empty() {
                return Err(Unsolvable);
            }

            self._insert_entry(entry);
            for &cell in neighbours(entry.cell) {
                if entry_mask.overlaps(self.cell_poss_digits[cell as usize]) {
                    self.remove_impossibilities(cell, entry_mask, stack)?;
                };
            }

            // found a lot of naked singles, switch to batch insertion
            if stack.len() > 4 {
                return Ok(());
            }
        }
        Ok(())
    }

    pub fn batch_insert_entries(&mut self, stack: &mut Vec<Entry>) -> Result<(), Unsolvable> {
        for entry in stack.drain(..) {
            // cell already solved from previous entry in stack, skip
            if self.cell_poss_digits[entry.cell().as_index()].is_empty() {
                continue;
            }

            let entry_mask = entry.digit_set();

            // is entry still possible?
            // have to check zone possibilities, because cell possibility
            // is temporarily out of date
            #[cfg_attr(rustfmt, rustfmt_skip)]
            {
                if self.zone_solved_digits[entry.row().as_index()].overlaps(entry_mask)
                || self.zone_solved_digits[entry.col().as_index()].overlaps(entry_mask)
                || self.zone_solved_digits[entry.field().as_index()].overlaps(entry_mask)
                {
                    return Err(Unsolvable);
                }
            }

            self._insert_entry(entry);
        }

        // update cell possibilities from zone masks
        for cell in 0..81 {
            if self.cell_poss_digits[cell as usize].is_empty() {
                continue;
            }
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
            let mut unsolved = Set::NONE;
            let mut multiple_unsolved = Set::NONE;

            let cells = cells_of_zone(zone);
            for &cell in cells {
                let poss_digits = self.cell_poss_digits[cell as usize];
                multiple_unsolved |= unsolved & poss_digits;
                unsolved |= poss_digits;
            }
            if unsolved | self.zone_solved_digits[zone as usize] != Set::ALL {
                return Err(Unsolvable);
            }

            let mut singles = unsolved.without(multiple_unsolved);
            if singles.is_empty() {
                continue;
            }

            for &cell in cells {
                let mask = self.cell_poss_digits[cell as usize];

                if let Ok(maybe_unique) = (mask & singles).unique() {
                    let num = maybe_unique.ok_or(Unsolvable)?;
                    stack.push(Entry { cell, num: num.val() });

                    // mark num as found
                    singles.remove(Set::from(num));

                    // everything in this zone found
                    // return to insert numbers immediately
                    if singles.is_empty() {
                        return Ok(());
                    }
                }
            }
            // can not occur but the optimizer appreciates the info
            break;
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
                let n_possibilities = cell_mask.len();
                // 0 means cell was already processed or its impossible in which case,
                // it should have been caught elsewhere
                // 1 shouldn't happen for the same reason, should have been processed
                if n_possibilities > 0 && n_possibilities < min_possibilities {
                    best_cell = cell;
                    min_possibilities = n_possibilities;
                    if n_possibilities == 2 {
                        break;
                    }
                }
                if cell == self.last_cell {
                    break;
                }
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
        let choice = ::rand::thread_rng().gen_range(0, poss_digits.len());
        let num = poss_digits.into_iter().nth(choice as usize).unwrap();
        Entry { num: num.val(), cell: best_cell }
    }

    // remove impossible digits from masks for given cell
    // also check for naked singles and impossibility of sudoku
    fn remove_impossibilities(
        &mut self,
        cell: u8,
        impossible: Set<Digit>,
        stack: &mut Vec<Entry>,
    ) -> Result<(), Unsolvable> {
        let cell_mask = &mut self.cell_poss_digits[cell as usize];
        cell_mask.remove(impossible);
        if let Some(num) = cell_mask.unique()? {
            stack.push(Entry { cell, num: num.val() });
        }
        Ok(())
    }

    // for generation of random, filled sudokus
    fn randomized_solve_one(mut self, stack: &mut Vec<Entry>) -> Result<Sudoku, Unsolvable> {
        // insert and deduce in a loop
        // do a random guess when no more deductions are found
        // backtrack on error (via recursion)
        loop {
            self.insert_entries(stack)?;
            if self.is_solved() {
                return Ok(self.grid);
            }

            self.find_hidden_singles(stack)?;
            if !stack.is_empty() {
                continue;
            }

            let entry = self.find_good_random_guess();
            stack.push(entry);
            if let filled_sudoku @ Ok(_) = self.clone().randomized_solve_one(stack) {
                return filled_sudoku;
            }
            stack.clear();

            self.remove_impossibilities(entry.cell, entry.digit_set(), stack)?;
        }
    }

    pub fn generate_filled() -> Sudoku {
        // fill first row with a permutation of 1...9
        // not necessary, but ~15% faster
        let mut stack = Vec::with_capacity(81);
        let mut perm = [1, 2, 3, 4, 5, 6, 7, 8, 9];
        ::rand::thread_rng().shuffle(&mut perm);

        stack.extend((0..9).zip(perm.iter()).map(|(cell, &num)| Entry { cell, num }));

        Self::new().randomized_solve_one(&mut stack).unwrap()
    }
}
