/*
#![allow(missing_docs, unused)]
use sudoku::Sudoku;
use types::{Array81, Mask, Digit, Position, Unsolvable, Entry};
use consts::*;
use positions::{
    row_zone, col_zone, field_zone, cells_of_zone,
    Cell, Line, Zone, Slice, Band,
};
use super::{StrategySolver, Deduce, NewStrategy, SudokuState};

struct NakedSingles {
}

impl NakedSingles {
	fn iter(state: &SudokuState) -> NakedSinglesIter {
        NakedSinglesIter {
            cell: 0,
            sudoku: state.grid,
        }
    }
}

pub struct NakedSinglesIter {
    cell: u8,
    sudoku: Sudoku
}

impl Iterator for NakedSinglesIter {
    type Item = Result<Entry, Unsolvable>;
    fn next(&mut self) -> Option<Self::Item> {
        let sudoku = &self.sudoku;
        for (cell, poss_digits) in (self.cell..).zip(&self.state.cell_poss_digits.0[self.cell as usize..])
			// skip filled cells
			.filter(|&(cell, _)| state.grid.0[cell as usize] == 0)
		{
            self.cell += 1;
            match poss_digits.unique_num() {
                Ok(Some(num)) => {
                    return Some( Ok( NakedSinglesDed {
                        entry: Entry { num, cell: cell as u8 }
                    }))
                },
                Err(Unsolvable) => return Some(Err(Unsolvable)),
                _ => (),
			}
		}
		None
    }
}

impl NewStrategy for NakedSingles {
    type Deduction = NakedSinglesDed;
    type Deductions = NakedSinglesIter;

	fn deduce(&self, sudoku: &StrategySolver) -> Self::Deductions {
        NakedSinglesIter {
            cell: 0,
            state: sudoku.clone()
        }
	}
}
*/
