#![allow(missing_docs, unused)]
use sudoku::Sudoku;
use types::{Array81, Mask, Digit, Position, Unsolvable, Entry};
use consts::*;
use positions::{
    row_zone, col_zone, field_zone, cells_of_zone,
    Cell, Line, Zone, Slice, Band,
};
use super::{StrategySolver, Deduce, NewStrategy};

#[derive(Debug, Clone, Copy)]
pub struct HiddenSingles;

pub struct HiddenSinglesDed {
    zone: u8,
    singles: Mask<Digit>,
    state: StrategySolver,
}
/*
impl Deduce for HiddenSinglesDed {
    fn apply_deductions(&mut self, deduced_entries: &mut Vec<Entry>, impossible_entries: &mut Vec<Entry>) -> Result<(), Unsolvable> {
        for &cell in cells_of_zone(self.zone) {
            let mask = self.state.possible_digits_in_cell(Cell(cell));
            if mask & self.singles != Mask::NONE {

                let num = (mask & self.singles).unique_num().expect("unexpected empty mask").ok_or(Unsolvable)?;
                deduced_entries.push(Entry{ cell: cell, num: num } );

                // remove single from mask
                self.singles &= !Mask::from_num(num);
                // everything in this zone found
                if self.singles == Mask::NONE { break }
            }
        }
        Ok(())
    }
}

pub struct HiddenSinglesIter {
    zone: u8,
    state: StrategySolver,
}

impl Iterator for HiddenSinglesIter {
    type Item = Result<HiddenSinglesDed, Unsolvable>;
    fn next(&mut self) -> Option<Self::Item> {
        for zone in self.zone..27 {
            self.zone = zone;
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
				.map(|cell| self.state.possible_digits_in_cell(cell));

			let (unsolved, _, mut singles) = find_unique(possible_digits_in_cells);

			if unsolved | self.state.zone_solved_digits[zone as usize] != Mask::ALL {
				return Some(Err(Unsolvable));
			}

			if singles == Mask::NONE { continue }

            return Some(Ok(
                HiddenSinglesDed {
                    zone,
                    singles,
                    state: self.state.clone(),
                }
            ))
		}
        None
    }
}

impl NewStrategy for HiddenSingles {
    type Deduction = HiddenSinglesDed;
    type Deductions = HiddenSinglesIter;

	fn deduce(&self, sudoku: &StrategySolver) -> Self::Deductions {
        HiddenSinglesIter {
            zone: 0,
            state: sudoku.clone()
        }
	}
}
*/
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
