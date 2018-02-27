use super::StrategySolver;
use std::ops::{Generator, GeneratorState};
use types::{Entry, Unsolvable, Mask, MaskIter, Position, Digit};
use positions::*;

enum Strategy {
    NakedSingle,
    HiddenSingle,
    LockedCandidates,
    NakedSubset,
    HiddenSubset,
    XWing,
    Swordfish,
    Jellyfish,
    SinglesChain,
    #[doc(hidden)] __NonExhaustive
}

impl Strategy {
    fn deduce_one(&self, state: &StrategySolver, deduced_entries: &mut Vec<Entry>, impossible_entries: &mut Vec<Entry>) {
        use self::Strategy::*;
        match *self {
            NakedSingle => (),
            HiddenSingle => (),
            _ => unimplemented!(),
        }
    }

    fn deduce_all(&self, state: &StrategySolver, deduced_entries: &mut Vec<Entry>, impossible_entries: &mut Vec<Entry>) {
        use self::Strategy::*;
        match *self {
            NakedSingle => (),
            HiddenSingle => (),
            _ => unimplemented!(),
        }
    }
}

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
