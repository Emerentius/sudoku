use crate::helper::{Unsolvable, CellArray, HouseArray};
use crate::bitset::Set;
use crate::board::{
    Digit,
    Candidate,
    positions::{
        House,
    }
};

pub(crate) fn find_hidden_singles(
    last_house: &mut u8,
    cell_poss_digits: &CellArray<Set<Digit>>,
    house_solved_digits: &HouseArray<Set<Digit>>,
    stop_after_first: bool,
    mut on_new_entry: impl FnMut(Candidate, House) -> Result<(), Unsolvable>,
) -> Result<(), Unsolvable> {
    for house in House::all().chain(House::all()).skip(*last_house as usize).take(27) {
        *last_house = if *last_house < 27 { *last_house + 1 } else { 0 };
        let mut unsolved: Set<Digit> = Set::NONE;
        let mut multiple_unsolved = Set::NONE;

        let cells = house.cells();
        for cell in cells {
            let poss_digits = cell_poss_digits[cell];
            multiple_unsolved |= unsolved & poss_digits;
            unsolved |= poss_digits;
        }
        if unsolved | house_solved_digits[house] != Set::ALL {
            return Err(Unsolvable);
        }

        let mut singles = unsolved.without(multiple_unsolved);
        if singles.is_empty() { continue }

        for cell in cells {
            let mask = cell_poss_digits[cell];

            if let Ok(maybe_unique) = (mask & singles).unique() {
                let digit = maybe_unique.ok_or(Unsolvable)?;
                let candidate = Candidate { cell, digit };

                on_new_entry(candidate, house)?;

                // mark num as found
                singles.remove(digit.as_set());

                // everything in this house found
                // return to insert numbers immediately
                match stop_after_first {
                    true => return Ok(()),
                    false if singles.is_empty() => break, // continue next house
                    _ => (), // find rest of singles in house
                }
            }
        }
    }
    Ok(())
}
