// WIP
#![allow(unused)]
use super::prelude::*;

pub(crate) fn find_almost_locked_sets(
    cells_poss_digits: &CellArray<Set<Digit>>,
    //house_solved_digits: &HouseArray<Set<Digit>>,
    //subset_size: u8,
    stop_after_first: bool,
    mut on_subset: impl FnMut(
        //House,
        //Set<Position<House>>,
        //Set<Digit>,
        (), // rustfmt bug: deletes comments unless something is here
    ) -> bool,
) -> Result<(), Unsolvable> {
    let als = _find_almost_locked_sets(cells_poss_digits);
    for first_set_size in 2..=8 {
        for second_set_size in 1..=first_set_size {
            let sets1 = &als[first_set_size];
            let sets2 = &als[second_set_size];
            // TODO: special case equal sized sets
            //       so there's no repetition

            // iterate over all house combinations
            // except combinations of the same house kind
            // because they have no overlap
            for (house1, set1) in sets1.iter().enumerate() {
                for &(cells1, digits1) in set1 {
                    for (house2, set2) in sets2.iter().enumerate().filter(|&(h2, _)| h2 / 9 != house1 / 9) {
                        for &(cells2, digits2) in set2 {
                            let common_digits = digits1 & digits2;
                            if common_digits.is_empty() {
                                continue;
                            }

                            //let mut restricted_common_digits = Set::NONE;

                            for digit in common_digits {
                                let cells_of_digit = |digit: Digit, cells| {
                                    let mut cells_of_digit = Set::NONE;
                                    let digit_set = digit.as_set();
                                    for cell in cells {
                                        if cells_poss_digits[cell].overlaps(digit_set) {
                                            cells_of_digit |= cell;
                                        }
                                    }
                                    cells_of_digit
                                };
                                let cells_of_digit1 = cells_of_digit(digit, cells1);
                                let cells_of_digit2 = cells_of_digit(digit, cells2);

                                // ALS with overlapping cells are possible
                                // but not yet supported
                                // also, the restricted common digit must not be in an
                                // overlapping cell
                                if cells_of_digit1.overlaps(cells_of_digit2) {
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

type AlmostLockedSets = [[Vec<(Set<Cell>, Set<Digit>)>; 27]; 8];

// 27 houses
// 1-8 cells
// 2-9 candidates
// [[Vec<(Set<Cell>, Set<Digit>)>; 9]; 27]
pub(crate) fn _find_almost_locked_sets(cells_poss_digits: &CellArray<Set<Digit>>) -> AlmostLockedSets {
    let mut sets = AlmostLockedSets::default(); //[[vec![]; 9]; 27];
    for house in House::all() {
        let cells = house.cells();
        _walk_combinations(
            cells_poss_digits,
            cells.into_iter(),
            Set::NONE,
            Set::NONE,
            &mut sets,
            house.as_index(),
            0,
        );
    }
    sets
}

pub(crate) fn _walk_combinations(
    cells_poss_digits: &CellArray<Set<Digit>>,
    mut cells: SetIter<Cell>,
    cell_set: Set<Cell>,
    digits: Set<Digit>,
    almost_locked_sets: &mut AlmostLockedSets,
    house: usize,
    depth: u8,
) {
    while let Some(cell) = cells.next() {
        let new_cell_set = cell_set | cell.as_set();
        let candidates = cells_poss_digits[cell];
        if candidates.len() <= 1 {
            continue;
        }
        let new_digits = digits | candidates;

        assert!(new_cell_set.len() == depth + 1);

        if new_digits.len() == new_cell_set.len() + 1 {
            almost_locked_sets[new_cell_set.len() as usize - 1][house].push((new_cell_set, new_digits));
        }

        _walk_combinations(
            cells_poss_digits,
            cells.clone(),
            new_cell_set,
            new_digits,
            almost_locked_sets,
            house,
            depth + 1,
        );
    }
}
