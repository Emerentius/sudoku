// WIP
#![allow(dead_code)]
use super::prelude::*;

pub(crate) fn find_mutant_fish(
    house_poss_positions: &HouseArray<DigitArray<Set<Position<House>>>>,
    target_size: u8,
    stop_after_first: bool,
    mut on_fish: impl FnMut(
        Digit,
        Set<Cell>,  // candidate cells in both base and cover
        Set<House>, // base houses
        Set<House>, // cover houses
    ) -> bool,
) -> Result<(), Unsolvable> {
    for digit in (1..10).map(Digit::new) {
        if find_base(
            house_poss_positions,
            digit,
            target_size,
            Set::NONE,
            Set::ALL.into_iter(),
            Set::NONE,
            &mut on_fish,
            stop_after_first,
        ) {
            return Ok(());
        };
    }
    Ok(())
}

fn find_base(
    house_poss_positions: &HouseArray<DigitArray<Set<Position<House>>>>,
    digit: Digit,
    target_size: u8,
    base_houses: Set<House>,
    houses: SetIter<House>,
    candidate_cells: Set<Cell>,
    on_fish: &mut impl FnMut(
        Digit,
        Set<Cell>,  // candidate cells in both base and cover
        Set<House>, // base houses
        Set<House>, // cover houses
    ) -> bool,
    stop_after_first: bool,
) -> bool {
    if base_houses.len() == target_size {
        // nothing of interest found
        if candidate_cells.len() != target_size {
            return false;
        }

        // found base, now try to find cover
        if find_cover(
            digit,
            base_houses,
            is_mutant(base_houses),
            candidate_cells,
            Set::NONE,
            (Set::ALL ^ base_houses).into_iter(),
            Set::NONE,
            on_fish,
            stop_after_first,
        ) {
            return true;
        }
    }

    let mut houses = houses;
    while let Some(house) = houses.next() {
        let possible_pos = house_poss_positions[house][digit];
        let n_poss = possible_pos.len();
        let house_candidate_cells = house.cells_at(possible_pos);

        // not looking for finned fish, neither endo nor exo
        if house_candidate_cells.len() > target_size || house_candidate_cells.overlaps(candidate_cells) {
            continue;
        }

        let new_candidate_cells = candidate_cells | house_candidate_cells;

        // n_poss == 0 => solved row (or impossible)
        // n_poss == 1 => hidden single
        if n_poss < 2 || new_candidate_cells.len() > target_size {
            continue;
        }

        let new_base_houses = base_houses | house;
        if find_base(
            house_poss_positions,
            digit,
            target_size,
            new_base_houses,
            houses.clone(),
            new_candidate_cells,
            on_fish,
            stop_after_first,
        ) {
            return true;
        };
    }
    false
}

fn find_cover(
    digit: Digit,
    base_houses: Set<House>,
    base_is_mutant: bool,
    candidate_cells: Set<Cell>,
    covered_candidate_cells: Set<Cell>,
    eligible_houses: SetIter<House>,
    chosen_houses: Set<House>,
    on_fish: &mut impl FnMut(
        Digit,
        Set<Cell>,  // candidate cells in both base and cover
        Set<House>, // base houses
        Set<House>, // cover houses
    ) -> bool,
    stop_after_first: bool,
) -> bool {
    if chosen_houses.len() == base_houses.len()
        && candidate_cells == covered_candidate_cells
        && (base_is_mutant || is_mutant(chosen_houses))
    {
        let found_conflicts = on_fish(digit, candidate_cells, base_houses, chosen_houses);

        return found_conflicts && stop_after_first;
    }

    let mut eligible_houses = eligible_houses;
    while let Some(house) = eligible_houses.next() {
        let house_covered_candidates = candidate_cells & house.cells();

        // no cell must be able to fulfill two houses in either base or cover at the same time
        if house_covered_candidates.overlaps(covered_candidate_cells) {
            continue;
        }
        let new_covered_candidate_cells = covered_candidate_cells | house_covered_candidates;

        if find_cover(
            digit,
            base_houses,
            base_is_mutant,
            candidate_cells,
            new_covered_candidate_cells,
            eligible_houses.clone(),
            chosen_houses | house,
            on_fish,
            stop_after_first,
        ) {
            return true;
        };
    }
    false
}

#[rustfmt::skip]
pub(crate) fn is_mutant(houses: Set<House>) -> bool {
    House::ALL_ROWS.contains(houses)
        || House::ALL_COLS.contains(houses)
        || House::ALL_BLOCKS.contains(houses)
}
