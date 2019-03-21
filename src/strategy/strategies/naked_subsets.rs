use super::prelude::*;

pub(crate) fn find_naked_subsets (
    cells_poss_digits: &CellArray<Set<Digit>>,
    house_solved_digits: &HouseArray<Set<Digit>>,
    subset_size: u8,
    stop_after_first: bool,
    mut on_subset: impl FnMut(
        House,
        Set<Position<House>>,
        Set<Digit>,
    ) -> bool,
) -> Result<(), Unsolvable> {

    fn walk_combinations (
        cells_poss_digits: &CellArray<Set<Digit>>,
        total_poss_digs: Set<Digit>,
        positions: SetIter<Position<House>>,
        house: House,
        position_set: Set<Position<House>>,
        on_subset: &mut impl FnMut(
            House,
            Set<Position<House>>,
            Set<Digit>,
        ) -> bool,
        subset_size: u8,
        stop_after_first: bool,
    ) -> bool {
        // subsets of 5 and more numbers always have complementary subsets
        // of 9 - subset_size
        if position_set.len() > subset_size { return false }
        if position_set.len() == subset_size
            && total_poss_digs.len() == position_set.len()
            && on_subset(house, position_set, total_poss_digs)
            && stop_after_first {
            // found a subset
            return true;
        }

        let mut positions = positions;
        while let Some(position) = positions.next() {
            let cell = house.cell_at(position);
            let cell_poss_digits = cells_poss_digits[cell];
            // solved or impossible cell
            if cell_poss_digits.is_empty() { continue }
            let new_pos_set = position_set | position.as_set();
            let new_total_poss_digs = total_poss_digs | cell_poss_digits;

            // if true, then a subset was found and stop_after_first is set
            // stop recursion
            if walk_combinations(cells_poss_digits, new_total_poss_digs, positions.clone(), house, new_pos_set, on_subset, subset_size, stop_after_first) {
                return true
            };
        }
        false
    }

    for house in House::all() {
        if house_solved_digits[house].is_full() { continue }
        // if true, then a subset was found and stop_after_first is set
        // stop looking
        if walk_combinations(cells_poss_digits, Set::NONE, Set::ALL.into_iter(), house, Set::NONE, &mut on_subset, subset_size, stop_after_first) {
            break
        };
    }
    Ok(())
}
