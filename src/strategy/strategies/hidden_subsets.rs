use super::prelude::*;

pub(crate) fn find_hidden_subsets (
    house_solved_digits: &HouseArray<Set<Digit>>,
    house_poss_positions: &HouseArray<DigitArray<Set<Position<House>>>>,
    subset_size: u8,
    stop_after_first: bool,
    mut on_subset: impl FnMut(
        House,
        Set<Digit>,
        Set<Position<House>>,
    ) -> bool,
) -> Result<(), Unsolvable> {

    fn walk_combinations (
        house_poss_positions: &DigitArray<Set<Position<House>>>,
        total_poss_pos: Set<Position<House>>,
        digits: SetIter<Digit>,
        house: House,
        digit_set: Set<Digit>,
        on_subset: &mut impl FnMut(
            House,
            Set<Digit>,
            Set<Position<House>>,
        ) -> bool,
        subset_size: u8,
        stop_after_first: bool,
    ) -> bool {
        // subsets of 5 and more numbers always have complementary subsets
        // of 9 - subset_size
        if digit_set.len() > subset_size { return false }
        if digit_set.len() == subset_size
            && total_poss_pos.len() == subset_size
            && on_subset(house, digit_set, total_poss_pos)
            && stop_after_first
        {
            return true;
        }

        let mut digits = digits;
        while let Some(digit) = digits.next() {
            let num_poss_pos = house_poss_positions[digit];
            // solved cell
            if num_poss_pos.is_empty() { continue }
            let new_digit_set = digit_set | digit.as_set();
            let new_total_poss_pos = total_poss_pos | num_poss_pos;
            if walk_combinations(house_poss_positions, new_total_poss_pos, digits.clone(), house, new_digit_set, on_subset, subset_size, stop_after_first) {
                return true
            };
        }
        false
    }

    for house in House::all() {
        if house_solved_digits[house].is_full() { continue }
        if walk_combinations(&house_poss_positions[house], Set::NONE, Set::ALL.into_iter(), house, Set::NONE, &mut on_subset, subset_size, stop_after_first) {
            break
        };
    }
    Ok(())
}
