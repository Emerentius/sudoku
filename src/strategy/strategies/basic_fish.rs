use crate::helper::{Unsolvable, HouseArray, DigitArray};
use crate::bitset::{Set, Iter as SetIter};
use crate::board::{
    Digit,
    positions::{
        Position,
        House,
        Line
    }
};

pub(crate) fn find_fish(
    house_poss_positions: &HouseArray<DigitArray<Set<Position<House>>>>,
    max_size: u8,
    stop_after_first: bool,
    mut on_fish: impl FnMut(
		Set<Line>,
        Digit,
        Set<Line>,
        Set<Position<Line>>,
    ) -> bool,
) -> Result<(), Unsolvable> {
    for digit in (1..10).map(Digit::new) {
        for &lines in &[Line::ALL_ROWS, Line::ALL_COLS] {
            if basic_fish_walk_combinations(house_poss_positions, digit, max_size, Set::NONE, lines.into_iter(), lines, Set::NONE, &mut on_fish, stop_after_first) {
                return Ok(());
            };
        }
    }
    Ok(())
}

//             goal_depth
// <degenerated>   1 (basically a naked/hidden single, not supported by this fn)
// x-wing          2
// swordfish       3
// jellyfish       4
fn basic_fish_walk_combinations(
    house_poss_positions: &HouseArray<DigitArray<Set<Position<House>>>>,
	digit: Digit,
	goal_depth: u8,
	line_set: Set<Line>,
	lines: SetIter<Line>,
	all_lines: Set<Line>,
	union_poss_pos: Set<Position<Line>>,
    on_fish: &mut impl FnMut(
		Set<Line>,
        Digit,
        Set<Line>,
        Set<Position<Line>>,
    ) -> bool,
	stop_after_first: bool,
) -> bool {
	if line_set.len() == goal_depth {
		// nothing of interest found
		if union_poss_pos.len() != goal_depth { return false }

		// found xwing, swordfish, or jellyfish
        if on_fish(all_lines, digit, line_set, union_poss_pos) {
            return true;
        }
	}

	let mut lines = lines;
	while let Some(line) = lines.next() {
		let possible_pos = house_poss_positions[line][digit];
		let n_poss = possible_pos.len();
		let new_union_poss_pos = union_poss_pos | possible_pos.as_line_set();

		// n_poss == 0 => solved row (or impossible)
		// n_poss == 1 => hidden single
		if n_poss < 2 || new_union_poss_pos.len() > goal_depth as u8 { continue }

        let new_line_set = line_set | line.as_set();
		if basic_fish_walk_combinations(house_poss_positions, digit, goal_depth, new_line_set, lines.clone(), all_lines, new_union_poss_pos, on_fish, stop_after_first) {
			return true
		};
	}
	false
}
