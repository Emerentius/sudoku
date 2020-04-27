use super::prelude::*;

// stop after first will only eliminate line OR field neighbors for ONE number
// even if multiple are found at the same time
pub(crate) fn find_locked_candidates(
    cell_poss_digits: &CellArray<Set<Digit>>,
    stop_after_first: bool,
    mut on_locked_candidates: impl FnMut(
        MiniLine,        // miniline
        Digit,           // digit that is locked to `miniline`
        [Set<Digit>; 9], // candidates for all minilines in chute
        &[MiniLine; 2],  // neighbor minilines
        bool,            // is_pointing
    ) -> bool,
) -> Result<(), Unsolvable> {
    for chute in Chute::all() {
        let mut miniline_poss_digits: [Set<Digit>; 9] = [Set::NONE; 9];

        {
            // compute possible digits for each miniline
            let minilines = chute.minilines();
            for (&miniline, poss_digs) in minilines.iter().zip(miniline_poss_digits.iter_mut()) {
                for cell in miniline.cells() {
                    *poss_digs |= cell_poss_digits[cell];
                }
            }
        }

        let mut line_unique_digits: [Set<Digit>; 3] = [Set::NONE; 3];
        let mut block_unique_digits: [Set<Digit>; 3] = [Set::NONE; 3];

        {
            let poss_digits = |chute_line, chute_field| miniline_poss_digits[chute_line * 3 + chute_field];
            for (chute_line, line_uniques) in line_unique_digits.iter_mut().enumerate() {
                let poss_digits_iter = (0..3).map(|chute_field| poss_digits(chute_line, chute_field));

                let (_, _, unique) = find_unique(poss_digits_iter);
                *line_uniques = unique;
            }
            for (chute_field, block_uniques) in block_unique_digits.iter_mut().enumerate() {
                let poss_digits_iter = (0..3).map(|chute_line| poss_digits(chute_line, chute_field));

                let (_, _, unique) = find_unique(poss_digits_iter);
                *block_uniques = unique;
            }
        }

        for (i, (&miniline, &poss_digits)) in chute
            .minilines()
            .iter()
            .zip(miniline_poss_digits.iter())
            .enumerate()
        {
            let chute_line = i / 3;
            let chute_field = i % 3;

            let line_uniques = poss_digits & line_unique_digits[chute_line];
            let block_uniques = poss_digits & block_unique_digits[chute_field];

            let (line_neighbors, field_neighbors) = miniline.neighbors();

            for &(uniques, neighbors, is_pointing) in [
                (line_uniques, &field_neighbors, false),
                (block_uniques, &line_neighbors, true),
            ]
            .iter()
            .filter(|&&(uniques, _, _)| !uniques.is_empty())
            {
                for digit in uniques {
                    let found_conflicts =
                        on_locked_candidates(miniline, digit, miniline_poss_digits, neighbors, is_pointing);
                    if stop_after_first && found_conflicts {
                        return Ok(());
                    };
                }
            }
        }
    }

    Ok(())
}

#[inline]
fn find_unique<I: Iterator<Item = Set<Digit>>>(possibilities: I) -> (Set<Digit>, Set<Digit>, Set<Digit>) {
    let mut unsolved = Set::NONE;
    let mut multiple_unsolved = Set::NONE;

    for poss_digits in possibilities {
        multiple_unsolved |= unsolved & poss_digits;
        unsolved |= poss_digits;
    }
    // >= 1, >1, =1 occurences
    (unsolved, multiple_unsolved, unsolved.without(multiple_unsolved))
}
