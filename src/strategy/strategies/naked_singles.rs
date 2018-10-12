use helper::{Unsolvable, CellArray};
use bitset::Set;
use board::{Digit, Candidate, Cell};

pub(crate) fn find_naked_singles(
    cell_poss_digits: &CellArray<Set<Digit>>,
    stop_after_first: bool,
    mut on_new_entry: impl FnMut(Candidate) -> Result<(), Unsolvable>,
) -> Result<(), Unsolvable> {
    for (cell, poss_digits) in Cell::all().zip(cell_poss_digits.iter()) {
        // if Err(_), then it's Set::NONE and the cell is already solved (or impossible)
        // skip in that case (via unwrap_or(None))
        if let Some(digit) = poss_digits.unique().unwrap_or(None) {
            let candidate = Candidate { cell, digit };

            on_new_entry(candidate)?;
            if stop_after_first {
                return Ok(());
            }
        }
    }
    Ok(())
}
