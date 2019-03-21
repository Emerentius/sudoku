#![allow(unused)]
use super::prelude::*;

// TODO: unify with xy-wing and possibly more wings (wxyz-wing?)
pub(crate) fn find_xyz_wing(
    cells_poss_digits: &CellArray<Set<Digit>>,
    stop_after_first: bool,
    mut on_xyz_wing: impl FnMut(
        (Cell, Set<Digit>), // hinge
        [(Cell, Set<Digit>); 2], // pincers
    ) -> bool,
) -> Result<(), Unsolvable> {
    for cell in Cell::all() {
        let poss_digits = cells_poss_digits[cell];
        if poss_digits.len() != 3 {
            continue
        }

        let row_cells = cell.row().cells();
        let col_cells = cell.col().cells();
        let block_cells = cell.block().cells();

        // nonoverlapping (disjoint = dj) cell sets
        let row_dj = row_cells.without(block_cells);
        let col_dj = col_cells.without(block_cells);
        let block_row_dj = block_cells.without(row_cells);
        let block_col_dj = block_cells.without(col_cells);

        for &(cells1, cells2) in &[(block_row_dj, row_dj), (block_col_dj, col_dj)] {
            let overlapping_bivalue_cells = |cells: Set<Cell>| {
                cells.into_iter()
                    .map(|cell| (cell, cells_poss_digits[cell]))
                    .filter(|&(_, other_poss_digs)| {
                        other_poss_digs.len() == 2 && (poss_digits & other_poss_digs).len() == 2
                    })
            };
            for (cell1, poss_digs1) in overlapping_bivalue_cells(cells1) {
                for (cell2, poss_digs2) in overlapping_bivalue_cells(cells2) {
                    let common_digits = poss_digs1 & poss_digs2;
                    if common_digits.len() != 1
                        || (poss_digs1 | poss_digs2 | poss_digits).len() != 3 {
                        continue
                    }

                    // found xy-wing
                    let found_conflicts = on_xyz_wing(
                        (cell, poss_digits),
                        [(cell1, poss_digs1), (cell2, poss_digs2)],
                    );
                    if found_conflicts && stop_after_first {
                        return Ok(());
                    }
                }
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    // sudoku taken from http://www.sudokuwiki.org/Y_Wing_Strategy, Example 1 (2019-03-18)
    #[test]
    fn xyz_wing() {
        let solver = crate::strategy::StrategySolver::from_grid_state_str(
            "
            ┌─────────────────┬───────────────────┬────────────────┐
            │ 38   9     2    │ 46    48    1     │ 7     5   346  │
            │ 5    134   1467 │ 2     47    679   │ 346   19  8    │
            │ 146  148   1467 │ 4569  3     56789 │ 2     19  46   │
            ├─────────────────┼───────────────────┼────────────────┤
            │ 38   7     5    │ 13    128   4     │ 9     6   12   │
            │ 2    38    14   │ 139   6     89    │ 148   7   5    │
            │ 14   6     9    │ 7     125   258   │ 148   3   124  │
            ├─────────────────┼───────────────────┼────────────────┤
            │ 146  145   8    │ 1456  9     567   │ 1356  2   1367 │
            │ 7    1245  146  │ 1456  1245  3     │ 156   8   9    │
            │ 9    125   3    │ 8     1257  2567  │ 156   4   167  │
            └─────────────────┴───────────────────┴────────────────┘"
        );

        let (_, deductions) = solver.solve(&[crate::strategy::Strategy::XyzWing]).unwrap_err();
        assert_eq!(deductions.len(), 1);
        assert_eq!(
            deductions.get(0).unwrap(),
            crate::strategy::Deduction::Wing {
                hinge: Cell::new(5*9 + 8),
                hinge_digits: Digit::new(1).as_set() | Digit::new(2) | Digit::new(4),
                pincers: Cell::new(3*9 + 8).as_set() | Cell::new(5*9 + 0),
                conflicts: &[
                    Candidate {
                        cell: Cell::new(5*9 + 6),
                        digit: Digit::new(1),
                    }
                ][..],
            }
        );
    }
}
