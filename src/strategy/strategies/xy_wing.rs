use super::prelude::*;

pub(crate) fn find_xy_wing(
    cells_poss_digits: &CellArray<Set<Digit>>,
    stop_after_first: bool,
    mut on_xy_wing: impl FnMut(
        (Cell, Set<Digit>),      // hinge
        [(Cell, Set<Digit>); 2], // pincers
    ) -> bool,
) -> Result<(), Unsolvable> {
    for cell in Cell::all() {
        let poss_digits = cells_poss_digits[cell];
        if poss_digits.len() != 2 {
            continue;
        }

        let row_cells = cell.row().cells();
        let col_cells = cell.col().cells();
        let block_cells = cell.block().cells();

        // nonoverlapping (disjoint = dj) cell sets
        let row_dj = row_cells.without(block_cells);
        let col_dj = col_cells.without(block_cells);
        let block_row_dj = block_cells.without(row_cells);
        let block_col_dj = block_cells.without(col_cells);

        for &(cells1, cells2) in &[(block_row_dj, row_dj), (block_col_dj, col_dj), (row_dj, col_dj)] {
            let overlapping_bivalue_cells = |cells: Set<Cell>| {
                cells
                    .into_iter()
                    .map(|cell| (cell, cells_poss_digits[cell]))
                    .filter(|&(_, other_poss_digs)| {
                        other_poss_digs.len() == 2 && (poss_digits & other_poss_digs).len() == 1
                    })
            };
            for (cell1, poss_digs1) in overlapping_bivalue_cells(cells1) {
                for (cell2, poss_digs2) in overlapping_bivalue_cells(cells2) {
                    let common_digits = poss_digs1 & poss_digs2;
                    if common_digits.len() != 1 || (poss_digs1 | poss_digs2 | poss_digits).len() != 3 {
                        continue;
                    }

                    // found xy-wing
                    let found_conflicts =
                        on_xy_wing((cell, poss_digits), [(cell1, poss_digs1), (cell2, poss_digs2)]);
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
    fn xy_wing() {
        let sudoku = Sudoku::from_str_line(
            "9..24.....5.69.231.2..5..9..9.7..32...29356.7.7...29...69.2..7351..79.622.7.86..9",
        )
        .unwrap();
        let solver = crate::strategy::StrategySolver::from_sudoku(sudoku);
        let (_, deductions) = solver.solve(&[crate::strategy::Strategy::XyWing]).unwrap_err();

        // TODO: match both xy-wings

        if let crate::strategy::Deduction::Wing {
            hinge,
            pincers,
            conflicts,
            ..
        } = deductions.get(0).unwrap()
        {
            assert_eq!(hinge.get(), 1);
            assert_eq!(pincers, Cell::new(1 * 9 + 2).as_set() | Cell::new(8 * 9 + 1));

            let conflict = Candidate {
                cell: Cell::new(7 * 9 + 2),
                digit: Digit::new(4),
            };
            assert_eq!(conflicts, &[conflict]);
        } else {
            panic!("No XyWing found.");
        }
    }
}
