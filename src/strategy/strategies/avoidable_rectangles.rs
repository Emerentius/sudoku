// WIP
use super::prelude::*;

fn find_avoidable_rectangles(
    filled_cells: Sudoku,
    clues: Sudoku,
    cell_poss_digits: &CellArray<Set<Digit>>,
    stop_after_first: bool,
    mut on_avoidable_rectangle: impl FnMut(
        // 2 lines and 2 cols of the rectangle
        Set<Line>,
        // impossible candidate
        Candidate,
    )
) -> Result<(), Unsolvable> {
    let cell = |row: u8, col: u8| row * 9 + col;
    for row1 in 0..8 {
        for row2 in row1+1..9 {
            let rows_in_same_chute = row1 / 3 == row2 / 3;
            for col1 in 0..8 {
                for col2 in col1+1..9 {
                    let cols_in_same_chute = col1 / 3 == col2 / 3;
                    if !(rows_in_same_chute ^ cols_in_same_chute) {
                        continue
                    }

                    // find the digits that are already set, their count
                    // and whether there are any clues
                    let cells = [cell(row1, col1), cell(row1, col2), cell(row2, col1), cell(row2, col2)];
                    let mut n_cells_set = 0;
                    let mut n_cells_clues = 0;
                    let mut set_digits = Set::NONE;
                    let mut free_cell = 0;
                    for &cell in &cells {
                        match filled_cells.0[cell as usize] {
                            0 => free_cell = cell,
                            digit => {
                                set_digits |= Digit::new(digit);
                                n_cells_set += 1;
                                if clues.0[cell as usize] != 0 {
                                    n_cells_clues += 1;
                                }
                            }
                        }
                    }

                    if n_cells_set == 3 && n_cells_clues == 0 && set_digits.len() == 2 {
                        let candidates_remaining_cell = cell_poss_digits[Cell::new(free_cell)];
                        if candidates_remaining_cell.overlaps(set_digits) {
                            // found an avoidable rectangle
                            let row1 = Row::new(row1);
                            let row2 = Row::new(row2);
                            let col1 = Col::new(col1);
                            let col2 = Col::new(col2);
                            if let Some(digit) = (candidates_remaining_cell & set_digits).unique()? {
                                on_avoidable_rectangle(
                                    Line::from(row1).as_set() | Line::from(row2) | Line::from(col1) | Line::from(col2),
                                    Candidate {
                                        cell: Cell::new(free_cell),
                                        digit
                                    }
                                )
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(())
}
