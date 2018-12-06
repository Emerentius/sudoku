use crate::Sudoku;

pub(crate) fn canonicalize_solved_sudoku(sudoku: Sudoku) -> Sudoku {
    let mut min_transformations = vec![];

    {
        let mut band_minimum = [9; 16];
        for &transpose in &[false, true] {
            let mut sudoku = sudoku;
            if transpose {
                sudoku.transpose();
            }

            for band in 0..3 {
                find_minlex_band_transformation(sudoku, band, transpose, &mut band_minimum, &mut min_transformations);
            }
        }
    }

    min_transformations.into_iter()
        .map(|trans| {
            let BandTransformation {
                band, transposed, row_permutation, stack_permutation, col_permutations, digit_remapping,
            } = trans;
            // apply the transformation for the minimal band
            let mut min_sudoku = sudoku;
            if transposed {
                min_sudoku.transpose();
            }

            min_sudoku.swap_bands(0, band);

            permute(&mut min_sudoku, row_permutation, 0, Sudoku::swap_rows);
            permute(&mut min_sudoku, stack_permutation, 0, Sudoku::swap_stacks);

            for (stack, perm) in (0..3).zip(col_permutations.iter()) {
                permute(&mut min_sudoku, *perm, stack * 3, Sudoku::swap_cols);
            }

            for cell in &mut min_sudoku.0[..] {
                *cell = digit_remapping[ *cell as usize - 1];
            }

            // now sort the remaining two bands
            // first, sort the rows in each band, then sort the two bands
            for band in 1..3 {
                let offset = band * 3;

                let first_choice = (0..3).min_by_key(|&row| &min_sudoku.0[9*(offset + row) as usize..][..9]).unwrap();
                min_sudoku.swap_rows(offset, offset + first_choice);

                let second_choice = (0..2).min_by_key(|&row| &min_sudoku.0[9*(offset + 1 + row) as usize..][..9]).unwrap();
                min_sudoku.swap_rows(offset + 1, offset + 1 + second_choice);
            }

            if min_sudoku.0[27..54] > min_sudoku.0[54..] {
                min_sudoku.swap_bands(1, 2);
            };

            min_sudoku
        })
        .min()
        .unwrap()
}

#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub(crate) struct BandTransformation {
    band: u8,
    transposed: bool,
    row_permutation: Permutation3,
    stack_permutation: Permutation3,
    col_permutations: [Permutation3; 3],
    digit_remapping: [u8; 9],
}

#[derive(Default)]
#[derive(PartialOrd, Ord, PartialEq, Eq, Copy, Clone)]
struct Permutation3(u8, u8);

impl Permutation3 {
    fn from_choices(choice3: u8, choice2: u8) -> Self {
        Permutation3(choice3, choice2)
    }

    fn new(perm: u8) -> Self {
        Permutation3(perm % 3, perm / 3)
    }

    fn choice3(self) -> u8 {
        self.0
    }

    fn choice2(self) -> u8 {
        self.1
    }
}

fn permute<T: ?Sized>(sudoku: &mut T, permutation: Permutation3, offset: u8, mut swapper: impl FnMut(&mut T, u8, u8)) {
    swapper(sudoku, offset, offset + permutation.choice3());
    swapper(sudoku, offset + 1, offset + 1 + permutation.choice2());
}

/// Searches through all permutations of a band to find the minimal lexicographical representation
/// and returns the transformation required to get there.
/// `band_minimum` is the lowest band seen so far. If no transformation results in a lower valued band,
/// then no transformation is returned.
pub(crate) fn find_minlex_band_transformation(sudoku: Sudoku, band_nr: u8, transposed: bool, band_minimum: &mut [u8; 16], minimal_transformations: &mut Vec<BandTransformation>) {
    let first_cell = band_nr * 27;
    let mut band = [0; 27];
    band.copy_from_slice(&sudoku.0[first_cell as usize..][..27]);

    for first_box in 0..3 {
        let mut band = band;
        swap_stack_in_band(&mut band, 0, first_box);

        for rows_perm in (0..6).map(Permutation3::new) {
            let mut band = band;
            permute(&mut band[..], rows_perm, 0, swap_rows);

            'cols_perm: for cols_perm in (0..6).map(Permutation3::new) {
                let mut band = band;
                permute(&mut band[..], cols_perm, 0, swap_cols_in_band);

                // the first two digits in the second row MUST be 4, then 5
                // just like the 4th and 5th digit in the first row
                // so it's clear which box is in the second stack and how it is permuted
                let digit4 = band[9];
                let pos4 = band[3..9].iter().position(|&num| num == digit4).unwrap() as u8;

                let second_box_choice = pos4 / 3;
                let second_box_first_col = pos4 % 3;

                swap_stack_in_band(&mut band, 1, 1 + second_box_choice);
                swap_cols_in_band(&mut band, 3, 3 + second_box_first_col);

                let digit5 = band[10];
                let second_box_second_col = if band[4] != digit5 { 1 } else { 0 };
                swap_cols_in_band(&mut band, 4, 4 + second_box_second_col);

                if band[4] != digit5 {
                    // impossible to reach 123456789 in first row, can't be minlex
                    continue
                }

                // impossible to find a lower valued band
                // by permuting the last 3 cols, if the first 3 are already larger
                if band_minimum[0] == 6 && band[11] != band[5] {
                    continue
                }

                let mut mapping = [0; 9];
                for (new_num, &old_num) in (1..7).zip(band.iter()) {
                    mapping[old_num as usize - 1] = new_num;
                }

                for last_box_cols_perm in (0..6).map(Permutation3::new) {
                    let mut band = band;
                    permute(&mut band[..], last_box_cols_perm, 6, swap_cols_in_band);

                    mapping[ band[6] as usize - 1 ] = 7;
                    mapping[ band[7] as usize - 1 ] = 8;
                    mapping[ band[8] as usize - 1 ] = 9;

                    for cell in &mut band[11..] {
                        *cell = mapping[*cell as usize - 1];
                    }

                    let band_value = &band[11..];
                    if band_value <= band_minimum {
                        if band_value < band_minimum {
                            band_minimum.copy_from_slice(band_value);
                            minimal_transformations.clear();
                        }
                        minimal_transformations.push(
                            BandTransformation {
                                band: band_nr,
                                transposed,
                                row_permutation: rows_perm,
                                stack_permutation: Permutation3::from_choices(first_box, second_box_choice),
                                col_permutations: [
                                    cols_perm,
                                    Permutation3::from_choices(second_box_first_col, second_box_second_col),
                                    last_box_cols_perm,
                                ],
                                digit_remapping: mapping,
                            }
                        );
                    }
                }
            }
        }
    }
}

#[inline(never)]
pub(crate) fn swap_cols_in_band(band: &mut [u8], col1: u8, col2: u8) {
    if col1 == col2 { return }
    debug_assert!(col1 < 9);
    debug_assert!(col2 < 9);

    swap_cells(
        band,
        (0..3).map(|row| (row*9 + col1 as usize, row*9 + col2 as usize))
    )
}

#[inline(never)]
pub(crate) fn swap_stack_in_band(band: &mut [u8], stack1: u8, stack2: u8) {
    if stack1 == stack2 { return }
    debug_assert!(stack1 < 3);
    debug_assert!(stack2 < 3);

    for inner_col in 0..3 {
        swap_cols_in_band(band, stack1 * 3 + inner_col, stack2 * 3 + inner_col);
    }
}

#[inline(never)]
fn swap_rows(band: &mut [u8], row1: u8, row2: u8) {
    if row1 == row2 { return }
    debug_assert!(row1 < 3);
    debug_assert!(row2 < 3);
    let start1 = (row1*9) as usize;
    let start2 = (row2*9) as usize;
    swap_cells(
        band,
        (start1..start1+9).zip(start2..start2+9)
    )
}

// takes iter of cell index pairs and swaps the corresponding cells
#[inline(never)]
fn swap_cells(slice: &mut [u8], iter: impl Iterator<Item=(usize, usize)>) {
	for (idx1, idx2) in iter {
		debug_assert!(idx1 != idx2);

		let a = slice[idx1];
		let b = slice[idx2];
		slice[idx1] = b;
		slice[idx2] = a;
	}
}
