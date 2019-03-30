use crate::Sudoku;

#[derive(PartialEq, Eq, Clone, Copy)]
pub(crate) struct Transformation {
    transpose: bool,
    band_permutation: Permutation3,
    stack_permutation: Permutation3,
    row_permutations: ChuteLinePermutations,
    col_permutations: ChuteLinePermutations,
    digit_remapping: [u8; 9],
}

impl Transformation {
    pub(crate) fn apply(self, sudoku: &mut Sudoku) {
        // order of some operations is important
        // transpose before stacks, bands
        // stacks before cols
        // bands before rows
        if self.transpose {
            sudoku.transpose();
        }

        self.band_permutation.apply(sudoku, 0, Sudoku::swap_bands);
        self.stack_permutation.apply(sudoku, 0, Sudoku::swap_stacks);

        self.col_permutations.apply(sudoku, Sudoku::swap_cols);
        self.row_permutations.apply(sudoku, Sudoku::swap_rows);

        apply_digit_mapping(self.digit_remapping, sudoku);
    }
}

impl Permutation3 {
    fn apply(self, sudoku: &mut Sudoku, offset: u8, f: impl FnMut(&mut Sudoku, u8, u8)) {
        permute(sudoku, self, offset, f);
    }
}

// permutations of the 3 lines in each of the 3 chutes.
// The chutes have to be either all bands or all stacks.
#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
struct ChuteLinePermutations([Permutation3; 3]);

impl ChuteLinePermutations {
    fn apply(self, sudoku: &mut Sudoku, mut f: impl FnMut(&mut Sudoku, u8, u8)) {
        for (chute, perm) in (0..3).zip(self.0.iter()) {
            perm.apply(sudoku, 3 * chute, &mut f);
        }
    }
}

fn apply_digit_mapping(digit_remapping: [u8; 9], sudoku: &mut Sudoku) {
    for cell_digit in &mut sudoku.0[..] {
        if *cell_digit == 0 {
            continue;
        }
        *cell_digit = digit_remapping[ *cell_digit as usize - 1];
    }
}

pub(crate) fn find_canonical_sudoku_and_transformation(sudoku: Sudoku) -> (Sudoku, Transformation) {
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
            let MinBandTransformation {
                band, transposed, row_permutation, stack_permutation, col_permutations, digit_remapping,
            } = trans;
            // apply the transformation for the minimal band
            let mut min_sudoku = sudoku;
            if transposed {
                min_sudoku.transpose();
            }

            min_sudoku.swap_bands(0, band);

            row_permutation.apply(&mut min_sudoku, 0, Sudoku::swap_rows);

            stack_permutation.apply(&mut min_sudoku, 0, Sudoku::swap_stacks);
            col_permutations.apply(&mut min_sudoku, Sudoku::swap_cols);

            apply_digit_mapping(digit_remapping, &mut min_sudoku);

            // now sort the remaining two bands
            // first, sort the rows in each band, then sort the two bands
            let mut row_perm2 = sort_rows_in_band_and_find_permutation(&mut min_sudoku, 1);
            let mut row_perm3 = sort_rows_in_band_and_find_permutation(&mut min_sudoku, 2);

            let needs_band_switch = min_sudoku.0[27..54] > min_sudoku.0[54..];
            let band_choice2 = needs_band_switch as u8;
            if needs_band_switch {
                std::mem::swap(&mut row_perm2, &mut row_perm3);
            }
            min_sudoku.swap_bands(1, 1 + band_choice2);

            let transformation = Transformation {
                transpose: transposed,
                band_permutation: Permutation3::from_choices(band, band_choice2),
                stack_permutation,
                row_permutations: ChuteLinePermutations([row_permutation, row_perm2, row_perm3]),
                col_permutations,
                digit_remapping,
            };

            (min_sudoku, transformation)
        })
        .min_by_key(|&(sudoku, _)| sudoku)
        .unwrap()
}

fn sort_rows_in_band_and_find_permutation(sudoku: &mut Sudoku, band: u8) -> Permutation3 {
    let row_offset = band * 3;
    let first_choice = (0..3).min_by_key(|&row| &sudoku.0[9*(row_offset + row) as usize..][..9]).unwrap();
    sudoku.swap_rows(row_offset, row_offset + first_choice);

    let second_choice = (0..2).min_by_key(|&row| &sudoku.0[9*(row_offset + 1 + row) as usize..][..9]).unwrap();
    sudoku.swap_rows(row_offset + 1, row_offset + 1 + second_choice);
    Permutation3::from_choices(first_choice, second_choice)
}

/// The transformation needed to get the lexicographically minimal first band.
/// After finding this one, the other two bands need only be sorted and we have
/// the minlex sudoku. This cuts the search space considerably.
#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub(crate) struct MinBandTransformation {
    band: u8,
    transposed: bool,
    row_permutation: Permutation3,
    stack_permutation: Permutation3,
    col_permutations: ChuteLinePermutations,
    digit_remapping: [u8; 9],
}

#[derive(Default)]
#[derive(PartialOrd, Ord, PartialEq, Eq, Copy, Clone)]
struct Permutation3(u8, u8);

impl Permutation3 {
    fn from_choices(choice3: u8, choice2: u8) -> Self {
        debug_assert!(choice3 < 3);
        debug_assert!(choice2 < 2);
        Permutation3(choice3, choice2)
    }

    fn new(perm: u8) -> Self {
        debug_assert!(perm < 6);
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
pub(crate) fn find_minlex_band_transformation(sudoku: Sudoku, band_nr: u8, transposed: bool, band_minimum: &mut [u8; 16], minimal_transformations: &mut Vec<MinBandTransformation>) {
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
                            MinBandTransformation {
                                band: band_nr,
                                transposed,
                                row_permutation: rows_perm,
                                stack_permutation: Permutation3::from_choices(first_box, second_box_choice),
                                col_permutations: ChuteLinePermutations([
                                    cols_perm,
                                    Permutation3::from_choices(second_box_first_col, second_box_second_col),
                                    last_box_cols_perm,
                                ]),
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

// check that the canonical sudoku found in the search
// matches the original sudoku after transformation.apply()
#[test]
fn transformation_from_struct() {
    fn read_sudokus(sudokus_str: &str) -> Vec<Sudoku> {
        sudokus_str
            .lines()
            .map(|line| Sudoku::from_str_line(line).unwrap_or_else(|err| panic!("{:?}", err)))
            .collect()
    }

    let sudokus = read_sudokus( include_str!("../../sudokus/Lines/easy_sudokus.txt") );

    for sudoku in sudokus {
        let mut solved_sudoku = sudoku.solve_unique().unwrap();
        let (canonical_sudoku, transformation) = find_canonical_sudoku_and_transformation(solved_sudoku);
        transformation.apply(&mut solved_sudoku);
        assert_eq!(canonical_sudoku, solved_sudoku, "\n{}\n{}", canonical_sudoku, solved_sudoku);
    }
}
