use rand::seq::SliceRandom;

use crate::consts::*;
use crate::errors::{BlockParseError, InvalidEntry, LineParseError, NotEnoughRows};
use crate::generator::SudokuGenerator;
use crate::solver::SudokuSolver;

#[cfg(feature = "serde")]
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use std::{
    convert::{From, TryFrom},
    fmt, iter, ops, slice, str,
};

/// The 9x9 sudoku board represented as an array of length 81
type SudokuArray = [u8; N_CELLS];

/// The main structure exposing all the functionality of the library
///
/// `Sudoku`s can generated, constructed from arrays or parsed from `&str`s
/// in either the line or block format.
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Debug, Hash)]
pub struct Sudoku(pub(crate) SudokuArray);

#[cfg(feature = "serde")]
impl Serialize for Sudoku {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable() {
            serializer.serialize_str(&self.to_str_line())
        } else {
            serializer.serialize_bytes(&self.0)
        }
    }
}

// Visitors for serde
#[cfg(feature = "serde")]
struct ByteSudoku; // 81 byte format
#[cfg(feature = "serde")]
struct StrSudoku; // 81 char format (line sudoku)

#[cfg(feature = "serde")]
impl de::Visitor<'_> for ByteSudoku {
    type Value = Sudoku;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "81 numbers from 0 to 9 inclusive")
    }

    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        // FIXME: return proper error
        Sudoku::from_bytes_slice(v)
            .map_err(|_| E::custom("byte array has incorrect length or contains numbers not from 0 to 9"))
    }
}

#[cfg(feature = "serde")]
impl de::Visitor<'_> for StrSudoku {
    type Value = Sudoku;
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "81 numbers from 0 to 9 inclusive")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Sudoku::from_str_line(v).map_err(E::custom)
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for Sudoku {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if deserializer.is_human_readable() {
            deserializer.deserialize_str(StrSudoku)
        } else {
            deserializer.deserialize_bytes(ByteSudoku)
        }
    }
}

pub type Iter<'a> = iter::Map<slice::Iter<'a, u8>, fn(&u8) -> Option<u8>>; // Iter over Sudoku cells

/// Position symmetries for clues of generated sudokus
///
/// For use with functions like [`Sudoku::generate_with_symmetry`].
#[non_exhaustive]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(test, derive(strum_macros::EnumIter))]
pub enum Symmetry {
    /// Mirror along the vertical axis through the center of the sudoku
    VerticalMirror,
    /// Mirror along the horizontal axis through the center of the sudoku
    HorizontalMirror,
    /// Mirror along both vertical and horizontal axes through the center of the sudoku
    VerticalAndHorizontalMirror,
    /// Mirror along the diagonal from the top left to the bottom right
    DiagonalMirror,
    /// Mirror along the diagonal from the bottom left to the top right
    AntidiagonalMirror,
    /// Mirror along both diagonals
    BidiagonalMirror,
    /// 90° rotational symmetry (implies 180° and 270° symmetry)
    QuarterRotation,
    /// 180° rotational symmetry
    HalfRotation,
    /// Mirror along both vertical and horizontal axes, both diagonals and 90° and 180° rotation
    Dihedral,
    /// No symmetry
    None,
}

impl Symmetry {
    // For a given cell, returns all cells that need to be either all filled or all empty to uphold the symmetry
    fn corresponding_cells(self, cell: usize) -> Vec<usize> {
        let row = cell / 9;
        let col = cell % 9;
        let mirr = |line| 8 - line; // mirror along the axis orthogonal to `line`
        let cell_at = |row, col| row * 9 + col;
        match self {
            Symmetry::None => vec![cell],
            Symmetry::VerticalMirror => vec![cell, cell_at(row, mirr(col))],
            Symmetry::HorizontalMirror => vec![cell, cell_at(mirr(row), col)],
            Symmetry::VerticalAndHorizontalMirror => vec![
                cell,
                cell_at(mirr(row), col),
                cell_at(row, mirr(col)),
                cell_at(mirr(row), mirr(col)),
            ],
            Symmetry::DiagonalMirror => vec![cell, cell_at(col, row)],
            Symmetry::AntidiagonalMirror => vec![cell, cell_at(mirr(col), mirr(row))],
            Symmetry::BidiagonalMirror => vec![
                cell,
                cell_at(col, row),
                cell_at(mirr(col), mirr(row)),
                cell_at(mirr(row), mirr(col)),
            ],
            Symmetry::QuarterRotation => vec![
                cell,
                cell_at(mirr(row), mirr(col)),
                cell_at(col, mirr(row)),
                cell_at(mirr(col), row),
            ],
            Symmetry::HalfRotation => vec![cell, cell_at(mirr(row), mirr(col))],
            Symmetry::Dihedral => vec![
                cell,
                cell_at(row, mirr(col)),
                cell_at(mirr(row), col),
                cell_at(mirr(row), mirr(col)),
                cell_at(col, row),
                cell_at(col, mirr(row)),
                cell_at(mirr(col), row),
                cell_at(mirr(col), mirr(row)),
            ],
        }
    }
}

impl Sudoku {
    /// Generate a random, solved sudoku
    pub fn generate_solved() -> Self {
        SudokuGenerator::generate_solved()
    }

    /// Generate a random, uniquely solvable sudoku with 180° rotational symmetry.
    ///
    /// The puzzles are minimal in that no cell can be removed without losing uniquess of the solution
    /// whilst also upholding the symmetry.
    /// Most puzzles generated by this are easy.
    pub fn generate() -> Self {
        Sudoku::generate_with_symmetry(Symmetry::HalfRotation)
    }

    /// Generate a random, uniquely solvable sudoku with the desired symmetry.
    ///
    /// The puzzles are minimal in that no cell can be removed without losing uniquess of the solution
    /// whilst also upholding the symmetry.
    /// Most puzzles generated by this are easy.
    pub fn generate_with_symmetry(symmetry: Symmetry) -> Self {
        Sudoku::generate_with_symmetry_from(Sudoku::generate_solved(), symmetry)
    }

    /// Generate a random, uniqely solvable sudoku
    /// that has the same solution as the given `sudoku` by removing the contents of some of its cells.
    ///
    /// Equivalent to `Sudoku::generate_with_symmetry_from(sudoku, Symmetry::None)`
    pub fn generate_from(sudoku: Sudoku) -> Self {
        Sudoku::generate_with_symmetry_from(sudoku, Symmetry::None)
    }

    /// Generate a random, uniqely solvable sudoku
    /// that has the same solution as the given `sudoku` by removing the contents of some of its cells
    /// whilst upholding the `symmetry`. If the input sudoku is partially filled without the desired
    /// symmetry, the output may not have it either.
    /// The puzzles are minimal in that no cell can be removed without losing uniquess of solution.
    /// Most puzzles generated by this from solved sudokus are easy.
    ///
    /// If the source `sudoku` is invalid or has multiple solutions, it will be returned as is.
    pub fn generate_with_symmetry_from(mut sudoku: Sudoku, symmetry: Symmetry) -> Self {
        // this function is following
        // the approach outlined here: https://stackoverflow.com/a/7280517
        //
        // delete numbers from a filled sudoku cells in random order
        // after each deletion check for unique solvability
        // and backtrack on error

        // generate random order
        let mut cell_order = [0; N_CELLS];
        cell_order
            .iter_mut()
            .enumerate()
            .for_each(|(cell, place)| *place = cell);
        cell_order.shuffle(&mut rand::thread_rng());

        // With symmetries, many cells are equivalent.
        // If we've already visited one cell in a symmetry class, we can skip ahead
        // when encountering one of the other ones.
        let mut cell_visited = [false; 81];

        // remove cell content if possible without destroying uniqueness of solution
        for &cell in &cell_order[..] {
            let cells = symmetry.corresponding_cells(cell);
            if cell_visited[cells[0]] {
                continue;
            }
            let mut sudoku_tmp = sudoku;
            for cell in cells {
                cell_visited[cell] = true;
                sudoku_tmp.0[cell] = 0;
            }
            if sudoku_tmp.is_uniquely_solvable() {
                sudoku = sudoku_tmp;
            }
        }

        sudoku
    }

    /// Creates a sudoku from a byte slice.
    /// All numbers must be below 10. Empty cells are denoted by 0, clues by the numbers 1-9.
    /// The slice must be of length 81.
    pub fn from_bytes_slice(bytes: &[u8]) -> Result<Sudoku, crate::errors::FromBytesSliceError> {
        use std::convert::TryInto;
        Self::_from_bytes(
            bytes
                .try_into()
                .map_err(|_| crate::errors::FromBytesSliceError::WrongLength(bytes.len()))?,
        )
        .map_err(crate::errors::FromBytesSliceError::FromBytesError)
    }

    /// Creates a sudoku from a byte array.
    /// All numbers must be below 10. Empty cells are denoted by 0, clues by the numbers 1-9.
    pub fn from_bytes(bytes: SudokuArray) -> Result<Sudoku, crate::errors::FromBytesError> {
        Self::_from_bytes(&bytes)
    }

    fn _from_bytes(bytes: &SudokuArray) -> Result<Sudoku, crate::errors::FromBytesError> {
        match bytes.iter().fold(true, |valid, &byte| valid & (byte <= 9)) {
            true => Ok(Sudoku(*bytes)),
            false => Err(crate::errors::FromBytesError(())),
        }
    }

    /// Reads a sudoku in the line format.
    ///
    /// This is a concatenation of the digits in each cell, line by line from top to bottom.
    /// Digits must be in range of 1-9.
    /// `'_'`, `'.'` and `'0'` are accepted interchangeably as empty cells
    ///
    /// An optional comment is allowed after the sudoku,
    /// separated by ASCII whitespace, commas or semicolons,
    /// that is, any of ' ', '\t', '\n', '\r', ',', ';'
    ///
    /// Example:
    ///
    /// ```text
    /// ..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.. optional comment
    /// ```
    ///
    /// Stops parsing after the first sudoku
    pub fn from_str_line(s: &str) -> Result<Sudoku, LineParseError> {
        let chars = s.as_bytes();
        if let Ok(sudoku) = Sudoku::_from_str_line_fast_path(chars) {
            return Ok(sudoku);
        }

        let mut grid = [0; N_CELLS];
        let mut i = 0;
        for (cell, &ch) in grid.iter_mut().zip(chars) {
            match ch {
                b'_' | b'.' => *cell = 0,
                b'0'..=b'9' => *cell = ch - b'0',
                // space ends sudoku before grid is filled
                b' ' | b'\t' => return Err(LineParseError::NotEnoughCells(i)),
                _ => {
                    return Err(LineParseError::InvalidEntry(InvalidEntry {
                        cell: i,
                        ch: s[i as usize..].chars().next().unwrap(),
                    }))
                }
            }
            i += 1;
        }

        if i != N_CELLS as u8 {
            return Err(LineParseError::NotEnoughCells(i));
        }

        // if more than 81 elements, sudoku must be delimited
        if let Some(&ch) = chars.get(N_CELLS) {
            match ch {
                // delimiters, end of sudoku
                b'\t' | b' ' | b'\r' | b'\n' | b';' | b',' => (),
                // valid cell entry => too long
                b'_' | b'.' | b'0'..=b'9' => return Err(LineParseError::TooManyCells),
                // any other char can not be part of sudoku
                // without having both length and character wrong
                // treat like comment, but with missing delimiter
                _ => return Err(LineParseError::MissingCommentDelimiter),
            }
        }

        Ok(Sudoku(grid))
    }

    /// Parses sudokus under the assumption that everything is valid.
    /// Checks only once the end if the assumption was valid.
    //
    // FIXME: there is some duplication among this and the full parser.
    fn _from_str_line_fast_path(chars: &[u8]) -> Result<Sudoku, ()> {
        if chars.len() < 81 {
            return Err(());
        }
        // map valid ascii bytes into the range 0..=9
        // for from_bytes()
        let mut grid = [0; N_CELLS];
        grid.copy_from_slice(&chars[..81]);
        for cell in &mut grid[..] {
            *cell = cell.wrapping_sub(b'0');
            if *cell == b'_' - b'0' {
                *cell = 0;
            }
            if *cell == b'.'.wrapping_sub(b'0') {
                *cell = 0;
            }
        }

        let valid_ending = chars.get(81).map_or(true, |ch| match ch {
            b'\t' | b' ' | b'\r' | b'\n' | b';' | b',' => true,
            _ => false,
        });

        match valid_ending {
            true => Sudoku::from_bytes(grid).map_err(drop),
            false => Err(()),
        }
    }

    /// Reads a sudoku in the block format with or without field delimiters.
    ///
    /// Digits must be in range of 1-9.
    /// `'_'`, `'.'` and `'0'` are accepted interchangeably as empty cells
    ///
    /// Optional comments are accepted after each line. They must be delimited by
    /// ' ' or '\t', i.e. a space or a tab character.
    ///
    /// ```text
    /// __3_2_6__ optional comment
    /// 9__3_5__1 another comment
    /// __18_64__
    /// __81_29__
    /// 7_______8
    /// __67_82__
    /// __26_95__
    /// 8__2_3__9
    /// __5_1_3__
    /// ```
    ///
    /// alternatively also with field delimiters
    ///
    /// ```text
    /// __3|_2_|6__ optional comment
    /// 9__|3_5|__1 another comment
    /// __1|8_6|4__
    /// ---+---+--- comment: "-----------", i.e. '-' 11 times is also allowed
    /// __8|1_2|9__          delimiters have to be consistent across the entire
    /// 7__|___|__8          grid
    /// __6|7_8|2__
    /// ---+---+---
    /// __2|6_9|5__
    /// 8__|2_3|__9
    /// __5|_1_|3__
    /// ```
    ///
    /// Stops parsing after the first sudoku
    pub fn from_str_block(s: &str) -> Result<Sudoku, BlockParseError> {
        let mut grid = [0; N_CELLS];
        #[derive(PartialEq)]
        enum Format {
            Unknown,
            Delimited,
            DelimitedPlus,
            Bare,
        }
        let mut format = Format::Unknown;

        // Read a row per line
        let mut n_line_sud = 0;
        for (n_line_str, line) in s.lines().enumerate() {
            // if sudoku complete
            // enforce empty line (whitespace ignored)
            // Maybe allow comment lines in the future
            if n_line_sud == 9 {
                match line.trim().is_empty() {
                    true => break,
                    false => return Err(BlockParseError::TooManyRows),
                }
            }

            // if delimited, check horizontal field delimiters and skip over line
            if (format == Format::Delimited || format == Format::DelimitedPlus)
                && (n_line_str == 3 || n_line_str == 7)
            {
                if n_line_str == 3 && (line.starts_with("---+---+---") || line.starts_with("---+---+--- ")) {
                    format = Format::DelimitedPlus;
                }
                if format == Format::Delimited {
                    match !(line.starts_with("-----------") || line.starts_with("----------- ")) {
                        true => return Err(BlockParseError::IncorrectFieldDelimiter),
                        false => continue,
                    }
                }
                if format == Format::DelimitedPlus {
                    match !(line.starts_with("---+---+---") || line.starts_with("---+---+--- ")) {
                        true => return Err(BlockParseError::IncorrectFieldDelimiter),
                        false => continue,
                    }
                }
            }

            let mut n_col_sud = 0;
            for (str_col, ch) in line.chars().enumerate() {
                // if line complete
                if n_col_sud == 9 {
                    match ch {
                        // comment separator
                        ' ' | '\t' => break,
                        // valid entry, line too long
                        '1'..='9' | '_' | '.' | '0' => {
                            return Err(BlockParseError::InvalidLineLength(n_line_sud))
                        }
                        // invalid entry, interpret as comment but enforce separation
                        _ => return Err(BlockParseError::MissingCommentDelimiter(n_line_sud)),
                    }
                }

                // if in place of vertical field delimiters
                if str_col == 3 || str_col == 7 {
                    // Set parse mode on 4th char in 1st line
                    if format == Format::Unknown {
                        format = if ch == '|' {
                            Format::Delimited
                        } else {
                            Format::Bare
                        };
                    }
                    // check and skip over delimiters
                    if format == Format::Delimited || format == Format::DelimitedPlus {
                        match ch {
                            '|' => continue,
                            _ => return Err(BlockParseError::IncorrectFieldDelimiter),
                        }
                    }
                }

                let cell = n_line_sud * 9 + n_col_sud;
                match ch {
                    '_' | '.' => grid[cell as usize] = 0,
                    '0'..='9' => grid[cell as usize] = ch as u8 - b'0',
                    _ => {
                        return Err(BlockParseError::InvalidEntry(InvalidEntry {
                            cell: cell as u8,
                            ch,
                        }))
                    }
                }
                n_col_sud += 1;
            }
            if n_col_sud != 9 {
                return Err(BlockParseError::InvalidLineLength(n_line_sud));
            }

            n_line_sud += 1;
        }
        if n_line_sud != 9 {
            return Err(BlockParseError::NotEnoughRows(n_line_sud + 1)); // number of rows = index of last + 1
        }
        Ok(Sudoku(grid))
    }

    /// Reads a sudoku in a variety of block formats with very few constraints.
    ///
    /// '_', '.' and '0' are treated as empty cells. '1' to '9' as clues.
    /// Each line needs to have 9 valid cells.
    /// Lines that don't contain 9 valid entries are ignored.
    ///
    /// Stops parsing after the first sudoku.
    ///
    /// Due to the lax format rules, the only failure that can occur
    /// is that there are not enough rows.
    pub fn from_str_block_permissive(s: &str) -> Result<Sudoku, NotEnoughRows> {
        let mut grid = [0; N_CELLS];

        let mut valid_rows = 0;
        for line in s.lines() {
            let mut row_vals = [0; 9];
            let mut nums_in_row = 0;
            for ch in line.chars() {
                if ['.', '_'].contains(&ch) {
                    row_vals[nums_in_row] = 0;
                    nums_in_row += 1;
                } else if ch.is_ascii_digit() {
                    row_vals[nums_in_row] = ch as u8 - b'0';
                    nums_in_row += 1;
                }
                // full sudoko row, write to grid
                // ignore anything after in same row
                if nums_in_row == 9 {
                    grid[valid_rows * 9..valid_rows * 9 + 9].copy_from_slice(&row_vals);
                    valid_rows += 1;
                    break;
                }
            }
            if valid_rows == 9 {
                return Ok(Sudoku(grid));
            }
        }
        Err(NotEnoughRows(valid_rows as u8))
    }

    /// Find a solution to the sudoku. When a solution is found, it immediately stops searching and can therefore not guarantee uniqueness.
    /// If there is a unique solution, this will find it in, on average, half the time as [`Sudoku::solution`].
    /// Return `None` if no solution exists.
    pub fn some_solution(self) -> Option<Sudoku> {
        let mut buf = [[0; N_CELLS]];
        match self.solutions_up_to_buffer(&mut buf, 1) == 1 {
            true => Some(Sudoku(buf[0])),
            false => None,
        }
    }

    /// Solve sudoku and return solution if solution is unique.
    pub fn solution(self) -> Option<Sudoku> {
        // without at least 8 digits present, sudoku has multiple solutions
        // bitmask
        let mut nums_contained: u16 = 0;
        // same with less than 17 clues
        let mut n_clues = 0;
        self.iter().filter_map(|id| id).for_each(|num| {
            nums_contained |= 1 << num;
            n_clues += 1;
        });
        if n_clues < 17 || nums_contained.count_ones() < 8 {
            return None;
        };

        let mut solution = [[0; N_CELLS]];
        let n_solutions = self.solutions_up_to_buffer(&mut solution, 2);
        match n_solutions == 1 {
            true => Some(Sudoku(solution[0])),
            false => None,
        }
    }

    /// Counts number of solutions to sudoku up to `limit`.
    /// This solves the sudoku but does not return the solutions which allows for slightly faster execution.
    pub fn solutions_count_up_to(self, limit: usize) -> usize {
        SudokuSolver::from_sudoku(self)
            .ok()
            .map_or(0, |solver| solver.solutions_count_up_to(limit))
    }

    /// Checks whether sudoku has one and only one solution.
    /// This solves the sudoku but does not return the solution which allows for slightly faster execution.
    pub fn is_uniquely_solvable(self) -> bool {
        self.solutions_count_up_to(2) == 1
    }

    /// Solve sudoku and return the first `limit` solutions it finds. If less solutions exist, return only those. Return `None` if no solution exists.
    /// No specific ordering of solutions is promised. It can change across versions.
    pub fn solutions_up_to(self, limit: usize) -> Vec<Sudoku> {
        SudokuSolver::from_sudoku(self)
            .ok()
            .map_or(vec![], |solver| solver.solutions_up_to(limit))
    }

    /// Counts number of solutions to sudoku up to `limit` and writes any solution found into `target`
    /// up to its capacity. Additional solutions will be counted but not saved.
    /// No specific ordering of solutions is promised. It can change across versions.
    /// This is primarily meant for C FFI.
    pub fn solutions_up_to_buffer(self, target: &mut [SudokuArray], limit: usize) -> usize {
        SudokuSolver::from_sudoku(self)
            .ok()
            .map_or(0, |solver| solver.solutions_up_to_buffer(target, limit))
    }

    /// Check whether the sudoku is solved.
    //
    // iterates through all cells and checks for each row, col and block
    // if all 9 digits are present
    pub fn is_solved(&self) -> bool {
        use crate::bitset::Set;
        use crate::board::*;
        use crate::helper::HouseArray;

        // collection of digit sets for all 9 rows, 9 cols and 9 blocks
        let mut house_digits = HouseArray([Set::NONE; N_HOUSES]);

        for (cell, &content) in Cell::all().zip(self.0.iter()) {
            let digit = match Digit::new_checked(content) {
                None => return false,
                Some(digit) => digit.as_set(),
            };

            house_digits[cell.row()] |= digit;
            house_digits[cell.col()] |= digit;
            house_digits[cell.block()] |= digit;
        }

        house_digits == HouseArray([Set::ALL; N_HOUSES])
    }

    /// Returns number of filled cells
    pub fn n_clues(&self) -> u8 {
        self.0.iter().filter(|&&num| num != 0).count() as u8
    }

    /// Perform various transformations that create a different but equivalent sudoku.
    /// The transformations preserve the sudoku's validity and the amount of solutions
    /// as well a the applicability of solution strategies.
    /// Shuffling can be used to quickly generate sudokus of the same difficulty as a given sudoku.
    ///
    /// Transformations that are applied:
    /// - Relabel numbers, e.g. swap all 1s and all 3s (9! permutations)
    /// - Permute rows within their band and columns within their stack (3!<sup>3 * 2</sup> permutations)
    /// - Permute stacks and bands (3!<sup>2</sup> permutations)
    /// - Transpose the board, i.e. mirror it along the diagonal (2 permutations)
    ///   The remaining rotations as well as mirrorings can be produced by a combination with the other transformations
    ///
    /// This results in a total of up to 2 * 9! * 3!<sup>8</sup> = 1,218,998,108,160 permutations.
    /// Fewer permutations exist if the sudoku is symmetrical in respect to some combination(s) of the transformations.
    /// The vast majority of sudokus do not have any such symmetries (automorphisms). The highest number of automorphisms
    /// a sudoku can have is 648 and ~99.99% of all non-equivalent sudokus have only 1, the identity transformation.

    // TODO: Deduplicate the shuffle_*lines_or_chutes* functions
    //       for some reason the shuffle_bands and shuffle_stacks functions work faster in their current form
    //       rather than with a generic function abstracting over both.
    pub fn shuffle(&mut self) {
        let transformation = crate::board::canonicalization::Transformation::random();
        transformation.apply(self);
    }

    /// Returns a [`shuffled`](Sudoku::shuffle) copy of the sudoku.
    pub fn shuffled(mut self) -> Self {
        self.shuffle();
        self
    }

    /// Returns the canonical representation of this sudoku and its automorphism count.
    ///
    /// All sudokus that can be translated into each other via validity preserving transformations belong to the same
    /// equivalence class (see [`Sudoku::shuffle`] docs for a list of transformations). The sudoku returned from this
    /// function is the same for all sudokus in the equivalence class, if fully solved. Non-solved sudokus will be in
    /// the canonical form after solving. This function allows therefore to check whether two sudokus are
    /// equivalent. It can be used for both for solved and unsolved puzzles.
    ///
    /// Some sudokus have multiple transformations resulting in the exact same sudoku, so called automorphisms.
    /// The number of these is a byproduct of canonicalization and returned as well.
    /// Every sudoku has at least 1 automorphism, the identity transformation.
    ///
    /// This function uses the lexicographically minimal permutation as the canonical form.
    ///
    /// Limited to uniquely solvable sudokus. Returns `None` otherwise.
    pub fn canonicalized(&self) -> Option<(Sudoku, usize)> {
        let solved_sudoku = if self.is_solved() {
            *self
        } else if let Some(solved) = self.solution() {
            solved
        } else {
            return None;
        };

        let mut sudoku = *self;
        let (_, transformation, n_automorphisms) =
            super::canonicalization::find_canonical_sudoku_and_transformation(solved_sudoku);
        transformation.apply(&mut sudoku);
        Some((sudoku, n_automorphisms))
    }

    /// Returns an Iterator over sudoku, going from left to right, top to bottom
    pub fn iter(&self) -> Iter {
        self.0.iter().map(num_to_opt)
    }

    /// Returns a byte array for the sudoku.
    /// Empty cells are denoted by 0, clues by the numbers 1-9.
    pub fn to_bytes(self) -> SudokuArray {
        self.0
    }

    /// Returns a representation of the sudoku in line format that can be printed
    /// and which derefs into a &str
    ///
    /// ```
    /// use sudoku::Sudoku;
    ///
    /// let mut grid = [0; 81];
    /// grid[3] = 5;
    /// let sudoku = Sudoku::from_bytes(grid).unwrap();
    /// let line = sudoku.to_str_line(); // :SudokuLine
    /// println!("{}", line);
    ///
    /// let line_str: &str = &line;
    /// assert_eq!(
    ///     "...5.............................................................................",
    ///     line_str
    /// );
    /// ```
    pub fn to_str_line(&self) -> SudokuLine {
        let mut chars = [0; N_CELLS];
        for (char_, entry) in chars.iter_mut().zip(self.iter()) {
            *char_ = match entry {
                Some(num) => num + b'0',
                None => b'.',
            };
        }
        SudokuLine(chars)
    }

    /// Returns a value that prints a block representation of the sudoku
    /// when formatted via the `Display` trait.
    ///
    /// ```
    /// use sudoku::Sudoku;
    ///
    /// let mut grid = [0; 81];
    /// grid[3] = 5;
    /// grid[36..45].copy_from_slice(&[1, 2, 3, 4, 5, 6, 7, 8, 9]);
    /// let sudoku = Sudoku::from_bytes(grid).unwrap();
    /// let block = sudoku.display_block(); // :SudokuBlock
    ///
    /// let block_string = format!("{}", block);
    /// assert_eq!(
    ///     &block_string,
    /// "
    /// ___ 5__ ___
    /// ___ ___ ___
    /// ___ ___ ___
    ///
    /// ___ ___ ___
    /// 123 456 789
    /// ___ ___ ___
    ///
    /// ___ ___ ___
    /// ___ ___ ___
    /// ___ ___ ___"
    /// );
    /// ```
    pub fn display_block(&self) -> SudokuBlock {
        SudokuBlock(self.0)
    }
}

#[rustfmt::skip]
#[allow(clippy::trivially_copy_pass_by_ref)]
fn num_to_opt(num: &u8) -> Option<u8> {
    if *num == 0 { None } else { Some(*num) }
}

impl fmt::Display for Sudoku {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.to_str_line(), f)
    }
}

impl TryFrom<SudokuArray> for Sudoku {
    type Error = crate::errors::FromBytesError;

    fn try_from(bytes: SudokuArray) -> Result<Self, Self::Error> {
        Self::from_bytes(bytes)
    }
}

impl TryFrom<&[u8]> for Sudoku {
    type Error = crate::errors::FromBytesSliceError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Self::from_bytes_slice(value)
    }
}

impl From<Sudoku> for SudokuArray {
    fn from(sudoku: Sudoku) -> Self {
        sudoku.to_bytes()
    }
}

/// Container for the &str representation of a sudoku
// MUST ALWAYS contain valid utf8
//
// the bytes representation uses b'.' for empty cells, which is below `0` and therefore
// this orders just like the regular sudoku would.
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub struct SudokuLine(SudokuArray);

impl std::hash::Hash for SudokuLine {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        <str as std::hash::Hash>::hash(self, state)
    }
}

impl fmt::Debug for SudokuLine {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        <str as fmt::Debug>::fmt(self, fmt)
    }
}

impl ops::Deref for SudokuLine {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        str::from_utf8(&self.0).unwrap()
    }
}

impl fmt::Display for SudokuLine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <str as fmt::Display>::fmt(self, f)
    }
}

/// Sudoku that will be printed in block format.
/// This exists primarily for debugging.
#[derive(Copy, Clone, PartialOrd, Ord, Hash, PartialEq, Eq, Debug)]
pub struct SudokuBlock(SudokuArray);

impl fmt::Display for SudokuBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::board::{Cell, Digit};
        for (digit, cell) in self.0.iter().cloned().map(Digit::new_checked).zip(Cell::all()) {
            #[rustfmt::skip]
            #[allow(clippy::write_with_newline)]
            match (cell.row().get(), cell.col().get()) {
                (_, 3) | (_, 6) => write!(f, " ")?,    // seperate fields in columns
                (3, 0) | (6, 0) => write!(f, "\n\n")?, // separate fields in rows
                (_, 0)          => write!(f, "\n")?,   // separate lines not between fields
                _ => {},
            };
            match digit {
                None => write!(f, "_")?,
                Some(dig) => write!(f, "{}", dig.get())?,
            };
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use strum::IntoEnumIterator;

    // each cell in a symmetry class must map to the same set of cells
    #[test]
    fn test_symmetry_all_cells_equivalent() {
        for symmetry in Symmetry::iter() {
            let cells_in_class = |cell| {
                let mut cells = symmetry.corresponding_cells(cell);
                cells.sort();
                cells
            };

            for cell in 0..81 {
                let equivalent_cells = cells_in_class(cell);
                for &cell in &equivalent_cells {
                    assert_eq!(equivalent_cells, cells_in_class(cell));
                }
            }
        }
    }

    // More complicated symmetries can be expressed as a combination of simple symmetries.
    #[test]
    fn test_symmetry_composite_symmetries() {
        use std::collections::HashSet;
        fn multi_symmmetry_corresponding_cells(symmetries: &[Symmetry], cell: usize) -> HashSet<usize> {
            let mut cells = HashSet::new();
            cells.insert(cell);
            for &symmetry in symmetries {
                let mut new_cells = vec![];
                for &cell in &cells {
                    new_cells.extend(symmetry.corresponding_cells(cell));
                }
                cells.extend(new_cells);
            }
            cells
        }

        use Symmetry::*;
        for &(symmetry, equivalent_symmetries) in &[
            (
                Dihedral,
                &[
                    // AntidiagonalMirror and both rotations could be included
                    // but these are enough to produce full dihedral symmetry.
                    VerticalMirror,
                    HorizontalMirror,
                    DiagonalMirror,
                ][..],
            ),
            (VerticalAndHorizontalMirror, &[VerticalMirror, HorizontalMirror]),
            (BidiagonalMirror, &[DiagonalMirror, AntidiagonalMirror]),
        ] {
            for cell in 0..81 {
                let symmetry_cells = symmetry
                    .corresponding_cells(cell)
                    .into_iter()
                    .collect::<HashSet<_>>();
                let equivalent_symmetry_cells =
                    multi_symmmetry_corresponding_cells(equivalent_symmetries, cell);
                assert_eq!(symmetry_cells, equivalent_symmetry_cells);
            }
        }
    }
}
