//! Errors that may occur when reading sudokus
#[cfg(doc)]
use crate::Sudoku;

/// Error for [`Sudoku::from_bytes`]
#[derive(Debug, thiserror::Error)]
#[error("byte array contains entries >9")]
pub struct FromBytesError(pub(crate) ());

/// Error for [`Sudoku::from_bytes_slice`]
#[derive(Debug, thiserror::Error)]
pub enum FromBytesSliceError {
    /// Slice is not 81 long
    #[error("byte slice should have length 81, found {0}")]
    WrongLength(usize),
    /// Slice contains invalid entries
    #[error(transparent)]
    FromBytesError(FromBytesError),
}

use crate::board::{block, col, row};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, thiserror::Error)]
/// An invalid sudoku entry encountered during parsing.
#[error("cell {cell} contains invalid character '{ch}'")]
pub struct InvalidEntry {
    /// Cell number goes from 0..=80, 0..=8 for first line, 9..=17 for 2nd and so on
    pub cell: u8,
    /// The parsed invalid char
    pub ch: char,
}

impl InvalidEntry {
    /// Row index from 0..=8, topmost row is 0
    #[inline]
    pub fn row(self) -> u8 {
        row(self.cell)
    }
    /// Column index from 0..=8, leftmost col is 0
    #[inline]
    pub fn col(self) -> u8 {
        col(self.cell)
    }
    /// Field index from 0..=8, numbering from left to right, top to bottom. Example: Top-row is 0, 1, 2
    #[inline]
    pub fn block(self) -> u8 {
        block(self.cell)
    }
}

////////////////////////////////////////////////////////////////////////////////

/// Error for lax block format parsing. Contains the number of rows found.
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
#[error("input ended after {0} valid rows")]
pub struct NotEnoughRows(pub u8);

/// A structure representing an error caused when parsing the sudoku
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
pub enum BlockParseError {
    /// Non-digit, non-placeholder encountered. Field delimiters chars in unexpected places also cause this
    #[error(transparent)]
    InvalidEntry(InvalidEntry),
    /// Line contains (>9 valid entries) or (<9 and no invalids)
    /// Returns index of row (0-8)
    #[error("a sudoku line should have 9 entries, found {0}")]
    InvalidLineLength(u8),
    /// Input ends with less than 9 rows. Returns number of rows encountered.
    #[error("a sudoku should have 9 rows, found only {0}")]
    NotEnoughRows(u8),
    /// If field delimiter is in place after 3rd number in 1st row
    /// all other horizontal and vertical field delimiters must be present or this is emitted
    #[error("inconsistent field delimiters")]
    IncorrectFieldDelimiter,
    /// More than 9 lines are supplied and the 10th line is not pure whitespace
    #[error("a sudoku should have 9 rows, found a 10th")]
    TooManyRows,
    /// Non-digit, non-placeholder after completed line encountered but without space
    #[error("missing comment delimiter in line {0}")]
    MissingCommentDelimiter(u8),
}

/// A structure representing an error caused when parsing the sudoku
#[derive(Clone, Debug, Eq, Hash, PartialEq, thiserror::Error)]
pub enum LineParseError {
    /// Accepted values are numbers 1...9 and '0', '.' or '_' for empty cells
    #[error(transparent)]
    InvalidEntry(InvalidEntry),
    /// Returns number of cells supplied
    #[error("sudoku contains {0} cells instead of required 81")]
    NotEnoughCells(u8),
    /// Returned if >=82 valid cell positions are supplied
    #[error("sudoku contains more than 81 cells or is missing comment delimiter")]
    TooManyCells,
    /// Comments must be delimited by a space or tab.
    #[error("missing comment delimiter")]
    MissingCommentDelimiter,
}
