//! Errors that may be encountered when reading a sudoku from a string
use crate::board::{row, col, block};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// An invalid sudoku entry encountered during parsing.
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

use std::fmt;

/// Error for lax block format parsing. Contains the number of rows found.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NotEnoughRows(pub u8);

/// A structure representing an error caused when parsing the sudoku
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BlockParseError {
    /// Non-digit, non-placeholder encountered. Field delimiters chars in unexpected places also cause this
    InvalidEntry(InvalidEntry),
    /// Line contains (>9 valid entries) or (<9 and no invalids)
    /// Returns index of row (0-8)
    InvalidLineLength(u8),
    /// Input ends with less than 9 rows. Returns number of rows encountered.
    NotEnoughRows(u8),
    /// If field delimiter is in place after 3rd number in 1st row
    /// all other horizontal and vertical field delimiters must be present or this is emitted
    IncorrectFieldDelimiter,
    /// More than 9 lines are supplied and the 10th line is not pure whitespace
    TooManyRows,
    /// Non-digit, non-placeholder after completed line encountered but without space
    MissingCommentDelimiter(u8),
}

/// A structure representing an error caused when parsing the sudoku
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum LineParseError {
    /// Accepted values are numbers 1...9 and '0', '.' or '_' for empty cells
    InvalidEntry(InvalidEntry),
    /// Returns number of cells supplied
    NotEnoughCells(u8),
    /// Returned if >=82 valid cell positions are supplied
    TooManyCells,
    /// Comments must be delimited by a space or tab.
    MissingCommentDelimiter,
}

impl fmt::Display for LineParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::LineParseError as Error;

        match *self {
            Error::InvalidEntry(InvalidEntry { cell, ch }) => {
                write!(f, "cell {} contains invalid character '{}'", cell, ch)
            }
            Error::NotEnoughCells(cells) => write!(f, "sudoku contains {} cells instead of required 81", cells),
            Error::TooManyCells => write!(
                f,
                "sudoku contains more than 81 cells or is missing comment delimiter"
            ),
            Error::MissingCommentDelimiter => write!(f, "missing comment delimiter"),
        }
    }
}
