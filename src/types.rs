//////////////////////////////////
//	Collection of helper types  //
//////////////////////////////////

use consts::*;
use positions::FIELD;
use positions2::{Set, Digit as Digit, Cell, House, Row, Col, Block};
use ::std::ops::{Deref, DerefMut, Index, IndexMut};

#[derive(Debug)]
pub struct Unsolvable;

/// Represents a digit in a specific cell
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Entry {
    pub(crate) cell: u8,
    pub(crate) num: u8,
}

impl Entry {
    /// Constructs a new entry.
    ///
    /// # Panics
    ///
    /// panics if `cell >= 81` or `!(1..=9).contains(num)`
    #[inline]
    pub fn new(cell: u8, num: u8) -> Entry {
        assert!(cell < 81);
        assert!(0 < num && num < 10);

        Entry { cell, num }
    }

    /// Returns the cell this entry belongs to
    #[inline]
    pub fn cell(self) -> Cell {
        Cell::new(self.cell)
    }

    /// Returns the row of this entry's cell
    #[inline]
    pub fn row(self) -> Row {
        self.cell().row()
    }

    /// Returns the columns of this entry's cell
    #[inline]
    pub fn col(self) -> Col {
        self.cell().col()
    }

    /// Returns the field (box) of this entry's cell
    #[inline]
    pub fn field(self) -> Block {
        self.cell().block()
    }

    /// Returns this entry's digit
    #[inline]
    pub fn digit(self) -> Digit {
        Digit::new(self.num)
    }

    #[inline]
    pub(crate) fn digit_set(self) -> Set<Digit> {
        Set::from(Digit::new(self.num))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
// Type surfaced in API for parsing errors as 'Entry'
/// An invalid entry that was parsed
pub struct PubEntry {
    /// Cell number goes from 0..=80, 0..=8 for first line, 9..=17 for 2nd and so on
    pub cell: u8,
    /// The parsed invalid char
    pub ch: char,
}

impl PubEntry {
    /// Row index from 0..=8, topmost row is 0
    #[inline]
    pub fn row(self) -> u8 {
        self.cell / 9
    }
    /// Column index from 0..=8, leftmost col is 0
    #[inline]
    pub fn col(self) -> u8 {
        self.cell % 9
    }
    /// Field index from 0..=8, numbering from left to right, top to bottom. Example: Top-row is 0, 1, 2
    #[inline]
    pub fn field(self) -> u8 {
        FIELD[self.cell as usize]
    }
}

////////////////////////////////////////////////////////////////////////////////

use std::fmt;

/// Error for lax block format parsing. Contains the number of rows found.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NotEnoughRows(pub u8);

/// A structure representing an error caused when parsing the sudoku
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum BlockFormatParseError {
    /// Non-digit, non-placeholder encountered. Field delimiters chars in unexpected places also cause this
    InvalidEntry(PubEntry),
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
pub enum LineFormatParseError {
    /// Accepted values are numbers 1...9 and '0', '.' or '_' for empty cells
    InvalidEntry(PubEntry),
    /// Returns number of cells supplied
    NotEnoughCells(u8),
    /// Returned if >=82 valid cell positions are supplied
    TooManyCells,
    /// Comments must be delimited by a space or tab.
    MissingCommentDelimiter,
}

impl fmt::Display for LineFormatParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::LineFormatParseError::*;

        match *self {
            InvalidEntry(PubEntry { cell, ch }) => {
                write!(f, "cell {} contains invalid character '{}'", cell, ch)
            }
            NotEnoughCells(cells) => write!(f, "sudoku contains {} cells instead of required 81", cells),
            TooManyCells => write!(
                f,
                "sudoku contains more than 81 cells or is missing comment delimiter"
            ),
            MissingCommentDelimiter => write!(f, "missing comment delimiter"),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

// Pure boilerplate. Just an array of size 81.
#[derive(Copy, Clone)]
pub(crate) struct CellArray<T>(pub [T; N_CELLS]);

impl<T: fmt::Debug> fmt::Debug for CellArray<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        (&self.0[..]).fmt(f)
    }
}

impl<T> Deref for CellArray<T> {
    type Target = [T; 81];
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for CellArray<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Index<Cell> for CellArray<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, idx: Cell) -> &Self::Output {
        &self.0[idx.as_index()]
    }
}

impl<T> IndexMut<Cell> for CellArray<T> {
    #[inline(always)]
    fn index_mut(&mut self, idx: Cell) -> &mut Self::Output {
        &mut self.0[idx.as_index()]
    }
}

///////////////////////////////
#[derive(Copy, Clone, Debug)]
pub(crate) struct HouseArray<T>(pub [T; 27]);

impl<T, IDX: Into<House>> Index<IDX> for HouseArray<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, idx: IDX) -> &Self::Output {
        &self.0[idx.into().as_index()]
    }
}

impl<T, IDX: Into<House>> IndexMut<IDX> for HouseArray<T> {
    #[inline(always)]
    fn index_mut(&mut self, idx: IDX) -> &mut Self::Output {
        &mut self.0[idx.into().as_index()]
    }
}

///////////////////////////////
#[derive(Copy, Clone, Debug)]
pub(crate) struct DigitArray<T>(pub [T; 9]);

impl<T> Index<Digit> for DigitArray<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, idx: Digit) -> &Self::Output {
        &self.0[idx.as_index()]
    }
}

impl<T> IndexMut<Digit> for DigitArray<T> {
    #[inline(always)]
    fn index_mut(&mut self, idx: Digit) -> &mut Self::Output {
        &mut self.0[idx.as_index()]
    }
}
