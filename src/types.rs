//////////////////////////////////
//	Collection of helper types  //
//////////////////////////////////

use consts::*;
use positions::FIELD;
use positions2::{Set, Digit as PDigit, Cell};

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
    pub fn cell(self) -> usize {
        self.cell as usize
    }

    /// Returns the row of this entry's cell
    #[inline]
    pub fn row(self) -> u8 {
        self.cell / 9
    }

    /// Returns the columns of this entry's cell
    #[inline]
    pub fn col(self) -> u8 {
        self.cell % 9
    }

    /// Returns the field (box) of this entry's cell
    #[inline]
    pub fn field(self) -> u8 {
        FIELD[self.cell()]
    }

    /// Returns this entry's digit
    #[inline]
    pub fn num(self) -> u8 {
        self.num
    }

    #[inline]
    pub(crate) fn mask(self) -> Mask<Digit> {
        Mask::from_num(self.num())
    }

    #[inline]
    pub(crate) fn cell_type(self) -> Cell {
        Cell::new(self.cell)
    }

    #[inline]
    pub(crate) fn digit_set(self) -> Set<PDigit> {
        Set::from(PDigit::new(self.num))
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Position(pub u8);
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Digit;

impl Position {
    pub fn in_row_of_cell(cell: u8) -> Self {
        Position(col(cell))
    }

    pub fn in_col_of_cell(cell: u8) -> Position {
        Position(row(cell))
    }

    pub fn in_field_of_cell(cell: u8) -> Position {
        let row_in_field = row(cell) % 3;
        let col_in_field = col(cell) % 3;
        Position(row_in_field*3 + col_in_field)
    }
}

// Bitmask struct for T where T is just an information regarding intent
// it could for example be Mask<Digit> or Mask<Position>
// All usual bitwise operations are implemented but do not work between
// Mask<T> and Mask<U>, when T != U
// NOTE: !mask is a leaky abstraction, see comment above trait implementation
#[derive(Clone, Copy, Eq, Debug)]
pub(crate) struct Mask<T>(pub(crate) u16, ::std::marker::PhantomData<T>);
#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct MaskIter<T>(Mask<T>);

impl<T> Mask<T> {
    pub const ALL: Mask<T> = Mask(0b_0001_1111_1111, ::std::marker::PhantomData);
    pub const NONE: Mask<T> = Mask(0, ::std::marker::PhantomData);

    #[inline(always)]
    fn new(mask: u16) -> Self {
        Mask(mask, ::std::marker::PhantomData)
    }

    #[inline(always)]
    pub fn n_possibilities(self) -> u8 {
        self.0.count_ones() as u8
    }

    #[inline(always)]
    pub fn is_empty(self) -> bool {
        self == Mask::NONE
    }

    #[inline(always)]
    pub fn without(self, other: Self) -> Self {
        Mask::new(self.0 & !other.0)
    }

    #[inline(always)]
    pub fn remove(&mut self, other: Self) {
        self.0 &= !other.0;
    }

    #[inline(always)]
    pub fn iter(self) -> MaskIter<T> {
        MaskIter(self)
    }
}

impl<T> PartialEq<Mask<T>> for Mask<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Mask<Digit> {
    #[inline(always)]
    pub fn from_num(num: u8) -> Self {
        debug_assert!(num > 0);
        debug_assert!(num <= 9);
        Mask::new(1 << (num - 1))
    }

    #[inline(always)]
    pub fn unique_num(self) -> Result<Option<u8>, Unsolvable> {
        match self.0 {
            0 => Err(Unsolvable),
            n if n.is_power_of_two() => Ok(Some(n.trailing_zeros() as u8 + 1)),
            _ => Ok(None),
        }
    }

    // return a possible digit
    // highest in this implementation, but that is not necessary
    #[inline(always)]
    pub fn one_possibility(self) -> u8 {
        debug_assert!(self.0 != 0);
        16 - self.0.leading_zeros() as u8
        //self.0.trailing_zeros() as u8 + 1
    }
}

use positions::{row, col};
impl Mask<Position> {
    #[inline(always)]
    pub fn from_pos(pos: u8) -> Self {
        debug_assert!(pos <= 8);
        Mask::new(1 << pos)
    }

    #[inline(always)]
    #[allow(unused)]
    pub fn unique_pos(self) -> Result<Option<Position>, Unsolvable> {
        match self.0 {
            0 => Err(Unsolvable),
            n if n.is_power_of_two() => Ok(Some(Position(n.trailing_zeros() as u8))),
            _ => Ok(None),
        }
    }

    pub fn row_pos_of_cell(cell: u8) -> Mask<Position> {
        Mask::from_pos(col(cell))
    }

    pub fn col_pos_of_cell(cell: u8) -> Mask<Position> {
        Mask::from_pos(row(cell))
    }

    pub fn field_pos_of_cell(cell: u8) -> Mask<Position> {
        let row_in_field = row(cell) % 3;
        let col_in_field = col(cell) % 3;
        Mask::from_pos(row_in_field*3 + col_in_field)
    }
    /*
    pub fn from_cell_by_zone(cell: Cell, zone: Zone) -> Self {
        const ROW_OFF_PL8: usize = ROW_OFFSET + 8;
        const COL_OFF_PL8: usize = COL_OFFSET + 8;
        const FIELD_OFF_PL8: usize = FIELD_OFFSET + 8;
        match Zone.0 as usize {
            ROW_OFFSET...ROW_OFF_PL8 => ,
            COL_OFFSET...COL_OFF_PL8 => ,
            FIELD_OFFSET...FIELD_OFF_PL8 => {

            },
        };
        unimplemented!()
        }
    */

    // return a possible pos
    // highest in this implementation, but that is not necessary
    #[inline(always)]
    pub fn one_possibility(self) -> Position {
        debug_assert!(self.0 != 0);
        Position(15 - self.0.leading_zeros() as u8)
        //self.0.trailing_zeros() as u8 + 1
    }
}

impl<T> MaskIter<T> {
    /// Return remaining possibilities in mask form
    #[allow(unused)]
    pub fn to_mask(self) -> Mask<T> {
        self.0
    }
}

impl Iterator for MaskIter<Digit> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        match (self.0).0 {
            0 => None,
            _ => {
                let num = self.0.one_possibility();
                self.0.remove(Mask::from_num(num));
                Some(num)
            },
        }
    }
}

impl Iterator for MaskIter<Position> {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        match (self.0).0 {
            0 => None,
            _ => {
                let pos = self.0.one_possibility();
                self.0.remove(Mask::from_pos(pos.0));
                Some(pos)
            },
        }
    }
}

// NOTE: a common operation is
//       some_mask &= !mask, where some_mask doesn't have the high bits set
//       use .without() (owned) or .remove() (referenced) to avoid masking the high bits off
//       for the negation. They won't be set anyway in `some_mask`.
impl<T> ::std::ops::Not for Mask<T> {
    type Output = Self;
    #[inline(always)]
    fn not(self) -> Self {
        Mask::new(!self.0 & 0b_0001_1111_1111)
    }
}

macro_rules! impl_bitops {
    ($trait_:path, $fn_name:ident) => {
        impl<T> $trait_ for Mask<T> {
            type Output = Self;
            #[inline(always)]
            fn $fn_name(self, rhs: Self) -> Self {
                Mask::new(u16::$fn_name(self.0, rhs.0))
            }
        }
    };
}

impl_bitops!(::std::ops::BitAnd, bitand);
impl_bitops!(::std::ops::BitOr, bitor);
impl_bitops!(::std::ops::BitXor, bitxor);

macro_rules! impl_bitops_assign {
    ($trait_:path, $fn_name:ident) => {
        impl<T> $trait_ for Mask<T> {
            fn $fn_name(&mut self, rhs: Self) {
                u16::$fn_name(&mut self.0, rhs.0)
            }
        }
    };
}

impl_bitops_assign!(::std::ops::BitAndAssign, bitand_assign);
impl_bitops_assign!(::std::ops::BitOrAssign, bitor_assign);
impl_bitops_assign!(::std::ops::BitXorAssign, bitxor_assign);

////////////////////////////////////////////////////////////////////////////////

// Pure boilerplate. Just an array of size 81.
#[derive(Copy, Clone)]
pub(crate) struct Array81<T>(pub [T; N_CELLS]);

impl<T: fmt::Debug> fmt::Debug for Array81<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        (&self.0[..]).fmt(f)
    }
}

impl<T> ::std::ops::Deref for Array81<T> {
    type Target = [T; 81];
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> ::std::ops::DerefMut for Array81<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
