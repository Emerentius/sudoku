use crate::bitset::Set;
use crate::board::{Block, Cell, Col, Digit, Row};

/// Represents a digit in a specific cell
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Candidate {
    pub cell: Cell,
    pub digit: Digit,
}

impl Candidate {
    /// Constructs a new candidate.
    ///
    /// # Panics
    ///
    /// panics if `cell >= 81` or `!(1..=9).contains(num)`
    #[inline]
    pub fn new(cell: u8, digit: u8) -> Candidate {
        assert!(cell < 81);
        assert!(0 < digit && digit < 10);

        Candidate {
            cell: Cell::new(cell),
            digit: Digit::new(digit),
        }
    }

    /// Returns the row of this candidate's cell
    #[inline]
    pub fn row(self) -> Row {
        self.cell.row()
    }

    /// Returns the column of this candidate's cell
    #[inline]
    pub fn col(self) -> Col {
        self.cell.col()
    }

    /// Returns the block (also called box or field) of this candidate's cell
    #[inline]
    pub fn block(self) -> Block {
        self.cell.block()
    }

    #[inline]
    pub(crate) fn digit_set(self) -> Set<Digit> {
        self.digit.as_set()
    }
}
