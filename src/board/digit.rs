use std::num::NonZeroU8;

// define digit separately because it has an offset
/// A digit that can be entered in a cell of a sudoku.
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug, Hash)]
pub struct Digit(NonZeroU8);

impl Digit {
    /// Constructs a new `Digit`.
    ///
    /// # Panic
    /// Panics, if the digit is not in the range of `1..=9`.
    pub fn new(digit: u8) -> Self {
        Self::new_checked(digit).unwrap()
    }

    /// Constructs a new `Digit`. Returns `None`, if the digit is not in the range of `1..=9`.
    pub fn new_checked(digit: u8) -> Option<Self> {
        if digit > 9 {
            return None;
        }
        NonZeroU8::new(digit).map(Digit)
    }

    /// Constructs a new `Digit` from an index, i.e. `digit - 1`.
    ///
    /// # Panic
    /// Panics, if the digit is not in the range of `0..=8`.
    pub(crate) fn from_index(idx: u8) -> Self {
        Self::new_checked(idx+1).unwrap()
    }

    /// Returns an iterator over all digits.
    pub fn all() -> impl Iterator<Item = Self> {
        (1..10).map(Digit::new)
    }

    /// Returns the digit contained within.
    pub fn get(self) -> u8 {
        self.0.get()
    }

    /// Returns the number contained within as `usize`, offset by `-1`. Guarantees that the numbering starts from `0`.
    pub fn as_index(self) -> usize {
        self.get() as usize - 1
    }
}
