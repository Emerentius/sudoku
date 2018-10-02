/// Bitmask for the possible digits in a cell
/// least significant bit = 1, most significant bit = 9
//#[derive(Copy, Clone)]
//pub struct DigitMask(pub u16);

use positions2::{Set, Digit};

/// Contains either a digit or all the candidates for an unsolved cell
#[derive(Copy, Clone)]
#[allow(missing_docs)]
pub enum CellState {
    Number(u8),
    Candidates(Set<Digit>),
}
