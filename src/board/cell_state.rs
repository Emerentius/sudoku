use crate::board::Digit;
use crate::bitset::Set;

/// Contains either a digit or all the candidates for an unsolved cell
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[allow(missing_docs)]
pub enum CellState {
    Digit(Digit),
    Candidates(Set<Digit>),
}
