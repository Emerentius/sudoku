use crate::bitset::Set;
use crate::board::Digit;

/// Contains either a digit or all the candidates for an unsolved cell
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[allow(missing_docs)]
pub enum CellState {
    Digit(Digit),
    Candidates(Set<Digit>),
}
