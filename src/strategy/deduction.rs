//! Results of strategy applications

use super::Strategy;
use crate::bitset::Set;
use crate::board::Candidate;
use crate::board::*;

type DeductionRange = std::ops::Range<usize>;
type _Deduction = Deduction<DeductionRange>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
/// Contains the sequence of deductions made to solve / partially solve the sudoku
pub struct Deductions {
    pub(crate) deductions: Vec<_Deduction>,
    pub(crate) deduced_entries: Vec<Candidate>,
    pub(crate) eliminated_entries: Vec<Candidate>,
}

/// Borrowing iterator over [`Deductions`]
pub struct Iter<'a> {
    deductions: std::slice::Iter<'a, _Deduction>,
    eliminated_entries: &'a [Candidate],
}

impl<'a> Iterator for Iter<'a> {
    type Item = Deduction<&'a [Candidate]>;

    fn next(&mut self) -> Option<Self::Item> {
        self.deductions
            .next()
            .map(|deduction| deduction.clone().with_slices(self.eliminated_entries))
    }
}

impl Deductions {
    /// Returns the number of deductions.
    pub fn len(&self) -> usize {
        self.deductions.len()
    }

    /// Return the `index`th Deduction, if it exists.
    pub fn get(&self, index: usize) -> Option<Deduction<&[Candidate]>> {
        self.deductions
            .get(index)
            .map(|deduction| deduction.clone().with_slices(&self.eliminated_entries))
    }

    /// Return an iterator over the deductions.
    pub fn iter(&self) -> Iter<'_> {
        Iter {
            deductions: self.deductions.iter(),
            eliminated_entries: &self.eliminated_entries,
        }
    }
}

/// Result of a single, successful strategy application
///
/// This enum contains the data necessary to explain why the step could be taken.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub enum Deduction<T> {
    /// Result of [`NakedSingles`](super::Strategy::NakedSingles)
    NakedSingles(Candidate),
    /// Result of [`HiddenSingles`](super::Strategy::HiddenSingles)
    HiddenSingles(Candidate, HouseType),
    /// Result of [`LockedCandidates`](super::Strategy::LockedCandidates)
    LockedCandidates {
        digit: Digit,
        /// The miniline which is the only one in the block or line, that contains `digit`
        miniline: MiniLine,
        /// In the "Pointing" variant, only one miniline in a block can contain the digit and all candidates
        /// in other blocks in the same line are impossible. In the "Claiming" variant, it's the other way around.
        is_pointing: bool,
        conflicts: T,
    }, // which miniline is affected and what's unique

    /// Result of naked or hidden subsets, i.e. [`NakedPairs`](super::Strategy::NakedPairs), [`NakedTriples`](super::Strategy::NakedTriples), [`NakedQuads`](super::Strategy::NakedQuads),
    /// [`HiddenPairs`](super::Strategy::HiddenPairs), [`HiddenTriples`](super::Strategy::HiddenTriples) or [`HiddenQuads`](super::Strategy::HiddenQuads).
    Subsets {
        /// A house that contains all cells of the locked set.
        house: House,
        /// The cells that contain the locked set. Can be 2-4 positions.
        positions: Set<Position<House>>,
        /// The digits that are part of the locked set. The number of digits is always equal to the number of
        /// positions
        digits: Set<Digit>,
        conflicts: T,
    },
    /// Result of [`XWing`](super::Strategy::XWing), [`Swordfish`](super::Strategy::Swordfish) or [`Jellyfish`](super::Strategy::Jellyfish)
    BasicFish {
        digit: Digit,
        /// The lines that contain the fish. Can be 2-4 lines.
        lines: Set<Line>,
        /// The union of possible positions in the `lines`. The number of positions is always equal to the number
        /// of lines.
        positions: Set<Position<Line>>,
        conflicts: T,
    },
    Fish {
        digit: Digit,
        base: Set<House>,
        cover: Set<House>,
        conflicts: T,
    },
    /// Result of [`XyWing`](super::Strategy::XyWing), [`XyzWing`](super::Strategy::XyzWing)
    Wing {
        hinge: Cell,
        // TODO: having just an identifier of Xy-, Xyz-, etc-wing is ugly
        //       but so is just having the hinge_digits as a set and not the pincer digits
        //       Find a way to get rid of that
        hinge_digits: Set<Digit>,
        pincers: Set<Cell>,
        conflicts: T,
    },
    AvoidableRectangle {
        /// The 2 rows and 2 columns forming the avoidable rectangle. The cells where they overlap always occupy 2 blocks in one chute.
        lines: Set<Line>,
        conflicts: T,
    },
    //SinglesChain(T),
    #[doc(hidden)]
    __NonExhaustive,
}

impl Deduction<&'_ [Candidate]> {
    /// Returns the type of strategy that was used to make this deduction.
    pub fn strategy(&self) -> Strategy {
        use self::Deduction::*;
        match *self {
            NakedSingles { .. } => Strategy::NakedSingles,
            HiddenSingles { .. } => Strategy::HiddenSingles,
            LockedCandidates { .. } => Strategy::LockedCandidates,
            BasicFish { positions, .. } => match positions.len() {
                2 => Strategy::XWing,
                3 => Strategy::Swordfish,
                4 => Strategy::Jellyfish,
                _ => unreachable!(),
            },
            //SinglesChain { .. } => Strategy::SinglesChain,
            Subsets {
                house,
                positions,
                conflicts,
                ..
            } => {
                use crate::board::positions::HouseType::*;
                let conflict_cell = conflicts[0].cell;
                let conflict_pos = match house.categorize() {
                    Row(_) => conflict_cell.row_pos(),
                    Col(_) => conflict_cell.col_pos(),
                    Block(_) => conflict_cell.block_pos(),
                };
                let is_hidden_subset = conflict_pos.as_set().overlaps(positions);
                match (is_hidden_subset, positions.len()) {
                    (false, 2) => Strategy::NakedPairs,
                    (false, 3) => Strategy::NakedTriples,
                    (false, 4) => Strategy::NakedQuads,
                    (true, 2) => Strategy::HiddenPairs,
                    (true, 3) => Strategy::HiddenTriples,
                    (true, 4) => Strategy::HiddenQuads,
                    _ => unreachable!(),
                }
            }
            /*HiddenSubsets { digits, .. } => {
                match digits.len() {
                    2 => Strategy::HiddenPairs,
                    3 => Strategy::HiddenTriples,
                    4 => Strategy::HiddenQuads,
                    _ => unreachable!(),
                }
            }*/
            Fish { base, cover, .. } => {
                use crate::strategy::strategies::mutant_fish::is_mutant;
                let is_mutant = is_mutant(base) || is_mutant(cover);
                match (is_mutant, base.len()) {
                    (false, 2) => Strategy::XWing,
                    (false, 3) => Strategy::Swordfish,
                    (false, 4) => Strategy::Jellyfish,
                    (true, 3) => Strategy::MutantSwordfish,
                    (true, 4) => Strategy::MutantJellyfish,
                    _ => unreachable!(),
                }
            }
            Wing { hinge_digits, .. } => match hinge_digits.len() {
                2 => Strategy::XyWing,
                3 => Strategy::XyzWing,
                _ => unreachable!(),
            },
            AvoidableRectangle { .. } => unimplemented!(),
            __NonExhaustive => unreachable!(),
        }
    }
}

#[rustfmt::skip]
impl _Deduction {
    /// Replace the index ranges from the internal representation with slices
    /// for the external API
    fn with_slices(self, eliminated: &[Candidate]) -> Deduction<&[Candidate]> {
        use self::Deduction::*;
        match self {
            NakedSingles(c) => NakedSingles(c),
            HiddenSingles(c, h) => HiddenSingles(c, h),

            LockedCandidates {
                miniline, digit, is_pointing,
                conflicts
            } => LockedCandidates { miniline, digit, is_pointing, conflicts: &eliminated[conflicts] },

            Subsets {
                house, positions, digits,
                conflicts
            }
            => Subsets { house, positions, digits, conflicts: &eliminated[conflicts]},

            BasicFish {
                lines, positions, digit,
                conflicts
            }
            => BasicFish { lines, positions, digit, conflicts: &eliminated[conflicts]},

            Fish {
                digit, base, cover,
                conflicts,
            }
            => Fish { digit, base, cover, conflicts: &eliminated[conflicts] },

            Wing {
                hinge, hinge_digits, pincers,
                conflicts
            }
            => Wing { hinge, hinge_digits, pincers, conflicts: &eliminated[conflicts] },

            AvoidableRectangle { .. } => unimplemented!(),
            //SinglesChain(x) => SinglesChain(&eliminated[x]),
            __NonExhaustive => __NonExhaustive
        }
    }
}
