use board::{Candidate};
use super::Strategy;
use board::*;
use bitset::Set;

type DeductionRange = ::std::ops::Range<usize>;
type _Deduction = Deduction<DeductionRange>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
/// Contains the sequence of deductions made to solve / partially solve the sudoku
pub struct Deductions {
	pub(crate) deductions: Vec<_Deduction>,
	pub(crate) deduced_entries: Vec<Candidate>,
	pub(crate) eliminated_entries: Vec<Candidate>,
}

pub struct Iter<'a> {
	deductions: ::std::slice::Iter<'a, _Deduction>,
	eliminated_entries: &'a [Candidate]
}

impl<'a> Iterator for Iter<'a> {
	type Item = Deduction<&'a [Candidate]>;

	fn next(&mut self) -> Option<Self::Item> {
		self.deductions.next()
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
		self.deductions.get(index)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Deduction<T> {
	Given(Candidate), // by user
    NakedSingles(Candidate),
    HiddenSingles(Candidate, HouseType),
    LockedCandidates(MiniLine, Set<Digit>, T), // which miniline is affected and what's unique
    NakedSubsets {
		house: House,
		positions: Set<Position<House>>,     // 2-4 positions
		digits: Set<Digit>,                  // digits restricted to cells
		conflicts: T,          				 // link to impossible entries
	},
    HiddenSubsets {
		house: House,
		digits: Set<Digit>,                  // 2-4 digits
		positions: Set<Position<House>>,     // positions restricted to digits
		conflicts: T,           			 // link to impossible entries
	},
    BasicFish {
		lines: Set<Line>, 					 // 2-4 lines
		positions: Set<Position<Line>>,		 // which positions in all lines
		digit: Digit,
		conflicts: T,
	},

	// TODO: expand information in variants below
    SinglesChain(T),
    #[doc(hidden)] __NonExhaustive
}

impl<T> Deduction<T> {
	pub fn strategy(&self) -> Strategy {
		use self::Deduction::*;
		match self {
			Given(_) => unimplemented!(),
			NakedSingles { .. } => Strategy::NakedSingles,
			HiddenSingles { .. } => Strategy::HiddenSingles,
			LockedCandidates { .. } => Strategy::LockedCandidates,
			BasicFish { positions, .. } => {
				match positions.len() {
					2 => Strategy::XWing,
					3 => Strategy::Swordfish,
					4 => Strategy::Jellyfish,
					_ => unreachable!(),
				}
			}
			SinglesChain { .. } => Strategy::SinglesChain,
			NakedSubsets { positions, .. } => {
				match positions.len() {
					2 => Strategy::NakedPairs,
					3 => Strategy::NakedTriples,
					4 => Strategy::NakedQuads,
					_ => unreachable!(),
				}
			}
			HiddenSubsets { digits, .. } => {
				match digits.len() {
					2 => Strategy::HiddenPairs,
					3 => Strategy::HiddenTriples,
					4 => Strategy::HiddenQuads,
					_ => unreachable!(),
				}
			}
			__NonExhaustive => unreachable!(),
		}
	}
}

impl _Deduction {
	fn with_slices(self, eliminated: &[Candidate]) -> Deduction<&[Candidate]> {
		use self::Deduction::*;
		match self {
			Given(c) => Given(c),
			NakedSingles(c) => NakedSingles(c),
			HiddenSingles(c, h) => HiddenSingles(c, h),
			LockedCandidates(ml, s, conflicts) => LockedCandidates(ml, s, &eliminated[conflicts]),

			NakedSubsets {
				house, positions, digits,
				conflicts
			}
			=> NakedSubsets { house, positions, digits, conflicts: &eliminated[conflicts]},

			HiddenSubsets {
				house, digits, positions, conflicts,
			}
			=> HiddenSubsets { house, digits, positions, conflicts: &eliminated[conflicts] },

			BasicFish {
				lines, positions, digit,
				conflicts
			}
			=> BasicFish { lines, positions, digit, conflicts: &eliminated[conflicts]},

			SinglesChain(x) => SinglesChain(&eliminated[x]),
			__NonExhaustive => __NonExhaustive
		}
	}
}
