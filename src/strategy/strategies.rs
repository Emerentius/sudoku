pub(crate) mod prelude;

pub(crate) mod almost_locked_sets;
pub(crate) mod avoidable_rectangles;
pub(crate) mod basic_fish;
pub(crate) mod hidden_singles;
pub(crate) mod hidden_subsets;
pub(crate) mod locked_candidates;
pub(crate) mod mutant_fish;
pub(crate) mod naked_singles;
pub(crate) mod naked_subsets;
pub(crate) mod xy_wing;
pub(crate) mod xyz_wing;

use super::StrategySolver;
use crate::helper::Unsolvable;

/// The strategies that can be used to find hints, solve or grade a sudoku.
///
/// This can be used with [`StrategySolver::solve`].
/// May be expanded in the future.
#[non_exhaustive]
#[derive(Debug, Clone)]
#[allow(missing_docs)]
pub enum Strategy {
    NakedSingles,
    HiddenSingles,
    LockedCandidates,
    NakedPairs,
    NakedTriples,
    NakedQuads,
    HiddenPairs,
    HiddenTriples,
    HiddenQuads,
    XWing,
    Swordfish,
    Jellyfish,
    XyWing,
    XyzWing,
    MutantSwordfish,
    MutantJellyfish,
    AvoidableRectangles,
    //SinglesChain,
}

impl Strategy {
    /// Set of all available strategies, for test purposes
    #[allow(unused)]
    #[rustfmt::skip]
    pub(crate) const ALL: &'static [Strategy] = &[
                                    // difficulty as assigned by
                                    // SudokuExplainer
        Strategy::NakedSingles,     // 23
        Strategy::HiddenSingles,    // 15
        Strategy::LockedCandidates, // 28
        Strategy::NakedPairs,       // 30
        Strategy::XWing,            // 32
        Strategy::HiddenPairs,      // 34
        Strategy::NakedTriples,     // 36
        Strategy::Swordfish,        // 38
        Strategy::HiddenTriples,    // 40
        Strategy::XyWing,           // 42
        Strategy::XyzWing,          // 44
        Strategy::NakedQuads,       // 50
        Strategy::Jellyfish,        // 52
        Strategy::HiddenQuads,      // 54
        //Strategy::SinglesChain,
    ];

    // is_first_strategy is an optimization hint
    // it doesn't need to be used
    pub(crate) fn deduce(
        &self,
        state: &mut StrategySolver,
        stop_after_first: bool,
        is_first_strategy: bool,
    ) -> Result<(), Unsolvable> {
        use self::Strategy::*;
        match *self {
            NakedSingles if !stop_after_first && is_first_strategy => state.find_all_naked_singles(),
            NakedSingles => state.find_naked_singles(stop_after_first),
            HiddenSingles => state.find_hidden_singles(stop_after_first),
            LockedCandidates => state.find_locked_candidates(stop_after_first),
            NakedPairs => state.find_naked_subsets(2, stop_after_first),
            NakedTriples => state.find_naked_subsets(3, stop_after_first),
            NakedQuads => state.find_naked_subsets(4, stop_after_first),
            HiddenPairs => state.find_hidden_subsets(2, stop_after_first),
            HiddenTriples => state.find_hidden_subsets(3, stop_after_first),
            HiddenQuads => state.find_hidden_subsets(4, stop_after_first),
            XWing => state.find_xwings(stop_after_first),
            Swordfish => state.find_swordfish(stop_after_first),
            Jellyfish => state.find_jellyfish(stop_after_first),
            XyWing => state.find_xy_wing(stop_after_first),
            XyzWing => state.find_xyz_wing(stop_after_first),
            MutantSwordfish => state.find_mutant_fish(3, stop_after_first),
            MutantJellyfish => state.find_mutant_fish(4, stop_after_first),
            //SinglesChain => state.find_singles_chain(stop_after_first), // TODO: Implement non-eager SinglesChain
            _ => unimplemented!(),
        }
    }

    pub(crate) fn deduce_one(&self, state: &mut StrategySolver) -> Result<(), Unsolvable> {
        self.deduce(state, true, false)
    }

    pub(crate) fn deduce_all(
        &self,
        state: &mut StrategySolver,
        is_first_strategy: bool,
    ) -> Result<(), Unsolvable> {
        self.deduce(state, false, is_first_strategy)
    }
}
