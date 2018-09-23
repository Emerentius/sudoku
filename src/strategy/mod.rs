//! This module contains the [`StrategySolver`] that mimics human approaches to sudoku solving
//! for hinting at possible moves and grading difficulty. The `StrategySolver` together with the [`Strategy`] enum
//! form the core of this module. All deductions are recorded and can be obtained through the appropriate methods.
//!
//! Strategies that humans employ to solve sudokus are typically much slower than the application of a few
//! simple strategies together with backtracking. While efforts are made to optimize it, expect the solver to
//! be at least an order of magnitude slower than the fast solver.

mod solver;
mod board;
pub use self::solver::{Strategy, StrategySolver, Deductions, Deduction, DeductionResult, DeductionsIter};
pub use self::board::{DigitMask, CellState};
