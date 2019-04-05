//! Tools for solving sudokus with strategies that humans use.
//!
//! This module contains the [`StrategySolver`](::strategy::StrategySolver) that mimics human
//! approaches to sudoku solving for hinting at possible moves and grading difficulty.
//! The `StrategySolver` together with the [`Strategy`](::strategy::Strategy) enum form the
//! core of this module. All deductions are recorded and can be obtained through
//! the appropriate methods.
//!
//! The strategies contained here are typically much slower than the use of
//! just a few simple strategies together with backtracking. While efforts are
//! made to optimize it, expect the solver to be at least an order of magnitude
//! slower than the fast solver.

mod solver;
pub mod deduction;
mod strategies;
pub(crate) mod utils;

pub use self::solver::{StrategySolver};
pub use self::strategies::Strategy;
pub use self::deduction::Deduction;
