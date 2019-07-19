#![warn(missing_docs)]
#![allow(
    clippy::cognitive_complexity,
    clippy::precedence,
    clippy::match_bool,
    clippy::unreadable_literal,
    clippy::wrong_self_convention,
    clippy::inconsistent_digit_grouping,
    clippy::too_many_arguments
)]
//! Utilities for classical 9x9 sudokus.
//!
//! This library currently offers extremely fast sudoku solving and a basic sudoku
//! generator. The solver is based on [jczsolve](http://forum.enjoysudoku.com/3-77us-solver-2-8g-cpu-testcase-17sodoku-t30470-210.html#p249309)
//! which is currently and to the best knowledge of the author the world's fastest sudoku
//! solver algorithm. A few modifications were made to improve the speed further.
//!
//! A future goal is the addition of a fast solver applying human style strategies
//! so that sudokus can be graded, hinted and the solution path explained. With the ability to
//! grade sudokus, puzzles of any desired desired difficulty can be generated.
//!
//! ## Example
//!
//! ```
//! use sudoku::Sudoku;
//!
//! // Sudokus can be created from &str's in both block or line formats or directly from bytes.
//! // here, an example in line format
//! let sudoku_line = "...2...633....54.1..1..398........9....538....3........263..5..5.37....847...1...";
//!
//! let sudoku = Sudoku::from_str_line(sudoku_line).unwrap();
//!
//! // Solve, print or convert the sudoku to another format
//! if let Some(solution) = sudoku.solve_unique() {
//!     // print the solution in line format
//!     println!("{}", solution);
//!
//!     // or return it as a byte array
//!     let cell_contents: [u8; 81] = solution.to_bytes();
//! }
//! ```

pub mod bitset;
pub mod board;
mod consts;
mod generator;
mod helper;
pub mod parse_errors;
mod solver;
pub mod strategy;

pub use crate::board::Sudoku;

use std::ffi::CStr;
use std::os::raw::c_char;

/// Export a "C" function for calling the solver from the Tdoku benchmark program
#[no_mangle]
pub extern fn rust_solve_sudoku(input: *const c_char, limit: usize) -> usize {
    let line = unsafe { CStr::from_ptr(input) };
    let sudoku = Sudoku::from_str_line(&line.to_str().unwrap()).unwrap();
    sudoku.count_at_most(limit)
}
