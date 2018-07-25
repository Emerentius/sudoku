#![warn(missing_docs)]
#![cfg_attr(feature = "cargo-clippy",
    allow(
        clippy_complexity,
        match_bool,
        unreadable_literal,
        needless_pass_by_value,
        wrong_self_convention,
        write_with_newline,
    )
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
#[macro_use]
extern crate crunchy;
#[cfg(feature="serde")] extern crate serde;
extern crate rand;

mod consts;
mod types;
mod sudoku;
mod solver;
mod generator;
mod positions;

pub use sudoku::Sudoku;

/// Contains errors for the various parsing modes
pub mod parse_errors {
    pub use types::{LineFormatParseError, BlockFormatParseError, NotEnoughRows, PubEntry as Entry};
}
