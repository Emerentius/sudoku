#![warn(missing_docs)]

//! The Sudoku library
//!
//! ## Overview
//!
//! Sudoku is a library that aims to provide a simple API to solve sudokus
//! without having to deal with too much details.
//!
//! ## Example
//!
//! ```
//! use sudoku::Sudoku;
//!
//! let sudoku_block =
//! "___|2__|_63
//! 3__|__5|4_1
//! __1|__3|98_
//! ---+---+---
//! ___|___|_9_
//! ___|538|___
//! _3_|___|___
//! ---+---+---
//! _26|3__|5__
//! 5_3|7__|__8
//! 47_|__1|___";
//!
//! // this parser ignores everything except '.', '_', '0' and '1'-'9' as well as non-full lines
//! // will be able to deal with most well-formed block formats
//! let sudoku = Sudoku::from_str_block_permissive(sudoku_block).unwrap();
//!
//! // this parser enforces consistency and accepts only a few formats
//! let sudoku = Sudoku::from_str_block(sudoku_block).unwrap();
//! match sudoku.solve_unique() {
//!     Some(solution) => println!("{}", solution),
//!     None           => println!("No unique solution."),
//! }
//! ```
extern crate core;

mod consts;
mod types;
mod sudoku;
mod positions;

pub use sudoku::Sudoku;
//pub use types::{LineFormatParseError, BlockFormatParseError, PubEntry as Entry};

/// Contains errors for the various parsing modes
pub mod parse_errors {
    pub use types::{LineFormatParseError, BlockFormatParseError, NotEnoughRows, PubEntry as Entry};
}