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
//! ```no_run
//! use sudoku::Sudoku;
//!
//! // Pipes are ignored, you can also omit them
//! let sudoku_str = "\
//! ___|2__|_63
//! 3__|__5|4_1
//! __1|__3|98_
//! ___|___|_9_
//! ___|538|___
//! _3_|___|___
//! _26|3__|5__
//! 5_3|7__|__8
//! 47_|__1|___";
//!
//! let mut sudoku = Sudoku::from_str(sudoku_str).unwrap();
//! sudoku.solve();
//! println!("{}", sudoku);
//! ```

mod consts;
mod covers;
mod entry;
mod parse_error;
mod sudoku;
mod positions;

pub use sudoku::Sudoku;
pub use parse_error::ParseError;
