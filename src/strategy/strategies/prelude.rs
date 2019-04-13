pub(crate) use crate::bitset::{Iter as SetIter, Set};
pub(crate) use crate::helper::{CellArray, DigitArray, HouseArray, Unsolvable};
pub(crate) use crate::Sudoku;
#[rustfmt::skip]
pub(crate) use crate::board::{
    Digit,
    Candidate,
    positions::{
        Cell,
        Row,
        Col,
        Line,
        Chute,
        MiniLine,
        Position,
        House,
    },
};

// this is used, but rustc doesn't detect that
// https://github.com/rust-lang/rust/issues/45268
#[allow(unused_imports)]
pub(crate) use crate::board::positions::CellAt;
