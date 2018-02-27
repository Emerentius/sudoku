#![allow(unused)]
use super::StrategySolver;
use sudoku::Sudoku;
use types::{Array81, Mask, Digit, Position, Unsolvable, Entry};
use consts::*;
use positions::{
    row_zone, col_zone, field_zone, cells_of_zone,
    Cell, Line, Zone, Slice, Band,
};

pub trait NewStrategy: Sync {
    type Deduction: Deduce;
    type Deductions: Iterator<Item=Result<Self::Deduction, Unsolvable>>;

    fn deduce(
		&self,
		sudoku: &StrategySolver,
	)
	-> Self::Deductions;
}

pub trait Deduce {
    fn apply_deductions(
        &mut self,
        deduced_entries: &mut Vec<Entry>,
		impossible_entries: &mut Vec<Entry>
    ) -> Result<(), Unsolvable>;
}
