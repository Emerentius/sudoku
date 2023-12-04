// A collection of internal helper types
// like arrays that can only be indexed by the right position structs

use crate::board::{Cell, Digit, House};
use crate::consts::N_CELLS;
use std::ops::{Deref, DerefMut, Index, IndexMut};

#[derive(Debug)]
pub(crate) struct Unsolvable;

#[derive(Copy, Clone, Debug)]
/// Container with one slot for each cell.
pub(crate) struct CellArray<T>(pub [T; N_CELLS]);

impl<T> Deref for CellArray<T> {
    type Target = [T; 81];
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for CellArray<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T> Index<Cell> for CellArray<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, idx: Cell) -> &Self::Output {
        &self.0[idx.as_index()]
    }
}

impl<T> IndexMut<Cell> for CellArray<T> {
    #[inline(always)]
    fn index_mut(&mut self, idx: Cell) -> &mut Self::Output {
        &mut self.0[idx.as_index()]
    }
}

///////////////////////////////
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// Container with one slot for each row, column and block
pub(crate) struct HouseArray<T>(pub [T; 27]);

impl<T, IDX: Into<House>> Index<IDX> for HouseArray<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, idx: IDX) -> &Self::Output {
        &self.0[idx.into().as_index()]
    }
}

impl<T, IDX: Into<House>> IndexMut<IDX> for HouseArray<T> {
    #[inline(always)]
    fn index_mut(&mut self, idx: IDX) -> &mut Self::Output {
        &mut self.0[idx.into().as_index()]
    }
}

///////////////////////////////
#[derive(Copy, Clone, Debug)]
/// Container with one slot for each digit.
pub(crate) struct DigitArray<T>(pub [T; 9]);

impl<T> Index<Digit> for DigitArray<T> {
    type Output = T;

    #[inline(always)]
    fn index(&self, idx: Digit) -> &Self::Output {
        &self.0[idx.as_index()]
    }
}

impl<T> IndexMut<Digit> for DigitArray<T> {
    #[inline(always)]
    fn index_mut(&mut self, idx: Digit) -> &mut Self::Output {
        &mut self.0[idx.as_index()]
    }
}
