//! Generic, fixed-size bitsets
//!
//! Sudoku strategies deal with sets of various things such as [`Digit`s](::board::Digit) and [`House`s](::board::House) a lot.
//! Efficient storage is important for maximal performance, but it should not be possible
//! to confuse bitmasks for different things. This module contains type-safe, space-efficient
//! fixed-length bitsets for digits and various sudoku positions.

use crate::board::{Cell, Col, Digit, House, Line, Position, Row};
use crate::helper::Unsolvable;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

/// Generic, fixed-size bitset
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Set<T: SetElement>(pub(crate) T::Storage);

/// Iterator over the elements contained in a [`Set`]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Iter<T: SetElement>(T::Storage);

impl<T: SetElement> IntoIterator for Set<T>
where
    Iter<T>: Iterator,
{
    type Item = <Iter<T> as Iterator>::Item;
    type IntoIter = Iter<T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter(self.0)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
//                                  Bitops
///////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! impl_binary_bitops {
    ( $( $trait:ident, $fn_name:ident);* $(;)* ) => {
        $(
            impl<T: SetElement> $trait for Set<T> {
                type Output = Self;

                #[inline(always)]
                fn $fn_name(self, other: Self) -> Self {
                    Set(
                        $trait::$fn_name(self.0, other.0)
                    )
                }
            }

            impl<T: SetElement> $trait<T> for Set<T> {
                type Output = Self;

                #[inline(always)]
                fn $fn_name(self, other: T) -> Self {
                    $trait::$fn_name(self, other.as_set())
                }
            }
        )*
    };
}

macro_rules! impl_bitops_assign {
    ( $( $trait:ident, $fn_name:ident);* $(;)* ) => {
        $(
            impl<T: SetElement> $trait for Set<T> {
                #[inline(always)]
                fn $fn_name(&mut self, other: Self) {
                    $trait::$fn_name(&mut self.0, other.0)
                }
            }

            impl<T: SetElement> $trait<T> for Set<T> {
                #[inline(always)]
                fn $fn_name(&mut self, other: T) {
                    $trait::$fn_name(self, other.as_set())
                }
            }
        )*
    };
}

impl_binary_bitops!(
    BitAnd, bitand;
    BitOr, bitor;
    BitXor, bitxor;
);

impl_bitops_assign!(
    BitAndAssign, bitand_assign;
    BitOrAssign, bitor_assign;
    BitXorAssign, bitxor_assign;
);

impl<T: SetElement> Not for Set<T>
where
    Self: PartialEq + Copy,
{
    type Output = Self;
    fn not(self) -> Self {
        Self::ALL.without(self)
    }
}

/// Potential return value for [`Set::unique`]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Empty;

impl From<Empty> for Unsolvable {
    fn from(_: Empty) -> Unsolvable {
        Unsolvable
    }
}

impl<T: SetElement> Set<T>
where
    // TODO: properly implement the traits for Set and Iter
    //       bounded on T::Storage, not on T (which derive does)
    Self: PartialEq + Copy,
{
    /// Set containing all possible elements
    pub const ALL: Set<T> = Set(<T as SetElement>::ALL);

    /// Empty Set
    pub const NONE: Set<T> = Set(<T as SetElement>::NONE);

    /// Construct a bitset from a raw integer.
    ///
    /// # Panic
    /// Panics, if the integer contains bits above [`Set::ALL`]
    pub fn from_bits(mask: T::Storage) -> Self {
        assert!(mask <= <T as SetElement>::ALL);
        Set(mask)
    }

    /// Return the raw integer backing the set.
    pub fn bits(self) -> T::Storage {
        self.0
    }

    /// Returns the set of elements in this set, that aren't present in `other`.
    pub fn without(self, other: Self) -> Self {
        Set(self.0 & !other.0)
    }

    /// Deletes all elements from this set that are present in `other`.
    pub fn remove(&mut self, other: Self) {
        self.0 &= !other.0;
    }

    /// Checks if `self` and `other` contain any common element.
    pub fn overlaps(&self, other: Self) -> bool {
        *self & other != Set::NONE
    }

    /// Checks if `self` contains `other`.
    pub fn contains(&self, other: impl Into<Self>) -> bool {
        let other = other.into();
        *self & other == other
    }

    /// Returns the number of elements in this set.
    pub fn len(&self) -> u8 {
        T::count_possibilities(self.0) as u8
    }

    /// Checks whether this set contains any element.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Checks whether this set contains all possible elements.
    pub fn is_full(&self) -> bool {
        *self == Self::ALL
    }

    /// Returns the only element in this set, iff only 1 element exists.
    /// If no elements exist, it returns `Err(Empty)`.
    /// If more than 1 element exists, it returns `Ok(None)`.
    pub fn unique(self) -> Result<Option<T>, Empty>
    where
        Iter<T>: Iterator<Item = T>,
    {
        match self.len() {
            1 => {
                let element = self.into_iter().next();
                debug_assert!(element.is_some());
                Ok(element)
            }
            0 => Err(Empty),
            _ => Ok(None),
        }
    }

    /// Returns one of the elements in this set.
    /// It is equivalent to `self.into_iter().next().unwrap()`
    ///
    /// # Panic
    /// Panics, if the set is empty
    #[allow(unused)]
    pub(crate) fn one_possibility(self) -> T
    where
        Iter<T>: Iterator<Item = T>,
    {
        self.into_iter().next().expect("mask is empty")
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////

/// Trait for types that can be stored in a [`Set`]
#[allow(missing_docs)]
pub trait SetElement: Sized + set_element::Sealed {
    const ALL: Self::Storage;
    const NONE: Self::Storage;

    type Storage: BitAnd<Output = Self::Storage>
        + BitAndAssign
        + BitOr<Output = Self::Storage>
        + BitOrAssign
        + BitXor<Output = Self::Storage>
        + BitXorAssign
        + Not<Output = Self::Storage>
        + PartialOrd
        + std::fmt::Binary
        + Copy;

    fn count_possibilities(set: Self::Storage) -> u32;
    fn as_set(self) -> Set<Self>;
}
mod set_element {
    use super::*;
    pub trait Sealed {}

    macro_rules! impl_sealed {
        ($($type:ty),*) => {
            $(
                impl Sealed for $type {}
            )*
        };
    }

    impl_sealed! {
        Cell, Digit, Row, Col, House, Line, Position<Line>, Position<House>
    }
}

macro_rules! impl_setelement {
    ( $( $type:ty => $storage_ty:ty, $all:expr),* $(,)* ) => {
        $(
            impl SetElement for $type {
                const ALL: $storage_ty = $all;
                const NONE: $storage_ty = 0;

                type Storage = $storage_ty;

                fn count_possibilities(set: Self::Storage) -> u32 {
                    set.count_ones()
                }

                fn as_set(self) -> Set<Self> {
                    Set(1 << self.as_index() as u8)
                }
            }

            impl $type {
                /// Returns a `Set<Self>` with the bit corresponding to this element set.
                pub fn as_set(self) -> Set<Self> {
                    SetElement::as_set(self)
                }
            }
        )*
    };
}

impl_setelement!(
    // 81 cells
    Cell => u128, 0o777_777_777___777_777_777___777_777_777,
    // 9 digits
    Digit => u16, 0o777,

    // 9 of each house
    //Row => u16, 0o777,
    //Col => u16, 0o777,
    //Block => u16, 0o777,
    Line => u32, 0o777_777,      // both Rows and Cols
    House => u32, 0o777_777_777, // Rows, Cols, Blocks

    // 9 positions per house
    //Position<Row> => u16, 0o777,
    //Position<Col> => u16, 0o777,
    Position<Line> => u16, 0o777,
    Position<House> => u16, 0o777,
    // 27 positions per chute
    //Position<Band> => u32, 0o777_777_777,
    //Position<Stack> => u32, 0o777_777_777,
    //Position<Chute> => u32, 0o777_777_777,
);

macro_rules! impl_iter_for_setiter {
    ( $( $type:ty => $constructor:expr ),* $(,)* ) => {
        $(
            impl Iterator for Iter<$type> {
                type Item = $type;

                fn next(&mut self) -> Option<Self::Item> {
                    debug_assert!(self.0 <= <Set<$type>>::ALL.0, "{:o}", self.0);
                    if self.0 == 0 {
                        return None;
                    }
                    let lowest_bit = self.0 & (!self.0 + 1);
                    let bit_pos = lowest_bit.trailing_zeros() as u8;
                    self.0 ^= lowest_bit;
                    Some($constructor(bit_pos))
                }
            }
        )*
    };
}

// can't do this generically
impl_iter_for_setiter!(
    Cell => Cell::new,
    Digit => Digit::from_index,
    Line => Line::new,
    House => House::new,
    //Position<Row> => Position::new,
    //Position<Col> => Position::new,
    Position<Line> => Position::new,
    Position<House> => Position::new,
    //Position<Band> => Position::new,
    //Position<Stack> => Position::new,
    //Position<Chute> => Position::new,
);

use std::fmt;
impl<T: SetElement> fmt::Binary for Set<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:b}", self.0)
    }
}
