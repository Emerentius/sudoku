//////////////////////////////////
//	Collection of helper types  //
//////////////////////////////////

use consts::*;
use positions::FIELD;

#[derive(Debug)]
pub struct Unsolvable;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Entry {
	pub cell: u8,
	pub num: u8,
}

impl Entry {
	#[inline] pub fn cell(self) -> usize { self.cell as usize }
	#[inline] pub fn row(self) -> u8 { self.cell / 9 }
	#[inline] pub fn col(self) -> u8 { self.cell % 9 }
	#[inline] pub fn field(self) -> u8 { FIELD[self.cell()] }
	#[inline] pub fn num(self) -> u8 { self.num }

	#[inline] pub fn mask(self) -> Mask<Digit> { Mask::from_num(self.num()) }
}

////////////////////////////////////////////////////////////////////////////////

use std::fmt;

/// A structure representing an error caused when parsing the sudoku
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseError {
    /// Error caused when the lenght of the parsed line was not 9
    InvalidLineLength(u8),
    /// Error caused when a character is found which is not a number between 1
    /// and 9 or a '_'
    InvalidNumber(u8, char),
    /// Error caused when the input has a number of rows lower than 9
    NotEnoughRows
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::ParseError::*;

        try!(write!(f, "Error parsing sudoku: "));
        match *self {
            InvalidLineLength(line_nr) => write!(f, "line number {} should contain exactly 9 characters", line_nr),
            InvalidNumber(line_nr, chr) => write!(f, "line number {} contains the invalid character `{}`", line_nr, chr),
            NotEnoughRows => write!(f, "the input should contain exactly 9 rows")
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, PartialEq, Eq, Debug)] pub struct Position;
#[derive(Clone, Copy, PartialEq, Eq, Debug)] pub struct Digit;

// Bitmask struct for T where T is just an information regarding intent
// it could for example be Mask<Digit> or Mask<Position>
// All usual bitwise operations are implemented but do not work between
// Mask<T> and Mask<U>, when T != U
// NOTE: !mask is a leaky abstraction, see comment above trait implementation
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct Mask<T>(u16, ::std::marker::PhantomData<T>);

impl<T> Mask<T> {
    #[inline(always)]
    fn new(mask: u16) -> Self {
        Mask(mask, ::std::marker::PhantomData)
    }

	#[inline(always)]
    pub fn all() -> Self {
		Mask::new(0b_1_1111_1111)
	}

	#[inline(always)]
    pub fn none() -> Self {
		Mask::new(0)
	}

	#[inline(always)]
    pub fn n_possibilities(self) -> u8 {
		self.0.count_ones() as u8
	}
}

impl Mask<Digit> {
    #[inline(always)]
    pub fn from_num(num: u8) -> Self {
        debug_assert!(num > 0);
        debug_assert!(num <= 9);
        Mask::new(1 << (num - 1))
    }

    #[inline(always)]
    pub fn unique_num(self) -> Result<Option<u8>, Unsolvable> {
        match self.0 {
            0 => Err(Unsolvable),
            n if n.is_power_of_two() => Ok(Some(n.trailing_zeros() as u8+1)),
            _ => Ok(None),
        }
    }

    // return a possible digit
    // highest in this implementation, but that is not necessary
    #[inline(always)]
    pub fn one_possibility(self) -> u8 {
        debug_assert!(self.0 != 0);
        16 - self.0.leading_zeros() as u8
        //self.0.trailing_zeros() as u8 + 1
    }
}

// NOTE: !Mask (bitwise negation) is a leaky abstraction
//       The implementation for Not should really be !self.0 & 0b_1_1111_1111
//       but because the most common operation is
//       some_mask &= !mask, where some_mask doesn't have the high bits set
//       this is a pointless operation in the most common case and costs performance
//       a | !b == Mask::all() will always be false as a result when
//       a and b are regular masks without the high bits set
impl<T> ::std::ops::Not for Mask<T> {
	type Output = Self;
    #[inline(always)]
	fn not(self) -> Self {
		Mask::new(!self.0)
	}
}

macro_rules! impl_bitops {
    ($trait_:path, $fn_name:ident ) => {
        impl<T> $trait_ for Mask<T> {
        	type Output = Self;
            #[inline(always)]
        	fn $fn_name(self, rhs: Self) -> Self {
        		Mask::new(u16::$fn_name(self.0, rhs.0))
        	}
        }
    }
}

impl_bitops!(::std::ops::BitAnd, bitand);
impl_bitops!(::std::ops::BitOr, bitor);
impl_bitops!(::std::ops::BitXor, bitxor);

macro_rules! impl_bitops_assign {
    ($trait_:path, $fn_name:ident ) => {
        impl<T> $trait_ for Mask<T> {
        	fn $fn_name(&mut self, rhs: Self) {
        		u16::$fn_name(&mut self.0, rhs.0)
        	}
        }
    }
}

impl_bitops_assign!(::std::ops::BitAndAssign, bitand_assign);
impl_bitops_assign!(::std::ops::BitOrAssign, bitor_assign);
impl_bitops_assign!(::std::ops::BitXorAssign, bitxor_assign);

////////////////////////////////////////////////////////////////////////////////

// Pure boilerplate. Just an array of size 81.
#[derive(Copy)]
pub struct Array81<T>(pub [T; N_CELLS]);


impl<T: Copy> Clone for Array81<T> {
    fn clone(&self) -> Self { *self }
}

impl<T: fmt::Debug> fmt::Debug for Array81<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        (&self.0[..]).fmt(f)
    }
}

impl<T> ::std::ops::Deref for Array81<T> {
    type Target = [T; 81];
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> ::std::ops::DerefMut for Array81<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
