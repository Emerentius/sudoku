use entry::Entry;

/// Contains all the information to represent the sparse exact cover matrix
#[derive(Clone, Debug)]
pub struct Covers {
	/// List of entries that could still be entered
	pub entries: Vec<Entry>,
	/// Count of how many entries are still able to satisfy a certain constraint
    pub possibilities_count: Array324<u8>,
	/// Storage for whether a constraint has been met already
	///
	/// A possibility count of 0 would otherwise be ambiguous
	/// (it could mean 'impossible' or 'already set')
	/// Note: could be a BitVec, improvements are miniscule but measurable
	pub covered: Array324<bool>,
}

impl Covers {
	pub fn new() -> Covers {
		Covers {
			entries: (0..9*9*9)
				.map(|i| Entry { cell: (i / 9) as u8, num: (i % 9) as u8 + 1 } )
				.collect::<Vec<Entry>>(),
			// idx = row/col/field/cell * 9 + (num - 1) + offset, offset = 81 for col, 162 for field, 243 for cells
			possibilities_count: Array324([9; 324]),
			covered: Array324([false; 324]),
		}
	}
}

#[derive(Copy)]
pub struct Array324<T>([T; 324]);

use std::ops;
impl<T> ops::Deref for Array324<T> {
	type Target = [T; 324];
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl<T: Copy> Clone for Array324<T> {
    fn clone(&self) -> Self { *self }
}

impl<T: ::std::fmt::Debug> ::std::fmt::Debug for Array324<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error> {
        (&self.0[..]).fmt(f)
    }
}
impl<T> ops::DerefMut for Array324<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}
