use consts::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Entry {
	pub cell: usize,
	pub num: u8,
}

impl Entry {
	#[inline] pub fn cell(self) -> usize { self.cell }
	#[inline] pub fn row(self) -> u8 { self.cell as u8 / 9 }
	#[inline] pub fn col(self) -> u8 { self.cell as u8 % 9 }
	#[inline] pub fn field(self) -> u8 { self.row() / 3 * 3 + self.col() / 3 }
	#[inline] pub fn num(self) -> u8 { self.num }

	#[inline]
	pub fn conflicts_with(self, other: Self) -> bool {
		self.cell() == other.cell() ||
		(self.num == other.num &&
			(  self.row() == other.row()
			|| self.col() == other.col()
			|| self.field() == other.field()
			)
		)
	}

	#[inline] pub fn num_offset(self) -> usize { self.num() as usize - 1 }
	#[inline] pub fn row_constraint(self)   -> usize { self.row()   as usize * 9 + self.num_offset() }
	#[inline] pub fn col_constraint(self)   -> usize { self.col()   as usize * 9 + self.num_offset() + COL_OFFSET }
	#[inline] pub fn field_constraint(self) -> usize { self.field() as usize * 9 + self.num_offset() + FIELD_OFFSET }
	#[inline] pub fn cell_constraint(self)  -> usize { self.cell()                                   + CELL_OFFSET }

	#[inline] pub fn constrains(self, constraint_nr: usize) -> bool {
		constraint_nr == match constraint_nr {
			0...80    => self.row_constraint(),
			81...161  => self.col_constraint(),
			162...242 => self.field_constraint(),
			243...323 => self.cell_constraint(),
			_ => unreachable!(),
		}
	}
}
