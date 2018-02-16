#![allow(unused)]
use consts::*;

#[inline(always)] pub fn row(cell: u8) -> u8 { cell / 9 }
#[inline(always)] pub fn col(cell: u8) -> u8 { cell % 9 }
#[inline(always)] pub fn field(cell: u8) -> u8 { FIELD[cell as usize] }

#[inline(always)] pub fn row_zone(cell: u8) -> usize   { row(cell) as usize + ROW_OFFSET }
#[inline(always)] pub fn col_zone(cell: u8) -> usize   { col(cell) as usize + COL_OFFSET }
#[inline(always)] pub fn field_zone(cell: u8) -> usize { field(cell) as usize + FIELD_OFFSET }

#[inline(always)]
pub fn cells_of_zone(zone: u8) -> &'static [u8; 9] {
	&CELLS_BY_ZONE[zone as usize]
}

#[inline(always)]
pub fn neighbours(cell: u8) -> &'static [u8; 20] {
	&ZONE_NEIGHBOURS_OF_CELL[cell as usize]
}

pub static FIELD: [u8; N_CELLS] = [
	0, 0, 0, 1, 1, 1, 2, 2, 2,
	0, 0, 0, 1, 1, 1, 2, 2, 2,
	0, 0, 0, 1, 1, 1, 2, 2, 2,
	3, 3, 3, 4, 4, 4, 5, 5, 5,
	3, 3, 3, 4, 4, 4, 5, 5, 5,
	3, 3, 3, 4, 4, 4, 5, 5, 5,
	6, 6, 6, 7, 7, 7, 8, 8, 8,
	6, 6, 6, 7, 7, 7, 8, 8, 8,
	6, 6, 6, 7, 7, 7, 8, 8, 8,
];

static CELLS_BY_ZONE: [[u8; 9]; 27] = [
	[0, 1, 2, 3, 4, 5, 6, 7, 8],
	[9, 10, 11, 12, 13, 14, 15, 16, 17],
	[18, 19, 20, 21, 22, 23, 24, 25, 26],
	[27, 28, 29, 30, 31, 32, 33, 34, 35],
	[36, 37, 38, 39, 40, 41, 42, 43, 44],
	[45, 46, 47, 48, 49, 50, 51, 52, 53],
	[54, 55, 56, 57, 58, 59, 60, 61, 62],
	[63, 64, 65, 66, 67, 68, 69, 70, 71],
	[72, 73, 74, 75, 76, 77, 78, 79, 80],

	[0, 9, 18, 27, 36, 45, 54, 63, 72],
	[1, 10, 19, 28, 37, 46, 55, 64, 73],
	[2, 11, 20, 29, 38, 47, 56, 65, 74],
	[3, 12, 21, 30, 39, 48, 57, 66, 75],
	[4, 13, 22, 31, 40, 49, 58, 67, 76],
	[5, 14, 23, 32, 41, 50, 59, 68, 77],
	[6, 15, 24, 33, 42, 51, 60, 69, 78],
	[7, 16, 25, 34, 43, 52, 61, 70, 79],
	[8, 17, 26, 35, 44, 53, 62, 71, 80],

	[0, 1, 2, 9, 10, 11, 18, 19, 20],
	[3, 4, 5, 12, 13, 14, 21, 22, 23],
	[6, 7, 8, 15, 16, 17, 24, 25, 26],
	[27, 28, 29, 36, 37, 38, 45, 46, 47],
	[30, 31, 32, 39, 40, 41, 48, 49, 50],
	[33, 34, 35, 42, 43, 44, 51, 52, 53],
	[54, 55, 56, 63, 64, 65, 72, 73, 74],
	[57, 58, 59, 66, 67, 68, 75, 76, 77],
	[60, 61, 62, 69, 70, 71, 78, 79, 80],
];

// list of cells that share a row, col or field for a given cell
// sorted low to high
static ZONE_NEIGHBOURS_OF_CELL: [[u8; 20]; 81] = [
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 18, 19, 20, 27, 36, 45, 54, 63, 72],
        [0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 18, 19, 20, 28, 37, 46, 55, 64, 73],
        [0, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 18, 19, 20, 29, 38, 47, 56, 65, 74],
        [0, 1, 2, 4, 5, 6, 7, 8, 12, 13, 14, 21, 22, 23, 30, 39, 48, 57, 66, 75],
        [0, 1, 2, 3, 5, 6, 7, 8, 12, 13, 14, 21, 22, 23, 31, 40, 49, 58, 67, 76],
        [0, 1, 2, 3, 4, 6, 7, 8, 12, 13, 14, 21, 22, 23, 32, 41, 50, 59, 68, 77],
        [0, 1, 2, 3, 4, 5, 7, 8, 15, 16, 17, 24, 25, 26, 33, 42, 51, 60, 69, 78],
        [0, 1, 2, 3, 4, 5, 6, 8, 15, 16, 17, 24, 25, 26, 34, 43, 52, 61, 70, 79],
        [0, 1, 2, 3, 4, 5, 6, 7, 15, 16, 17, 24, 25, 26, 35, 44, 53, 62, 71, 80],
        [0, 1, 2, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 27, 36, 45, 54, 63, 72],
        [0, 1, 2, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 28, 37, 46, 55, 64, 73],
        [0, 1, 2, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 29, 38, 47, 56, 65, 74],
        [3, 4, 5, 9, 10, 11, 13, 14, 15, 16, 17, 21, 22, 23, 30, 39, 48, 57, 66, 75],
        [3, 4, 5, 9, 10, 11, 12, 14, 15, 16, 17, 21, 22, 23, 31, 40, 49, 58, 67, 76],
        [3, 4, 5, 9, 10, 11, 12, 13, 15, 16, 17, 21, 22, 23, 32, 41, 50, 59, 68, 77],
        [6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 24, 25, 26, 33, 42, 51, 60, 69, 78],
        [6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 24, 25, 26, 34, 43, 52, 61, 70, 79],
        [6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 24, 25, 26, 35, 44, 53, 62, 71, 80],
        [0, 1, 2, 9, 10, 11, 19, 20, 21, 22, 23, 24, 25, 26, 27, 36, 45, 54, 63, 72],
        [0, 1, 2, 9, 10, 11, 18, 20, 21, 22, 23, 24, 25, 26, 28, 37, 46, 55, 64, 73],
        [0, 1, 2, 9, 10, 11, 18, 19, 21, 22, 23, 24, 25, 26, 29, 38, 47, 56, 65, 74],
        [3, 4, 5, 12, 13, 14, 18, 19, 20, 22, 23, 24, 25, 26, 30, 39, 48, 57, 66, 75],
        [3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 23, 24, 25, 26, 31, 40, 49, 58, 67, 76],
        [3, 4, 5, 12, 13, 14, 18, 19, 20, 21, 22, 24, 25, 26, 32, 41, 50, 59, 68, 77],
        [6, 7, 8, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 26, 33, 42, 51, 60, 69, 78],
        [6, 7, 8, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 34, 43, 52, 61, 70, 79],
        [6, 7, 8, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 35, 44, 53, 62, 71, 80],
        [0, 9, 18, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 45, 46, 47, 54, 63, 72],
        [1, 10, 19, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 45, 46, 47, 55, 64, 73],
        [2, 11, 20, 27, 28, 30, 31, 32, 33, 34, 35, 36, 37, 38, 45, 46, 47, 56, 65, 74],
        [3, 12, 21, 27, 28, 29, 31, 32, 33, 34, 35, 39, 40, 41, 48, 49, 50, 57, 66, 75],
        [4, 13, 22, 27, 28, 29, 30, 32, 33, 34, 35, 39, 40, 41, 48, 49, 50, 58, 67, 76],
        [5, 14, 23, 27, 28, 29, 30, 31, 33, 34, 35, 39, 40, 41, 48, 49, 50, 59, 68, 77],
        [6, 15, 24, 27, 28, 29, 30, 31, 32, 34, 35, 42, 43, 44, 51, 52, 53, 60, 69, 78],
        [7, 16, 25, 27, 28, 29, 30, 31, 32, 33, 35, 42, 43, 44, 51, 52, 53, 61, 70, 79],
        [8, 17, 26, 27, 28, 29, 30, 31, 32, 33, 34, 42, 43, 44, 51, 52, 53, 62, 71, 80],
        [0, 9, 18, 27, 28, 29, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 54, 63, 72],
        [1, 10, 19, 27, 28, 29, 36, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 55, 64, 73],
        [2, 11, 20, 27, 28, 29, 36, 37, 39, 40, 41, 42, 43, 44, 45, 46, 47, 56, 65, 74],
        [3, 12, 21, 30, 31, 32, 36, 37, 38, 40, 41, 42, 43, 44, 48, 49, 50, 57, 66, 75],
        [4, 13, 22, 30, 31, 32, 36, 37, 38, 39, 41, 42, 43, 44, 48, 49, 50, 58, 67, 76],
        [5, 14, 23, 30, 31, 32, 36, 37, 38, 39, 40, 42, 43, 44, 48, 49, 50, 59, 68, 77],
        [6, 15, 24, 33, 34, 35, 36, 37, 38, 39, 40, 41, 43, 44, 51, 52, 53, 60, 69, 78],
        [7, 16, 25, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 51, 52, 53, 61, 70, 79],
        [8, 17, 26, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 51, 52, 53, 62, 71, 80],
        [0, 9, 18, 27, 28, 29, 36, 37, 38, 46, 47, 48, 49, 50, 51, 52, 53, 54, 63, 72],
        [1, 10, 19, 27, 28, 29, 36, 37, 38, 45, 47, 48, 49, 50, 51, 52, 53, 55, 64, 73],
        [2, 11, 20, 27, 28, 29, 36, 37, 38, 45, 46, 48, 49, 50, 51, 52, 53, 56, 65, 74],
        [3, 12, 21, 30, 31, 32, 39, 40, 41, 45, 46, 47, 49, 50, 51, 52, 53, 57, 66, 75],
        [4, 13, 22, 30, 31, 32, 39, 40, 41, 45, 46, 47, 48, 50, 51, 52, 53, 58, 67, 76],
        [5, 14, 23, 30, 31, 32, 39, 40, 41, 45, 46, 47, 48, 49, 51, 52, 53, 59, 68, 77],
        [6, 15, 24, 33, 34, 35, 42, 43, 44, 45, 46, 47, 48, 49, 50, 52, 53, 60, 69, 78],
        [7, 16, 25, 33, 34, 35, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 53, 61, 70, 79],
        [8, 17, 26, 33, 34, 35, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 62, 71, 80],
        [0, 9, 18, 27, 36, 45, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 72, 73, 74],
        [1, 10, 19, 28, 37, 46, 54, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 72, 73, 74],
        [2, 11, 20, 29, 38, 47, 54, 55, 57, 58, 59, 60, 61, 62, 63, 64, 65, 72, 73, 74],
        [3, 12, 21, 30, 39, 48, 54, 55, 56, 58, 59, 60, 61, 62, 66, 67, 68, 75, 76, 77],
        [4, 13, 22, 31, 40, 49, 54, 55, 56, 57, 59, 60, 61, 62, 66, 67, 68, 75, 76, 77],
        [5, 14, 23, 32, 41, 50, 54, 55, 56, 57, 58, 60, 61, 62, 66, 67, 68, 75, 76, 77],
        [6, 15, 24, 33, 42, 51, 54, 55, 56, 57, 58, 59, 61, 62, 69, 70, 71, 78, 79, 80],
        [7, 16, 25, 34, 43, 52, 54, 55, 56, 57, 58, 59, 60, 62, 69, 70, 71, 78, 79, 80],
        [8, 17, 26, 35, 44, 53, 54, 55, 56, 57, 58, 59, 60, 61, 69, 70, 71, 78, 79, 80],
        [0, 9, 18, 27, 36, 45, 54, 55, 56, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74],
        [1, 10, 19, 28, 37, 46, 54, 55, 56, 63, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74],
        [2, 11, 20, 29, 38, 47, 54, 55, 56, 63, 64, 66, 67, 68, 69, 70, 71, 72, 73, 74],
        [3, 12, 21, 30, 39, 48, 57, 58, 59, 63, 64, 65, 67, 68, 69, 70, 71, 75, 76, 77],
        [4, 13, 22, 31, 40, 49, 57, 58, 59, 63, 64, 65, 66, 68, 69, 70, 71, 75, 76, 77],
        [5, 14, 23, 32, 41, 50, 57, 58, 59, 63, 64, 65, 66, 67, 69, 70, 71, 75, 76, 77],
        [6, 15, 24, 33, 42, 51, 60, 61, 62, 63, 64, 65, 66, 67, 68, 70, 71, 78, 79, 80],
        [7, 16, 25, 34, 43, 52, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 71, 78, 79, 80],
        [8, 17, 26, 35, 44, 53, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 78, 79, 80],
        [0, 9, 18, 27, 36, 45, 54, 55, 56, 63, 64, 65, 73, 74, 75, 76, 77, 78, 79, 80],
        [1, 10, 19, 28, 37, 46, 54, 55, 56, 63, 64, 65, 72, 74, 75, 76, 77, 78, 79, 80],
        [2, 11, 20, 29, 38, 47, 54, 55, 56, 63, 64, 65, 72, 73, 75, 76, 77, 78, 79, 80],
        [3, 12, 21, 30, 39, 48, 57, 58, 59, 66, 67, 68, 72, 73, 74, 76, 77, 78, 79, 80],
        [4, 13, 22, 31, 40, 49, 57, 58, 59, 66, 67, 68, 72, 73, 74, 75, 77, 78, 79, 80],
        [5, 14, 23, 32, 41, 50, 57, 58, 59, 66, 67, 68, 72, 73, 74, 75, 76, 78, 79, 80],
        [6, 15, 24, 33, 42, 51, 60, 61, 62, 69, 70, 71, 72, 73, 74, 75, 76, 77, 79, 80],
        [7, 16, 25, 34, 43, 52, 60, 61, 62, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 80],
        [8, 17, 26, 35, 44, 53, 60, 61, 62, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79],
];

#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Cell(pub u8);
//#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Row(pub u8);
//#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Col(pub u8);
#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Field(pub u8);
#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Line(pub u8); // row or col
#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Zone(pub u8); // row, col or field

//#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Intersection<T, U>(pub T, pub U);

//type RowSlice = Intersection<Row, Field>;
//type ColSlice = Intersection<Col, Field>;
#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Slice(pub u8); // equivalent to Intersection<Line, Field>;

//#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct RowBand(pub u8);
//#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct ColBand(pub u8);
#[derive(Copy, Clone, Eq, PartialEq, Debug)] pub(crate) struct Band(pub u8); // RowBand or ColBand

use types::Position;
impl Cell {
        pub fn new(num: u8) -> Cell { Cell(num) }

        #[inline(always)] pub fn row(self) -> Line { Line(self.0 / 9) }
        #[inline(always)] pub fn col(self) -> Line { Line(self.0 % 9 + COL_OFFSET as u8) }
        #[inline(always)] pub fn field(self) -> Field { Field(FIELD[self.0 as usize]) }

        pub fn from_zone_pos(Zone(zone): Zone, Position(pos): Position) -> Self {
                const ROW_OFF_PL8: usize = ROW_OFFSET + 8;
                const COL_OFF_PL8: usize = COL_OFFSET + 8;
                const FIELD_OFF_PL8: usize = FIELD_OFFSET + 8;
                match zone as usize {
                        ROW_OFFSET...ROW_OFF_PL8 => Cell(zone*9 + pos),
                        COL_OFFSET...COL_OFF_PL8 => Cell(zone-9 + pos*9),
                        FIELD_OFFSET...FIELD_OFF_PL8 => {
                                let row_band = (zone - 18) / 3;
                                let base_cell = row_band * 27 + (zone % 3) * 3;

                                let row_off = pos / 3;
                                let col_off = pos % 3;
                                debug_assert!(base_cell + row_off*9 + col_off < 81);
                                Cell(base_cell + row_off*9 + col_off)
                        },
                        _ => unreachable!(),
                }
        }

        pub fn zones(self) -> [Zone; 3] {
                [self.row().zone(), self.col().zone(), self.field().zone() ]
        }
}

impl Field {
        pub fn new(num: u8) -> Field { Field(num) }

        pub fn rows(self) -> [Line; 3] {
                // in self's row band
                let min_field = self.0 / 3 * 3;
                [Line(min_field), Line(min_field+1), Line(min_field+2)]
        }
        
        pub fn cols(self) -> [Line; 3] {
                // in self's col band
                let min_field = self.0 % 3;
                [Line(min_field), Line(min_field+3), Line(min_field+6)]
        }

        pub fn bands(self) -> (Band, Band) {
                // (row, col)
                (Band(self.0 / 3), Band(self.0 % 3 + 3))
        }

        pub fn zone(self) -> Zone {
                Zone(self.0 + FIELD_OFFSET as u8)
        }
        
        // returns row slices for a row band, col slices for a col band
        pub fn slices(self, band: Band) -> [Slice; 3] {
                match band.is_row_band() {
                        true => self.row_slices(),
                        false => {
                                self.col_slices()
                        },
                }
        }

        pub fn row_slices(self) -> [Slice; 3] {
                // row band, col band
                let rb = self.0 / 3;
                let cb = self.0 % 3;

                // base slice
                let b = rb*9 + cb;
                [Slice(b), Slice(b+3), Slice(b+6)]
        }

        pub fn col_slices(self) -> [Slice; 3] {
                // row band, col band
                let rb = self.0 / 3;
                let cb = self.0 % 3;

                // base slice
                let b = cb*9 + rb;
                [Slice(b), Slice(b+3), Slice(b+6)]
        }
}

impl Line {
        pub const ALL_ROWS: [Line; 9] = [Line(0), Line(1), Line(2), Line(3), Line(4), Line(5), Line(6), Line(7), Line(8),];
        pub const ALL_COLS: [Line; 9] = [Line(9), Line(10), Line(11), Line(12), Line(13), Line(14), Line(15), Line(16), Line(17),];

        pub fn new(num: u8) -> Line { Line(num) }

        pub fn band(self) -> Band {
                Band(self.0 / 3)
        }

        pub fn fields(self) -> [Field; 3] {
                self.band().fields()
        }

        pub fn zone(self) -> Zone {
                Zone(self.0)
        }

        pub fn cells(self) -> [Cell; 9] {
                self.zone().cells()
        }

        pub fn slices(self) -> [Slice; 3] {
                let base = self.0 * 3;
                [Slice(base), Slice(base+1), Slice(base+2)]
        }
}

impl Band {
        pub fn new(num: u8) -> Band { Band(num) }

        pub fn lines(self) -> [Line; 3] {
                let min_line = self.0 * 3;
                [Line(min_line), Line(min_line+1), Line(min_line+2)]
        }

        pub fn fields(self) -> [Field; 3] {
                if self.is_row_band() {
                        let min_field = self.0 * 3;
                        [Field(min_field), Field(min_field+1), Field(min_field+2)]
                } else { // col band
                        let min_field = self.0 - 3;
                        [Field(min_field), Field(min_field+3), Field(min_field+6)]
                }
        }

        pub fn slices(self) -> [Slice; 9] {
                let mut slices = [Slice(0); 9];
                for (i, slice) in (0..9).zip(slices.iter_mut()) {
                        *slice = Slice(self.0 * 9 + i);
                }
                slices
        }

        pub fn is_row_band(self) -> bool {
                self.0 < 3
        }
}

impl Zone {
        pub fn cells(self) -> [Cell; 9] {
                let mut cells = [Cell(0); 9];
                for (cell, cell_) in cells.iter_mut().zip(CELLS_BY_ZONE[self.0 as usize].iter()) {
                        *cell = Cell(*cell_);
                }
                cells
        }
}

impl Slice {
        pub fn neighbours(self) -> ([Slice; 2], [Slice; 2]) {
                // line neighbour, field neighbour
                let (ln, bn) = SLICE_NEIGHBOURS[self.0 as usize];
                (
                        [ Slice(ln[0]), Slice(ln[1]) ],
                        [ Slice(bn[0]), Slice(bn[1]) ],
                )
        }

        pub fn cells(self) -> [Cell; 3] {
                let c = CELLS_IN_SLICE[self.0 as usize];
                [Cell(c[0]), Cell(c[1]), Cell(c[2])]
        }

        pub fn line(self) -> Line {
                Line(self.0 / 3)
        }

        pub fn field(self) -> Field {
                if self.is_row_slice() {
                        let min_field = self.band().0 * 3;
                        Field(min_field + self.0 % 3)
                } else { // col slice
                        let min_field = self.band().0 - 3;
                        Field(min_field + (self.0 % 3) * 3)
                }
        }

        // row band for row slice, col band for col slice
        pub fn band(self) -> Band {
                Band(self.0 / 9)
        }

        pub fn is_row_slice(self) -> bool {
                self.0 < 27
        }

        pub fn band_line(self) -> usize {
                self.0 as usize / 3 % 3
        }

        pub fn band_field(self) -> usize {
                self.0 as usize % 3
        }

        pub fn band_idx(self) -> usize {
                self.band_line() * 3 + self.band_field()
        }
}
/*
#[inline(always)] pub fn row(cell: u8) -> u8 { cell / 9 }
#[inline(always)] pub fn col(cell: u8) -> u8 { cell % 9 }
#[inline(always)] pub fn field(cell: u8) -> u8 { FIELD[cell as usize] }

#[inline(always)] pub fn row_zone(cell: u8) -> usize   { row(cell) as usize + ROW_OFFSET }
#[inline(always)] pub fn col_zone(cell: u8) -> usize   { col(cell) as usize + COL_OFFSET }
#[inline(always)] pub fn field_zone(cell: u8) -> usize { field(cell) as usize + FIELD_OFFSET }
*/

pub(crate) fn cells<T: IntoCells>(t: T) -> T::Output {
        t.into_cells()
}

pub(crate) trait IntoNeighbours {
        type Output;
        fn into_neighbours(self) -> Self::Output;
}

#[inline(always)]
pub(crate) fn neighbours_ty<T: IntoNeighbours>(cell_set: T) -> T::Output {
        cell_set.into_neighbours()
}

extern crate core;
use self::core::convert::From;
/*
impl From<Row> for Line {
        fn from(Row(row): Row) -> Self {
                Line(row)
        }
}

impl From<Col> for Line {
        fn from(Col(col): Col) -> Self {
                Line(col + COL_OFFSET as u8)
        }
}

impl From<Row> for Zone {
        fn from(Row(row): Row) -> Self {
                Zone(row + ROW_OFFSET as u8)
        }
}

impl From<Col> for Zone {
        fn from(Col(col): Col) -> Self {
                Zone(col + COL_OFFSET as u8)
        }
}
*/
impl From<Field> for Zone {
        fn from(Field(field): Field) -> Self {
                Zone(field + FIELD_OFFSET as u8)
        }
}

impl From<Line> for Zone {
        // FIXME: potential difference between Line enumeration and Zone enumeration with OFFSETs
        fn from(Line(line): Line) -> Self {
                Zone(line)
        }
}


pub(crate) trait IntoCells {
        type Output;
        fn into_cells(self) -> Self::Output;
}

/*
impl IntoCells for Row {
        type Output = &'static [u8; 9];
        #[inline(always)]
        fn into_cells(self) -> Self::Output {
                &CELLS_BY_ZONE[self.0 as usize + ROW_OFFSET]
        }
}

impl IntoCells for Col {
        type Output = &'static [u8; 9];
        #[inline(always)]
        fn into_cells(self) -> Self::Output {
                &CELLS_BY_ZONE[self.0 as usize + COL_OFFSET]
        }
}
*/

impl IntoCells for Line {
        type Output = &'static [u8; 9];
        #[inline(always)]
        fn into_cells(self) -> Self::Output {
                let Line(line) = self;
                match line {
                        0...2 => &CELLS_BY_ZONE[line as usize + ROW_OFFSET],
                        // TODO: both code paths should be equal
                        3...6 => &CELLS_BY_ZONE[line as usize - 3 + COL_OFFSET],
                        _ => unreachable!(),
                }
        }
}

impl IntoCells for Field {
        type Output = &'static [u8; 9];
        #[inline(always)]
        fn into_cells(self) -> Self::Output {
                &CELLS_BY_ZONE[self.0 as usize + FIELD_OFFSET]
        }
}

impl IntoCells for Zone {
        type Output = &'static [u8; 9];
        #[inline(always)]
        fn into_cells(self) -> Self::Output {
                &CELLS_BY_ZONE[self.0 as usize]
        }
}

// IntoCells for Intersection impls
// assume that they are only called when there actually is an intersection
// e.g. row 0, field 3 are not allowed

/*
impl IntoCells for Intersection<Row, Field> {
        type Output = &'static [u8; 3];
        #[inline]
        fn into_cells(self) -> Self::Output {
                let Intersection(Row(row), Field(field)) = self;
                debug_assert!(row / 3 == field / 3);
                let field = field % 3;
                &ROW_BLOCK_INTERSECTIONS[row as usize][field as usize]
        }
}

impl IntoCells for Intersection<Col, Field> {
        type Output = &'static [u8; 3];
        #[inline]
        fn into_cells(self) -> Self::Output {
                let Intersection(Col(col), Field(field)) = self;
                debug_assert!(col / 3 == field % 3);
                let field = field % 3;
                &COL_BLOCK_INTERSECTIONS[col as usize][field as usize]
        }
}

impl IntoCells for Intersection<Row, Col> {
        type Output = u8;
        #[inline]
        fn into_cells(self) -> Self::Output {
                let Intersection(Row(row), Col(col)) = self;
                row*9 + col
        }
}
*/

impl IntoNeighbours for Cell {
        type Output =  &'static [u8; 20];
        fn into_neighbours(self) -> Self::Output {
	        &ZONE_NEIGHBOURS_OF_CELL[self.0 as usize]
        }
}

impl IntoNeighbours for Slice {
        type Output = &'static ([u8; 2], [u8; 2]);
        fn into_neighbours(self) -> Self::Output {
	        &SLICE_NEIGHBOURS[self.0 as usize]
        }
}

static SLICES_IN_BAND: [[u8; 9]; 6] = [
        [0, 1, 2, 3, 4, 5, 6, 7, 8],
        [9, 10, 11, 12, 13, 14, 15, 16, 17],
        [18, 19, 20, 21, 22, 23, 24, 25, 26],
        [27, 28, 29, 30, 31, 32, 33, 34, 35],
        [36, 37, 38, 39, 40, 41, 42, 43, 44],
        [45, 46, 47, 48, 49, 50, 51, 52, 53],
];

static CELLS_IN_SLICE: [[u8; 3]; 54] = [
        [0, 1, 2],
        [3, 4, 5],
        [6, 7, 8],
        [9, 10, 11],
        [12, 13, 14],
        [15, 16, 17],
        [18, 19, 20],
        [21, 22, 23],
        [24, 25, 26],
        [27, 28, 29],
        [30, 31, 32],
        [33, 34, 35],
        [36, 37, 38],
        [39, 40, 41],
        [42, 43, 44],
        [45, 46, 47],
        [48, 49, 50],
        [51, 52, 53],
        [54, 55, 56],
        [57, 58, 59],
        [60, 61, 62],
        [63, 64, 65],
        [66, 67, 68],
        [69, 70, 71],
        [72, 73, 74],
        [75, 76, 77],
        [78, 79, 80],
        [0, 9, 18],
        [27, 36, 45],
        [54, 63, 72],
        [1, 10, 19],
        [28, 37, 46],
        [55, 64, 73],
        [2, 11, 20],
        [29, 38, 47],
        [56, 65, 74],
        [3, 12, 21],
        [30, 39, 48],
        [57, 66, 75],
        [4, 13, 22],
        [31, 40, 49],
        [58, 67, 76],
        [5, 14, 23],
        [32, 41, 50],
        [59, 68, 77],
        [6, 15, 24],
        [33, 42, 51],
        [60, 69, 78],
        [7, 16, 25],
        [34, 43, 52],
        [61, 70, 79],
        [8, 17, 26],
        [35, 44, 53],
        [62, 71, 80],
];

// SLICE_NEIGHBORS[slice_nr] = (line_neighbor_slice_nrs, field_neighbor_slice_nrs)
static SLICE_NEIGHBOURS: [([u8; 2], [u8; 2]); 54] = [
        ([1, 2], [3, 6]),
        ([2, 0], [4, 7]),
        ([0, 1], [5, 8]),
        ([4, 5], [6, 0]),
        ([5, 3], [7, 1]),
        ([3, 4], [8, 2]),
        ([7, 8], [0, 3]),
        ([8, 6], [1, 4]),
        ([6, 7], [2, 5]),
        ([10, 11], [12, 15]),
        ([11, 9], [13, 16]),
        ([9, 10], [14, 17]),
        ([13, 14], [15, 9]),
        ([14, 12], [16, 10]),
        ([12, 13], [17, 11]),
        ([16, 17], [9, 12]),
        ([17, 15], [10, 13]),
        ([15, 16], [11, 14]),
        ([19, 20], [21, 24]),
        ([20, 18], [22, 25]),
        ([18, 19], [23, 26]),
        ([22, 23], [24, 18]),
        ([23, 21], [25, 19]),
        ([21, 22], [26, 20]),
        ([25, 26], [18, 21]),
        ([26, 24], [19, 22]),
        ([24, 25], [20, 23]),
        ([28, 29], [30, 33]),
        ([29, 27], [31, 34]),
        ([27, 28], [32, 35]),
        ([31, 32], [33, 27]),
        ([32, 30], [34, 28]),
        ([30, 31], [35, 29]),
        ([34, 35], [27, 30]),
        ([35, 33], [28, 31]),
        ([33, 34], [29, 32]),
        ([37, 38], [39, 42]),
        ([38, 36], [40, 43]),
        ([36, 37], [41, 44]),
        ([40, 41], [42, 36]),
        ([41, 39], [43, 37]),
        ([39, 40], [44, 38]),
        ([43, 44], [36, 39]),
        ([44, 42], [37, 40]),
        ([42, 43], [38, 41]),
        ([46, 47], [48, 51]),
        ([47, 45], [49, 52]),
        ([45, 46], [50, 53]),
        ([49, 50], [51, 45]),
        ([50, 48], [52, 46]),
        ([48, 49], [53, 47]),
        ([52, 53], [45, 48]),
        ([53, 51], [46, 49]),
        ([51, 52], [47, 50]),
];

/*
static ROW_BLOCK_INTERSECTIONS: [[[u8; 3]; 3]; 9] = [
        [
                [0, 1, 2],
                [3, 4, 5],
                [8, 6, 7],
        ],
        [
                [9, 10, 11],
                [12, 13, 14],
                [16, 17, 15],
        ],
        [
                [18, 19, 20],
                [21, 22, 23],
                [24, 25, 26],
        ],
        [
                [27, 28, 29],
                [32, 30, 31],
                [33, 34, 35],
        ],
        [
                [36, 37, 38],
                [40, 41, 39],
                [42, 43, 44],
        ],
        [
                [45, 46, 47],
                [48, 49, 50],
                [51, 52, 53],
        ],
        [
                [56, 54, 55],
                [57, 58, 59],
                [60, 61, 62],
        ],
        [
                [64, 65, 63],
                [66, 67, 68],
                [69, 70, 71],
        ],
        [
                [72, 73, 74],
                [75, 76, 77],
                [80, 78, 79],
        ],
];

static COL_BLOCK_INTERSECTIONS:  [[[u8; 3]; 3]; 9]  = [
        [
                [0, 9, 18],
                [27, 36, 45],
                [72, 54, 63],
        ],
        [
                [1, 10, 19],
                [28, 37, 46],
                [64, 73, 55],
        ],
        [
                [2, 11, 20],
                [29, 38, 47],
                [56, 65, 74],
        ],
        [
                [3, 12, 21],
                [48, 30, 39],
                [57, 66, 75],
        ],
        [
                [4, 13, 22],
                [40, 49, 31],
                [58, 67, 76],
        ],
        [
                [5, 14, 23],
                [32, 41, 50],
                [59, 68, 77],
        ],
        [
                [24, 6, 15],
                [33, 42, 51],
                [60, 69, 78],
        ],
        [
                [16, 25, 7],
                [34, 43, 52],
                [61, 70, 79],
        ],
        [
                [8, 17, 26],
                [35, 44, 53],
                [80, 62, 71],
        ],
];
*/