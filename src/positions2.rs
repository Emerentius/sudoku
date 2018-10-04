#![allow(unused, missing_docs)]
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not, BitXor, BitXorAssign};
use std::num::NonZeroU8;
use types::Unsolvable;

macro_rules! define_types(
    ($( $vis:tt $name:ident : $limit:expr ),* $(,)*) => {
        $(
            define_types!(@internal $vis $name : $limit );
        )*
    };
    (@internal priv $name:ident : $limit:expr) => {
        #[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
        struct $name(u8);

        define_types!(@internal $name $limit);
    };
    (@internal pub $name:ident : $limit:expr) => {
        #[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug, Hash)]
        pub struct $name(u8);

        define_types!(@internal $name $limit);
    };
    (@internal $name:ident $limit:expr) => {
        impl $name {
            pub fn new(num: u8) -> Self {
                debug_assert!(num < $limit);
                $name(num)
            }

            pub fn new_checked(num: u8) -> Option<Self> {
                if num < $limit {
                    Some($name(num))
                } else {
                    None
                }
            }

            pub fn val(self) -> u8 {
                self.0
            }

            pub fn as_index(self) -> usize {
                self.0 as _
            }

            pub fn all() -> impl Iterator<Item = Self> {
                (0..$limit).map(Self::new)
            }
        }
    };
);

define_types!(
    pub Cell: 81,
    pub Row: 9,
    pub Col: 9,
    pub Block: 9,
    pub Line: 18,
    pub House: 27,
    pub MiniRow: 27,
    pub MiniCol: 27,
    pub MiniLine: 54,
    pub Band: 3,
    pub Stack: 3,
    pub Chute: 6,
);

// define digit separately because it has an offset
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug, Hash)]
pub struct Digit(NonZeroU8);

impl Digit {
    pub fn new(digit: u8) -> Self {
        debug_assert!(digit <= 9);
        Self::new_checked(digit).unwrap()
    }

    pub fn from_index(idx: u8) -> Self {
        debug_assert!(idx < 9);
        Self::new_checked(idx+1).unwrap()
    }

    pub fn new_checked(num: u8) -> Option<Self> {
        NonZeroU8::new(num).map(Digit)
    }

    pub fn all() -> impl Iterator<Item = Self> {
        (1..10).map(Digit::new)
    }

    pub fn val(self) -> u8 {
        self.0.get()
    }

    pub fn as_index(self) -> usize {
        self.val() as usize - 1
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum LineType {
    Row(Row),
    Col(Col),
}

impl Line {
    pub fn categorize(self) -> LineType {
        debug_assert!(self.0 < 18);
        match self.0 < 9 {
            true => LineType::Row(Row::new(self.0)),
            false => LineType::Col(Col::new(self.0 - 9)),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum HouseType {
    Row(Row),
    Col(Col),
    Block(Block),
}

impl House {
    pub fn categorize(self) -> HouseType {
        debug_assert!(self.0 < 27);
        match self.0 {
            0..= 8 => HouseType::Row(Row::new(self.0)),
            9..=17 => HouseType::Col(Col::new(self.0 - 9)),
            _      => HouseType::Block(Block::new(self.0 - 18)),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum ChuteType {
    Band(Band),
    Stack(Stack),
}

impl Chute {
    pub fn categorize(self) -> ChuteType {
        debug_assert!(self.0 < 6);
        match self.0 < 3 {
            true => ChuteType::Band(Band::new(self.0)),
            false => ChuteType::Stack(Stack::new(self.0 - 3)),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum MiniLineType {
    MiniRow(MiniRow),
    MiniCol(MiniCol),
}

impl MiniLine {
    pub fn categorize(self) -> MiniLineType {
        debug_assert!(self.0 < 54);
        match self.0 < 27 {
            true => MiniLineType::MiniRow(MiniRow::new(self.0)),
            false => MiniLineType::MiniCol(MiniCol::new(self.0 - 27)),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct Position<IN>(pub(crate) u8, std::marker::PhantomData<IN>);

impl<IN> Position<IN> {
    pub fn new(pos: u8) -> Self {
        Position(pos, std::marker::PhantomData)
    }

    fn as_index(self) -> usize {
        self.0 as _
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Set<T: SetElement>(T::Storage);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct SetIter<T: SetElement>(T::Storage);

impl<T: SetElement> IntoIterator for Set<T>
where
    SetIter<T>: Iterator,
{
    type Item = <SetIter<T> as Iterator>::Item;
    type IntoIter = SetIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        SetIter(self.0)
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
                fn $fn_name(self, other: Self) -> Self {
                    Set(
                        $trait::$fn_name(self.0, other.0)
                    )
                }
            }
        )*
    };
}

macro_rules! impl_bitops_assign {
    ( $( $trait:ident, $fn_name:ident);* $(;)* ) => {
        $(
            impl<T: SetElement> $trait for Set<T> {
                fn $fn_name(&mut self, other: Self) {
                    $trait::$fn_name(&mut self.0, other.0)
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
    Self: PartialEq + Copy
{
    type Output = Self;
    fn not(self) -> Self {
        Self::ALL.without(self)
    }
}

impl<T: SetElement> Set<T>
where
    // TODO: properly implement the traits for Set and SetIter
    //       bounded on T::Storage, not on T (which derive does)
    Self: PartialEq + Copy
{
    pub const ALL: Set<T> = Set(<T as SetElement>::ALL);
    pub const NONE: Set<T> = Set(<T as SetElement>::NONE);

    pub fn new(mask: T::Storage) -> Self {
        Set(mask)
    }

    pub fn without(self, other: Self) -> Self {
        Set(self.0 & !other.0)
    }

    pub fn remove(&mut self, other: Self) {
        self.0 &= !other.0;
    }

    pub fn overlaps(&self, other: Self) -> bool {
        *self & other != Set::NONE
    }

    pub fn len(&self) -> u8 {
        T::count_possibilities(self.0) as u8
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_full(&self) -> bool {
        *self == Self::ALL
    }

    // TODO: make enum for return value
    pub fn unique(self) -> Result<Option<T>, Unsolvable>
    where
        SetIter<T>: Iterator<Item = T>,
    {
        match self.len() {
            0 => Err(Unsolvable),
            1 => Ok({
                let element = self.into_iter().next();
                debug_assert!(element.is_some());
                element
            }),
            _ => Ok(None),
        }
    }

    pub fn one_possibility(self) -> T
    where
        SetIter<T>: Iterator<Item = T>,
    {
        self.into_iter().next().expect("mask is empty")
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////

// TODO: Decide about visibility
use self::set_element::SetElement;
pub mod set_element {
    use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, Not, BitXor, BitXorAssign};
    use super::Set;

    pub trait SetElement: Sized {
        const ALL: Self::Storage;
        const NONE: Self::Storage;

        type Storage:
            BitAnd<Output = Self::Storage> + BitAndAssign
            + BitOr<Output = Self::Storage> + BitOrAssign
            + BitXor<Output = Self::Storage> + BitXorAssign
            + Not<Output = Self::Storage>
            + Copy;

        fn count_possibilities(set: Self::Storage) -> u32;
        fn as_set(self) -> Set<Self>;
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
    //House => u32, 0o777_777_777, // Rows, Cols, Blocks

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
            impl Iterator for SetIter<$type> {
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
    //Position<Row> => Position::new,
    //Position<Col> => Position::new,
    Position<Line> => Position::new,
    Position<House> => Position::new,
    //Position<Band> => Position::new,
    //Position<Stack> => Position::new,
    //Position<Chute> => Position::new,
);

macro_rules! into_cells {
    ( $( $name:ident => |$arg:ident| $code:block );* $(;)* ) => {
        $(
            impl $name {
                pub fn cells(self) -> Set<Cell> {
                    let $arg = self;
                    Set($code)
                }
            }
        )*
    };
}

// the closures here aren't actually closures, they just introduce
// the variables to be used in the code blocks for macro hygiene reasons
into_cells!(
    Cell => |cell| { 1 << cell.0 };
    Row  => |row| { 0o777 << (9 * row.0) };
    Col  => |col| { 0o_001_001_001___001_001_001___001_001_001 << col.0 };
    Block  => |block| {
        let band = block.0 / 3;
        let stack = block.0 % 3;
        0o007_007_007 << (band * 27 + stack * 3)
    };
    Line => |line| {
        use self::LineType::*;
        match line.categorize() {
            Row(row) => row.cells().0,
            Col(col) => col.cells().0,
        }
    };
    House => |house| {
        use self::HouseType::*;
        match house.categorize() {
            Row(row) => row.cells().0,
            Col(col) => col.cells().0,
            Block(block) => block.cells().0,
        }
    };
    MiniRow => |mr| { 0o7 << 3 * mr.0 };
    //MiniCol => |mc| { 0o001_001_001 << mc / 9 * 27 + mc % 9 }; // old, different counting system
    MiniCol => |mc| {
        let band = mc.0 % 3;
        let col = mc.0 / 3;
        0o001_001_001 << band * 27 + col
    };
    MiniLine => |ml| {
        use self::MiniLineType::*;
        match ml.categorize() {
            MiniRow(mr)  => mr.cells().0,
            MiniCol(mc) => mc.cells().0,
        }
    };
    Band => |band| { 0o777_777_777 << 27 * band.0 };
    Stack => |stack| { 0o_007_007_007___007_007_007___007_007_007 << 3 * stack.0 };
    Chute => |chute| {
        use self::ChuteType::*;
        match chute.categorize() {
            Band(band) => band.cells().0,
            Stack(stack) => stack.cells().0,
        }
    };
);

///////////////////////////////////////////////////////////////////////////////////////////////
//                                  Conversions
///////////////////////////////////////////////////////////////////////////////////////////////

macro_rules! define_conversion_shortcuts {
    (
        $(
            $type:ty : {
                $( $target_type:ty , $method_name:ident );* $(;)*
            }
        )*
    ) => {
        $(
            impl $type {
                $(
                    #[inline(always)]
                    pub fn $method_name(self) -> $target_type {
                        <$target_type>::from(self)
                    }
                )*
            }
        )*
    };
}

define_conversion_shortcuts!(
    Cell : {
        Row, row;
        Col, col;
        Block, block;
        //Position<Row>, row_pos;
        //Position<Col>, col_pos;
        //Position<Block>, block_pos;
    }
    /*
    Row : {
        Line, line;
        House, house;
    }
    Col : {
        Line, line;
        House, house;
    }
    Block : {
        House, house;
    }
    Position<Row> : {
        Position<Line>, line_pos;
        Position<House>, house_pos;
    }
    Position<Col> : {
        Position<Line>, line_pos;
        Position<House>, house_pos;
    }
    Position<Block> : {
        Position<House>, house_pos;
    }
    */
);

impl Cell {
    pub fn row_pos(self) -> Position<House> {
        Position::<Row>::from(self).into()
    }

    pub fn col_pos(self) -> Position<House> {
        Position::<Col>::from(self).into()
    }

    pub fn block_pos(self) -> Position<House> {
        Position::<Block>::from(self).into()
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////

impl Cell {
    pub fn houses(self) -> [House; 3] {
            [self.row().house(), self.col().house(), self.block().house() ]
    }
}

impl Chute {
    pub fn minilines(self) -> [MiniLine; 9] {
        let mut slices = [MiniLine(0); 9];
        for (i, slice) in (0..9).zip(slices.iter_mut()) {
                *slice = MiniLine(self.0 * 9 + i);
        }
        slices
    }
}

impl Line {
    pub const ALL_ROWS: Set<Line> = Set(0o000_777);
    pub const ALL_COLS: Set<Line> = Set(0o777_000);
}

impl MiniLine {
    pub fn neighbours(self) -> ([MiniLine; 2], [MiniLine; 2]) {
        // line neighbour, field neighbour
        let (ln, bn) = MINILINE_NEIGHBOURS[self.as_index()];
        (
            [ MiniLine(ln[0]), MiniLine(ln[1]) ],
            [ MiniLine(bn[0]), MiniLine(bn[1]) ],
        )
    }

    // TODO: refactor to be part of the define_conversion_shortcuts macro
    pub fn chute(self) -> Chute {
        Chute::new(self.0 / 9)
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////

impl Row {
    pub fn cell_at(self, pos: Position<Row>) -> Cell {
        Cell::new(self.0 * 9 + pos.0)
    }
}

impl Col {
    pub fn cell_at(self, pos: Position<Col>) -> Cell {
        Cell::new(pos.0 * 9 + self.0)
    }
}

impl Block {
    pub fn cell_at(self, pos: Position<Block>) -> Cell {
        // TODO: use (implement) methods for getting band of block and such
        let band = self.0 / 3;
        let stack = self.0 % 3;
        let row_in_band = pos.0 / 3;
        let col_in_stack = pos.0 % 3;
        let row = band * 3 + row_in_band;
        let col = stack * 3 + col_in_stack;
        Cell::new(row * 9 + col)
    }
}

impl Line {
    pub fn cell_at(self, pos: Position<Line>) -> Cell {
        match self.categorize() {
            LineType::Row(row) => row.cell_at(Position::new(pos.0)),
            LineType::Col(col) => col.cell_at(Position::new(pos.0)),
        }
    }
}

impl House {
    pub fn cell_at(self, pos: Position<House>) -> Cell {
        match self.categorize() {
            HouseType::Row(row) => row.cell_at(Position::new(pos.0)),
            HouseType::Col(col) => col.cell_at(Position::new(pos.0)),
            HouseType::Block(block) => block.cell_at(Position::new(pos.0)),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////
macro_rules! impl_from {
    ( $( $from:ty, $to:ty, |$arg:ident| $code:block ),* $(,)* ) => {
        $(
            impl From<$from> for $to {
                fn from($arg: $from) -> $to {
                    let $arg = $arg.0;
                    <$to>::new($code)
                }
            }
        )*
    };
}

// same as above, but doesn't take the inner element before
// executing $code
macro_rules! impl_from_raw {
    ( $( $from:ty, $to:ty, |$arg:ident| $code:block ),* $(,)* ) => {
        $(
            impl From<$from> for $to {
                fn from($arg: $from) -> $to {
                    $code
                }
            }
        )*
    };
}

impl_from!(
    Row, Line, |r| { r },
    Col, Line, |c| { c + 9 },
    Row, House, |r| { r },
    Col, House, |c| { c + 9 },
    Line, House, |l| { l },
    Block, House, |b| { b + 18 },
    Band, Chute, |b| { b },
    Stack, Chute, |s| { s + 3 },
);

// non-equivalent conversions
// the first type is the container of the second
impl_from!(
    Cell, Row, |c| { row(c) },
    Cell, Col, |c| { col(c) },
    Cell, Block, |c| { 3 * band(c) + stack(c) },
    Cell, Band, |c| { band(c) },
    Cell, Stack, |c| { stack(c) },
    Cell, Position<Row>, |c| { col(c) },
    Cell, Position<Col>, |c| { row(c) },
    Position<Row>, Position<Line>, |pos| { pos },
    Position<Col>, Position<Line>, |pos| { pos },
    Position<Row>, Position<House>, |pos| { pos },
    Position<Col>, Position<House>, |pos| { pos },
    Position<Line>, Position<House>, |pos| { pos },
    Position<Block>, Position<House>, |pos| { pos },
    Cell, Position<Block>, |c| { row(c) % 3 * 3 + col(c) % 3 },
    Cell, Position<Band>, |c| { row(c) % 3 * 9 + col(c) },
    Position<Chute>, Position<Band>, |pos| { pos },
    //Cell, Position<Stack>, |c| { row(c) % 3 * 3 + col(c) % 3 }, // not sure how to lay this out, yet
);

impl<T: SetElement> From<T> for Set<T> {
    fn from(arg: T) -> Set<T> {
        arg.as_set()
    }
}

impl_from_raw!(
    Position<Row>, Set<Position<Line>>, |pos| { Position::<Line>::from(pos).as_set() },
    Position<Col>, Set<Position<Line>>, |pos| { Position::<Line>::from(pos).as_set() },
    Position<Row>, Set<Position<House>>, |pos| { Position::<House>::from(pos).as_set() },
    Position<Col>, Set<Position<House>>, |pos| { Position::<House>::from(pos).as_set() },
    Position<Line>, Set<Position<House>>, |pos| { Position::<House>::from(pos).as_set() },
    Position<Block>, Set<Position<House>>, |pos| { Position::<House>::from(pos).as_set() },
);

fn row(cell: u8) -> u8 {
    cell / 9
}

fn col(cell: u8) -> u8 {
    cell % 9
}

fn band(cell: u8) -> u8 {
    cell / 27
}

fn stack(cell: u8) -> u8 {
    col(cell) / 3
}

pub fn neighbours2(cell: Cell) -> impl IntoIterator<Item = Cell> {
    use positions::neighbours;
    neighbours(cell.as_index() as u8).into_iter().cloned().map(Cell::new)
}

// TODO: generalize
impl Set<Position<House>> {
    pub fn as_line_set(self) -> Set<Position<Line>> {
        debug_assert!(self.0 <= Set::<Position<Line>>::ALL.0);
        Set::new(self.0)
    }
}

pub trait IntoHouse: Into<House> {
    #[inline(always)]
    fn house(self) -> House {
        self.into()
    }
}

impl<T: Into<House>> IntoHouse for T {}

static MINILINE_NEIGHBOURS: [([u8; 2], [u8; 2]); 54] = [
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn unique() {
        for digit in <Set<Digit>>::ALL {
            assert_eq!(digit.as_set().into_iter().count(), 1);
        }
    }

    #[test]
    fn row_cells() {
        for (raw_row, row) in (0..9).map(|r| (r, Row::new(r))) {
            let first_cell = raw_row * 9;

            let iter1 = row.cells().into_iter();
            let iter2 = (first_cell..first_cell+9).map(Cell::new);
            assert!( iter1.eq(iter2) );
        }
    }

    #[test]
    fn col_cells() {
        for (raw_col, col) in (0..9).map(|c| (c, Col::new(c))) {
            let iter1 = col.cells().into_iter();
            let iter2 = (raw_col..81).step_by(9).map(Cell::new);
            assert!( iter1.eq(iter2) );
        }
    }

    /*
    #[test]
    fn block_cells() {
        for (raw_block, block) in (0..9).map(|b| (b, Block::new(b))) {
            let iter1 = block.cells().into_iter();


            let iter2 = (raw_block..81).step_by(9).map(Cell::new);
            assert!( iter1.eq(iter2) );
        }
    }
    */

    #[test]
    fn band_cells() {
        for (raw_band, band) in (0..3).map(|b| (b, Band::new(b))) {
            let first_cell = raw_band * 27;

            let iter1 = band.cells().into_iter();
            let iter2 = (first_cell..first_cell+27).map(Cell::new);
            assert!( iter1.eq(iter2) );
        }
    }

    #[test]
    fn stack_cells() {
        for (raw_stack, stack) in (0..3).map(|s| (s, Stack::new(s))) {
            let first_col = raw_stack * 3;

            let iter1 = stack.cells().into_iter();

            let iter2 = (0..9).flat_map(|row| {
                    let cell = row * 9 + first_col;
                    cell..cell+3
                })
                .map(Cell::new);

            assert!( iter1.eq(iter2) );
        }
    }
}
