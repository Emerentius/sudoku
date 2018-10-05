use ::std::num::NonZeroU8;

// define digit separately because it has an offset
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug, Hash)]
pub struct Digit(NonZeroU8);

impl Digit {
    pub fn new(digit: u8) -> Self {
        Self::new_checked(digit).unwrap()
    }

    pub fn new_checked(digit: u8) -> Option<Self> {
        if digit > 9 {
            return None;
        }
        NonZeroU8::new(digit).map(Digit)
    }

    pub(crate) fn from_index(idx: u8) -> Self {
        Self::new_checked(idx+1).unwrap()
    }

    pub fn all() -> impl Iterator<Item = Self> {
        (1..10).map(Digit::new)
    }

    pub fn get(self) -> u8 {
        self.0.get()
    }

    pub fn as_index(self) -> usize {
        self.get() as usize - 1
    }
}
