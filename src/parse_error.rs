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