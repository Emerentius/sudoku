use std::fmt;

#[derive(Debug)]
pub enum ParseError {
    InvalidLineLength(u8),
    InvalidNumber(u8, char)
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::ParseError::*;

        try!(write!(f, "Error parsing sudoku: "));
        match *self {
            InvalidLineLength(line_nr) => write!(f, "line number {} should contain exactly 9 characters", line_nr),
            InvalidNumber(line_nr, chr) => write!(f, "line number {} contains the invalid character `{}`", line_nr, chr)
        }
    }
}