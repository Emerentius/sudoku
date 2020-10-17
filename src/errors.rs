#[cfg(doc)]
use crate::Sudoku;

/// Error for [`Sudoku::from_bytes`]
#[derive(Debug, thiserror::Error)]
#[error("byte array contains entries >9")]
pub struct FromBytesError(pub(crate) ());

/// Error for [`Sudoku::from_bytes_slice`]
#[derive(Debug, thiserror::Error)]
pub enum FromBytesSliceError {
    /// Slice is not 81 long
    #[error("byte slice should have length 81, found {0}")]
    WrongLength(usize),
    /// Slice contains invalid entries
    #[error(transparent)]
    FromBytesError(FromBytesError),
}
