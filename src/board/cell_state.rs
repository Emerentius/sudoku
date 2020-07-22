use crate::bitset::Set;
use crate::board::Digit;

/// Contains either a digit or all the candidates for an unsolved cell
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[allow(missing_docs)]
pub enum CellState {
    Digit(Digit),
    Candidates(Set<Digit>),
}

impl std::fmt::Display for CellState {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let requested_width = f.width().unwrap_or(0);
        match self {
            Self::Digit(digit) => write!(f, "{:<width$}", digit.get(), width = requested_width),
            &Self::Candidates(cands) => {
                if cands.is_empty() {
                    write!(f, "_")?;
                } else {
                    for cand in cands {
                        write!(f, "{}", cand.get())?;
                    }
                }

                let required_padding = requested_width.saturating_sub(std::cmp::max(cands.len(), 1) as usize);
                if required_padding > 0 {
                    write!(f, "{:<width$}", " ", width = required_padding)?;
                }
                Ok(())
            }
        }
    }
}
