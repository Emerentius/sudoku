use super::CellState;
use std::fmt::Formatter;

pub struct GridState([CellState; 81]);

/* Example output
┌──────────────────────────────┬──────────────────────────────┬──────────────────────────────┐
│ 1         2         3        │ 4         5         6        │ 7         8         _        │
│ 45678     45678     45678    │ 12378     12378     12378    │ 123456    123456    9        │
│ 456789    456789    456789   │ 123789    123789    123789   │ 123456    123456    123456   │
├──────────────────────────────┼──────────────────────────────┼──────────────────────────────┤
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
├──────────────────────────────┼──────────────────────────────┼──────────────────────────────┤
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
└──────────────────────────────┴──────────────────────────────┴──────────────────────────────┘
*/

impl std::fmt::Display for GridState {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        let mut column_widths = [0; 9];
        for col in 0..9 {
            let max_width = (0..9)
                .map(|row| match self.0[row * 9 + col] {
                    CellState::Digit(_) => 1,
                    CellState::Candidates(cands) => std::cmp::max(1, cands.len()),
                })
                .max()
                .unwrap();
            debug_assert!(1 <= max_width && max_width <= 9);
            column_widths[col] = max_width;
        }

        let stack_width = |stack_nr: usize| {
            column_widths[stack_nr * 3..stack_nr * 3 + 3]
                .iter()
                .map(|&x| x as usize)
                .sum::<usize>()
                + 6 // spaces in between cells and walls
        };

        let print_horizontal_delimiter =
            |f: &mut Formatter, leftmost: char, middle: char, rightmost: &str| {
                write!(
                    f,
                    "{left}{0:─<1$}{middle}{0:─<2$}{middle}{0:─<3$}{right}",
                    "",
                    stack_width(0),
                    stack_width(1),
                    stack_width(2),
                    left = leftmost,
                    middle = middle,
                    right = rightmost,
                )
            };
        let print_minirow = |f: &mut Formatter, row, stack| {
            let base_col = (stack * 3) as usize;
            let base_idx = (row * 9 + stack * 3) as usize;
            //for col in stack * 3..stack * 3 + 3 {
            write!(
                f,
                " {:width1$}  {:width2$}  {:width3$} │",
                self.0[base_idx],
                self.0[base_idx + 1],
                self.0[base_idx + 2],
                width1 = column_widths[base_col] as usize,
                width2 = column_widths[base_col + 1] as usize,
                width3 = column_widths[base_col + 2] as usize,
            )
        };

        let print_band = |f: &mut Formatter, band| {
            for row in band * 3..band * 3 + 3 {
                write!(f, "│")?;
                for stack in 0..3 {
                    print_minirow(f, row, stack)?;
                }
                writeln!(f)?;
            }
            Ok(())
        };

        print_horizontal_delimiter(f, '┌', '┬', "┐\n")?;
        print_band(f, 0)?;
        print_horizontal_delimiter(f, '├', '┼', "┤\n")?;
        print_band(f, 1)?;
        print_horizontal_delimiter(f, '├', '┼', "┤\n")?;
        print_band(f, 2)?;
        print_horizontal_delimiter(f, '└', '┴', "┘")
    }
}

#[test]
fn grid_state_roundtrip() {
    use crate::strategy::StrategySolver;
    let s = "┌──────────────────────────────┬──────────────────────────────┬──────────────────────────────┐
│ 1         2         3        │ 4         5         6        │ 7         8         _        │
│ 45678     45678     45678    │ 12378     12378     12378    │ 123456    123456    9        │
│ 456789    456789    456789   │ 123789    123789    123789   │ 123456    123456    123456   │
├──────────────────────────────┼──────────────────────────────┼──────────────────────────────┤
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
├──────────────────────────────┼──────────────────────────────┼──────────────────────────────┤
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
│ 23456789  13456789  12456789 │ 12356789  12346789  12345789 │ 12345689  12345679  12345678 │
└──────────────────────────────┴──────────────────────────────┴──────────────────────────────┘";
    let solver = StrategySolver::from_grid_state_str(s);
    let grid_state = GridState(solver.grid_state());
    let new_str = format!("{}", grid_state);
    assert_eq!(s, &new_str[..]);
}
