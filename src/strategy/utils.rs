// WIP
#![allow(unused)]
use super::strategies::prelude::*;
use std::collections::HashMap;

enum Link {
    Strong,
    Weak,
}

// sparse matrix in key-value representation
type LinkMatrix = HashMap<Candidate, Vec<Candidate>>;

struct Links {
    weak: LinkMatrix,
    strong: LinkMatrix,
}

// finds strong and weak links between candidates residing in the same cell
// or house
fn find_direct_links(
    cells_poss_digits: &CellArray<Set<Digit>>,
    house_poss_positions: &HouseArray<DigitArray<Set<Position<House>>>>,
) -> Links {
    let mut strong_links = LinkMatrix::new();
    let mut weak_links = LinkMatrix::new();
    for cell in Cell::all() {
        let digits = cells_poss_digits[cell];
        for digit in digits {
            let candidate1 = Candidate { cell, digit };

            // TODO: don't iterate twice over each combination
            let other_digits = (digits ^ digit);

            // cell links
            let insert_link = |other_digit, link_collection: &mut LinkMatrix| {
                let candidate2 = Candidate {
                    cell,
                    digit: other_digit,
                };
                link_collection.entry(candidate1).or_default().push(candidate2);
            };
            // if unique() returns Err(Zero), then it's a naked single
            // not of interest here
            if let Some(other_digit) = other_digits.unique().unwrap_or(None) {
                insert_link(other_digit, &mut strong_links);
                continue;
            }

            for digit2 in other_digits {
                insert_link(digit2, &mut weak_links);
            }

            // House links
            for (house, other_positions) in conflicting_house_positions(candidate1, house_poss_positions) {
                let insert_link = |other_pos, link_collection: &mut LinkMatrix| {
                    let other_cell = house.cell_at(other_pos);
                    let candidate2 = Candidate {
                        cell: other_cell,
                        digit,
                    };
                    link_collection.entry(candidate1).or_default().push(candidate2);
                };
                // if unique() returns Err(Zero), then it's a hidden single
                // not of interest here
                if let Some(other_pos) = other_positions.unique().unwrap_or(None) {
                    insert_link(other_pos, &mut strong_links);
                    continue;
                }

                for other_pos in other_positions {
                    insert_link(other_pos, &mut weak_links);
                }
            }
        }
    }

    Links {
        strong: strong_links,
        weak: weak_links,
    }
}

fn conflicting_house_positions(
    Candidate { cell, digit }: Candidate,
    house_poss_positions: &HouseArray<DigitArray<Set<Position<House>>>>,
) -> impl Iterator<Item = (House, Set<Position<House>>)> + '_ {
    // TODO: remove vectors when const generics are there
    //       or use some closure capture trickery
    let positions = vec![cell.row_pos(), cell.col_pos(), cell.block_pos()];
    let houses = cell.houses().to_vec();
    houses
        .into_iter()
        .zip(positions)
        .map(move |(house, pos)| (house, house_poss_positions[house][digit] ^ pos))
}
