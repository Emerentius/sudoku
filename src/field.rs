/*

Implements the Field struct.

Fields are the blocks upon which a sudoku is built. They have a set of
"possible numbers", which are the values that a field could have. There is
also the "projected" value, which indicates if the field has already been projected
according to the algorithm in project_numbers.rs

*/

use std::collections::HashSet;

// This is the basic unit of the sudoku
#[derive(Clone)]
pub struct Field {
    pub possible_numbers: HashSet<u8>,
    pub projected: bool
}

impl Field {
    pub fn new() -> Field {
        Field { projected: false, possible_numbers: (1..10).collect() }
    }

    // Returns true if a number has been found
    pub fn number_found(&self) -> bool {
        self.possible_numbers.len() == 1
    }

    // Sets the number of the current field
    pub fn set_number(&mut self, x: u8) {
        self.possible_numbers.clear();
        self.possible_numbers.insert(x);
    }

    // Gets the number of the current field, if any
    // Fails if there is more than one possibility, or none
    pub fn get_number(&self) -> u8 {
        let mut it = self.possible_numbers.iter();
        match (it.next(), it.next()) {
            (Some(&x), None) => { x }
            _ => { panic!("Called get_number(), but there are many possible numbers") }
        }
    }

    // Removes a possibility from the field and returns true if it was contained
    pub fn cannot_be(&mut self, x: u8) -> bool {
        // If there is only one possibility, it cannot be removed
        if self.possible_numbers.len() == 1 {
            return false;
        }

        self.possible_numbers.remove(&x)
    }

    // Resets the possibilities to their default range [1, 9]
    pub fn reset_possibilities(&mut self) {
        self.possible_numbers.clear();
        self.possible_numbers.extend(1..10);
    }

    // Give the field the next value available
    pub fn set_next_number(&mut self) -> bool {
        if !self.number_found() {
            self.set_number(1);
        } else {
            let number = self.get_number();

            // 9 is the last number available
            if number == 9 {
                self.reset_possibilities();
                return false;
            }

            self.set_number(number + 1);
        }

        true
    }
}