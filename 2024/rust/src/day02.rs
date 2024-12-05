use std::str::FromStr;

use crate::{day::AoCDay, utils::Grid};

pub struct Day {
    grid: Grid<i32>,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let grid = Grid::new(s, |s| {
            s.split_ascii_whitespace()
                .into_iter()
                .filter_map(|s| s.parse::<i32>().ok())
                .collect()
        });
        Ok(Day { grid })
    }
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }

    pub fn is_safe(&self, vec: &Vec<i32>) -> bool {
        for (a, b) in vec.iter().zip(vec.iter().skip(1)) {
            let val = a - b;
            if vec[0] > vec[1] {
                if val <= 0 || val > 3 {
                    return false;
                }
            } else if vec[0] < vec[1] {
                if val < -3 || val >= 0 {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        let mut count = 0;
        for row in 0..self.grid.cells.len() {
            if self.is_safe(&self.grid.cells[row]) {
                count += 1;
            }
        }
        count.to_string()
    }

    fn part2(&mut self) -> String {
        let mut count = 0;
        for row in 0..self.grid.cells.len() {
            let cc = self.grid.cells[row].len();
            for c in 0..cc {
                let mut vec = Vec::from(&self.grid.cells[row][0..c]);
                vec.extend_from_slice(&self.grid.cells[row][c+1..cc]);
                if self.is_safe(&vec) {
                    count += 1;
                    break;
                }
            }
        }
        count.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const PUZZLE_INPUT: &str = include_str!("../data/day02");

    #[test]
    fn test_part1_input() {
        let actual = Day::new(TEST_INPUT).part1();
        assert_eq!(actual, "2");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(TEST_INPUT).part2();
        assert_eq!(actual, "4");
    }

    #[test]
    fn test_part1_puzzle_input() {
        let mut day = Day::new(PUZZLE_INPUT);
        let actual = day.part1();
        println!("{actual}");
        assert_eq!(true, true);
    }

    #[test]
    fn test_part2_puzzle_input() {
        let mut day = Day::new(PUZZLE_INPUT);
        let actual = day.part2();
        println!("{actual}");
        assert_eq!(true, true);
    }

    const TEST_INPUT: &str = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9";
}
