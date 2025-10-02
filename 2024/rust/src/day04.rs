use std::str::FromStr;

use crate::day::AoCDay;
use crate::utils::Grid;

pub struct Day {
    grid: Grid<char>,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let grid: Grid<char> = Grid::new(s, |s: &str| s.chars().collect());
        Ok(Day { grid })
    }
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        let xmas = vec!['X', 'M', 'A', 'S'];
        let samx = vec!['S', 'A', 'M', 'X'];

        let mut count = 0;

        for row in 0..self.grid.cells.len() {
            for column in 0..self.grid.cells[row].len() {
                let h = self
                    .grid
                    .read_horizontal(row, column, crate::utils::HDir::Right, 4);
                if h == xmas || h == samx {
                    count += 1
                }
                let v = self
                    .grid
                    .read_vertical(row, column, crate::utils::VDir::Down, 4);
                if v == xmas || v == samx {
                    count += 1
                }

                let d = self.grid.read_diagonal(
                    row,
                    column,
                    crate::utils::HDir::Right,
                    crate::utils::VDir::Down,
                    4,
                );
                if d == xmas || d == samx {
                    count += 1
                }

                let d = self.grid.read_diagonal(
                    row,
                    column,
                    crate::utils::HDir::Left,
                    crate::utils::VDir::Down,
                    4,
                );
                if d == xmas || d == samx {
                    count += 1
                }
            }
        }

        count.to_string()
    }

    fn part2(&mut self) -> String {
        let mas = vec!['M', 'A', 'S'];
        let sam = vec!['S', 'A', 'M'];

        let mut count = 0;

        for row in 0..self.grid.cells.len() {
            for column in 0..self.grid.cells[row].len() {
                let rd = self.grid.read_diagonal(
                    row,
                    column,
                    crate::utils::HDir::Right,
                    crate::utils::VDir::Down,
                    3,
                );
                if rd == mas || rd == sam {
                    let ld = self.grid.read_diagonal(
                        row,
                        column + 2,
                        crate::utils::HDir::Left,
                        crate::utils::VDir::Down,
                        3,
                    );
                    if ld == mas || ld == sam {
                        count += 1
                    }
                }
            }
        }

        count.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_INPUT: &str = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX";

    const PUZZLE_INPUT: &str = include_str!("../data/day04");

    #[test]
    fn test_part1_input() {
        let actual = Day::new(TEST_INPUT).part1();
        assert_eq!(actual, "18");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(TEST_INPUT).part2();
        assert_eq!(actual, "9");
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
}
