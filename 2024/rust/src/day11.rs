use std::{collections::HashSet, str::FromStr};

use crate::day::AoCDay;

pub struct Day {
    cells: Vec<Vec<u32>>,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cells: Vec<Vec<u32>> = s
            .split('\n')
            .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
            .collect();
        Ok(Day { cells })
    }
}

impl Day {
    pub fn traverse(&self, r: usize, c: usize) -> Vec<(usize, usize)> {
        let mut nine_locations = vec![];
        let cur = 0;
        let mut pending = vec![(r, c, cur)];

        while let Some((r, c, cur)) = pending.pop() {
            if cur == 9 {
                nine_locations.push((r, c));
            } else {
                if r + 1 < self.cells.len() && self.cells[r + 1][c] == cur + 1 {
                    pending.push((r + 1, c, cur + 1));
                }

                if r > 0 && self.cells[r - 1][c] == cur + 1 {
                    pending.push((r - 1, c, cur + 1));
                }

                if c + 1 < self.cells[r].len() && self.cells[r][c + 1] == cur + 1 {
                    pending.push((r, c + 1, cur + 1));
                }

                if c > 0 && self.cells[r][c - 1] == cur + 1 {
                    pending.push((r, c - 1, cur + 1));
                }
            }
        }

        nine_locations
    }

    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        (0..self.cells.len())
            .map(|r| {
                (0..self.cells[r].len())
                    .filter_map(|c| {
                        if self.cells[r][c] == 0 {
                            let vec = self.traverse(r, c);
                            let set: HashSet<_> = HashSet::from_iter(vec);
                            Some(set.len())
                        } else {
                            None
                        }
                    })
                    .sum::<usize>()
            })
            .sum::<usize>()
            .to_string()
    }

    fn part2(&mut self) -> String {
        (0..self.cells.len())
            .map(|r| {
                (0..self.cells[r].len())
                    .filter_map(|c| {
                        if self.cells[r][c] == 0 {
                            Some(self.traverse(r, c).len())
                        } else {
                            None
                        }
                    })
                    .sum::<usize>()
            })
            .sum::<usize>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1_input() {
        let actual = Day::new(INPUT).part1();
        assert_eq!(actual, "36");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(INPUT).part2();
        assert_eq!(actual, "81");
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

    const PUZZLE_INPUT: &str = include_str!("../data/day10");

    const INPUT: &str = "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732";
}
