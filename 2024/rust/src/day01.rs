use std::{collections::HashMap, fs::File, io::Read, str::FromStr};

use crate::{day::AoCDay, utils::Grid};

const TEST_INPUT: &str = "3   4
4   3
2   5
1   3
3   9
3   3";

const PUZZLE_INPUT: &str = include_str!("../data/day01");

pub struct Day {
    left: Vec<i32>,
    right: Vec<i32>,
    right_count: HashMap<i32, i32>,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut right_count = HashMap::new();

        let grid = Grid::new(s, |s| {
            s.split_ascii_whitespace()
                .filter_map(|s| s.parse::<i32>().ok())
                .collect()
        });

        let mut left = grid.read_column(0);
        let mut right = grid.read_column(1);

        for num in right.iter() {
            right_count.entry(*num).and_modify(|c| *c += 1).or_insert(1);
        }

        left.sort();
        right.sort();
        Ok(Day {
            left,
            right,
            right_count,
        })
    }
}

impl Day {
    pub async fn new(day: i32) -> Self {
        let url = format!("https://adventofcode.com/2024/day/{:02}/input", day);
        let resp = reqwest::get(&url).await.unwrap();
        let input = resp.text().await.unwrap();
        println!("{}\n{}", url, input);
        let input = "std::fs::read_to_string(path).unwrap();";
        Day::from_str(&input).unwrap()
    }
}

impl AoCDay for Day {
    fn part1(&self) -> String {
        self.left
            .iter()
            .zip(self.right.iter())
            .map(|(num1, num2)| (num1 - num2).abs())
            .sum::<i32>()
            .to_string()
    }

    fn part2(&self) -> String {
        self.left
            .iter()
            .map(|num1| (num1 * self.right_count.get(num1).unwrap_or(&0)))
            .sum::<i32>()
            .to_string()
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     #[test]
//     fn test_part1_input() {
//         let actual = Day::new(TEST_INPUT).part1();
//         assert_eq!(actual, "11");
//     }
//
//     #[test]
//     fn test_part2_input() {
//         let actual = Day::new(TEST_INPUT).part2();
//         assert_eq!(actual, "31");
//     }
//
//     #[test]
//     fn test_part1_puzzle_input() {
//         let mut day = Day::new(PUZZLE_INPUT);
//         let actual = day.part1();
//         println!("{actual}");
//         assert_eq!(true, true);
//     }
//
//     #[test]
//     fn test_part2_puzzle_input() {
//         let mut day = Day::new(PUZZLE_INPUT);
//         let actual = day.part2();
//         println!("{actual}");
//         assert_eq!(true, true);
//     }
// }
