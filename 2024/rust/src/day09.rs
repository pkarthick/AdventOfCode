use std::str::FromStr;

use crate::day::AoCDay;

pub struct Day {}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let _ = s;
        Ok(Day {})
    }
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        println!("Part 1 not implemented");
        "".to_string()
    }

    fn part2(&mut self) -> String {
        println!("Part 2 not implemented");
        "".to_string()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1_input() {
        let actual = Day::new(INPUT).part1();
        assert_eq!(actual, "");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(INPUT).part2();
        assert_eq!(actual, "");
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

    const PUZZLE_INPUT: &str = include_str!("../data/day09");

    const INPUT: &str = "";
}