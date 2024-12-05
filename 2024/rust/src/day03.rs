use crate::day::AoCDay;
use regex::Regex;
use std::str::FromStr;

pub struct Day {
    input: String,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(Day {
            input: String::from(input),
        })
    }
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        let mut total = 0;
        let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
        for (_, [num1, num2]) in re.captures_iter(&self.input).map(|c| c.extract()) {
            if let Ok(num1) = num1.parse::<i32>() {
                if let Ok(num2) = num2.parse::<i32>() {
                    total += num1 * num2;
                }
            }
        }
        total.to_string()
    }

    fn part2(&mut self) -> String {
        let mut total = 0;

        let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();

        for s in self.input.split("do()") {
            let ss = if let Some(ind) = s.find("don\'t()") {
                s.split_at(ind).0
            } else {
                s
            };

            for (_, [num1, num2]) in re.captures_iter(&ss).map(|c| c.extract()) {
                if let Ok(num1) = num1.parse::<i32>() {
                    if let Ok(num2) = num2.parse::<i32>() {
                        total += num1 * num2;
                    }
                }
            }
        }

        total.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const PUZZLE_INPUT: &str = include_str!("../data/day03");

    #[test]
    fn test_part1_input() {
        let actual = Day::new(TEST_INPUT).part1();
        assert_eq!(actual, "161");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(TEST_INPUT).part2();
        assert_eq!(actual, "48");
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

    const TEST_INPUT: &str =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
}
