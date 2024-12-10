use std::str::FromStr;

use crate::day::AoCDay;

pub struct Day {
    equations: Vec<Equation>,
}
pub struct Equation {
    test_value: i64,
    nums: Vec<i64>,
}

impl Equation {
    fn is_ok(&self, result: i64, index: usize) -> bool {
        if result == self.test_value {
            return true;
        }

        if result > self.test_value {
            return false;
        }

        match self.nums.len() - index {
            0 => result == self.test_value,
            1 if self.nums[index] * result == self.test_value => true,
            1 if self.nums[index] + result == self.test_value => true,

            _ => {
                if self.is_ok(self.nums[index] * result, index + 1) {
                    return true;
                }

                if self.is_ok(self.nums[index] + result, index + 1) {
                    return true;
                }

                false
            }
        }
    }

    fn is_ok_2(&self, result: i64, index: usize) -> bool {
        if result == self.test_value {
            return true;
        } else if result > self.test_value {
            return false;
        } else if index < self.nums.len() {
            if self.is_ok_2(self.nums[index] * result, index + 1) {
                return true;
            }

            if self.is_ok_2(self.nums[index] + result, index + 1) {
                return true;
            }

            let s = result.to_string() + &self.nums[index].to_string();
            if let Ok(concat_val) = s.parse::<i64>() {
                if self.is_ok_2(concat_val, index + 1) {
                    return true;
                }
            }

            false
        } else {
            false
        }
    }
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut equations = vec![];

        for line in s.split("\n") {
            let pair: Vec<&str> = line.split(":").collect();

            let test_value = pair[0].parse::<i64>().unwrap();
            let nums: Vec<i64> = pair[1]
                .split_ascii_whitespace()
                .map(|n| n.parse::<i64>().unwrap())
                .collect();
            equations.push(Equation { test_value, nums });
        }
        Ok(Day { equations })
    }
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        let total: i64 = self.equations.iter().filter(|equation| equation.is_ok(equation.nums[0], 1)).map(|e| e.test_value).sum();
        total.to_string()
    }

    fn part2(&mut self) -> String {
        let total: i64 = self.equations.iter().filter(|equation| equation.is_ok_2(equation.nums[0], 1)).map(|e| e.test_value).sum();
        total.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1_input() {
        let actual = Day::new(INPUT).part1();
        assert_eq!(actual, "3749");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(INPUT).part2();
        assert_eq!(actual, "11387");
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

    const PUZZLE_INPUT: &str = include_str!("../data/day07");

    const INPUT: &str = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20";
}
