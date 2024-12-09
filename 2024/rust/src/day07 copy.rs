use std::{
    collections::{HashSet, VecDeque},
    str::FromStr,
};

use crate::day::AoCDay;

pub struct Day {
    equations: Vec<Equation>,
}
pub struct Equation {
    test_value: i64,
    nums: VecDeque<i64>,
}

impl Equation {
    fn is_ok(&self) -> bool {
        match self.nums.len() {
            0 => false,
            1 => self.nums[0] == self.test_value,
            2 if self.nums[0] * self.nums[1] == self.test_value => true,
            2 if self.nums[0] + self.nums[1] == self.test_value => true,
           
            _ => {
                let mut nums = self.nums.clone();
                if let Some(first) = nums.pop_front() {
                    nums[0] *= first;

                    let eq = Equation {
                        test_value: self.test_value,
                        nums,
                    };
                    if eq.is_ok() {
                        return true;
                    }
                }

                let mut nums = self.nums.clone();
                if let Some(first) = nums.pop_front() {
                    nums[0] += first;
                    let eq = Equation {
                        test_value: self.test_value,
                        nums,
                    };
                    if eq.is_ok() {
                        return true;
                    }
                }

                false
            }
        }
    }
    fn is_ok_2(&self) -> bool {
        match self.nums.len() {
            0 => false,
            1 => self.nums[0] == self.test_value,
                        
            _ => {
                let mut nums = self.nums.clone();
                if let Some(first) = nums.pop_front() {
                    nums[0] *= first;

                    let eq = Equation {
                        test_value: self.test_value,
                        nums,
                    };
                    if eq.is_ok_2() {
                        return true;
                    }
                }

                let mut nums = self.nums.clone();
                if let Some(first) = nums.pop_front() {
                    nums[0] += first;
                    let eq = Equation {
                        test_value: self.test_value,
                        nums,
                    };
                    if eq.is_ok_2() {
                        return true;
                    }
                }

                let mut nums = self.nums.clone();
                if let Some(first) = nums.pop_front() {
                    let snum = first.to_string() + &nums[0].to_string();
                    if let Ok(concat_val) = snum.parse::<i64>() {
                        nums[0] = concat_val;
                        let eq = Equation {
                            test_value: self.test_value,
                            nums,
                        };

                        if eq.is_ok_2() {
                            return true;
                        }
                    }
                }

                false
            }
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
            let nums: VecDeque<i64> = pair[1]
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
        let mut total = 0;

        for equation in self.equations.iter() {
            if equation.is_ok() {
                total += equation.test_value;
            }
        }

        total.to_string()
    }


    fn part2(&mut self) -> String {
        let mut total = 0;

        for equation in self.equations.iter() {
            if equation.is_ok_2() {
                total += equation.test_value;
            }
        }

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
