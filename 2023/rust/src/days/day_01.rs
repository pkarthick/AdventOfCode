use std::{collections::HashSet, str::FromStr};

pub struct Input {
    calibrations: Vec<Calibration>,
}

pub struct Calibration {
    info: String,
}

impl Calibration {
    fn new(s: String) -> Self {
        Calibration { info: s }
    }

    fn value(&self) -> u32 {
        let digits: Vec<u32> = self
            .info
            .chars()
            .filter(|c| c.is_digit(10))
            .map(|c| c.to_digit(10).unwrap())
            .collect();
        digits[0] * 10 + digits.last().unwrap()
    }

    fn value2(&self) -> u32 {
        let mut digit_and_index_vec: HashSet<(usize, u32)> = HashSet::new();
        let string_digits: Vec<&str> = vec![
            "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        ];

        for (ch, d) in ('0'..='9').into_iter().zip(0_u32..=9) {
            if let Some(index) = self.info.find(ch) {
                digit_and_index_vec.insert((index, d));
            }
            if let Some(index) = self.info.rfind(ch) {
                digit_and_index_vec.insert((index, d));
            }
        }

        for (s, d) in string_digits.into_iter().zip(1_u32..=9) {
            if let Some(index) = self.info.find(s) {
                digit_and_index_vec.insert((index, d));
            }
            if let Some(index) = self.info.rfind(s) {
                digit_and_index_vec.insert((index, d));
            }
        }

        let mut digit_and_index_vec: Vec<_> = digit_and_index_vec.into_iter().collect();
        digit_and_index_vec.sort();

        (digit_and_index_vec.first().unwrap().1 * 10) + digit_and_index_vec.last().unwrap().1
    }
}

impl Input {
    fn new(s: &str) -> Self {
        let calibrations = s.split("\n").map(|s| Calibration::new(s.into())).collect();
        Self { calibrations }
    }
}

impl FromStr for Input {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Input::new(s))
    }
}

pub struct Day {}

impl crate::day::DayTrait<Input, Input> for Day {
    fn day_number(&self) -> &'static str {
        "01"
    }

    fn part1_input(&self) -> &'static str {
        "day01"
    }

    fn part2_input(&self) -> &'static str {
        "day01"
    }

    fn part1(&self, input: Input) -> String {
        let total: u32 = input.calibrations.iter().map(|c| c.value()).sum();
        total.to_string()
    }

    fn part2(&self, input: Input) -> String {
        let total: u32 = input.calibrations.iter().map(|c| c.value2()).sum();
        total.to_string()
    }
}
