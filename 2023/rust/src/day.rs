use std::{
    io::{self},
    str::FromStr,
};

pub trait DayTrait<Input1: FromStr, Input2: FromStr> {
    fn day_number(&self) -> &'static str;
    fn part1_input(&self) -> &'static str;
    fn part2_input(&self) -> &'static str;

    fn part1(&self, input: Input1) -> String;
    fn part2(&self, input: Input2) -> String;

    fn run(&self, part_num: usize) -> io::Result<String> {
        match part_num {
            1 => {
                let buffer = std::fs::read_to_string(format!("../input/{}", self.part1_input()))?;

                if let Ok(input) = buffer.parse::<Input1>() {
                    Ok(self.part1(input))
                } else {
                    panic!(
                        "Parsing input failed for Part1 of Day {:?}",
                        self.day_number()
                    );
                }
            }
            2 => {
                let buffer = std::fs::read_to_string(format!("../input/{}", self.part2_input()))?;

                if let Ok(input) = buffer.parse::<Input2>() {
                    Ok(self.part2(input))
                } else {
                    panic!(
                        "Parsing input failed for Part2 of Day {:?}",
                        self.day_number()
                    );
                }
            }
            _ => panic!("Only parts 1 and 2 are available!"),
        }
    }
}
