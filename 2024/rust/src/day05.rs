use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use crate::day::AoCDay;

#[derive(Debug, Clone)]
pub struct Sequence {
    page_numbers: Vec<i32>,
}

impl Sequence {
    fn is_before(&self, before: i32, after: i32, befores: &HashMap<i32, HashSet<i32>>) -> bool {
        if let Some(bs) = befores.get(&after) {
            if bs.contains(&before) {
                return true;
            }

            if let Some(bs1) = befores.get(&before) {
                if bs1.contains(&after) {
                    return false;
                }
            }

            if bs.iter().any(|b| self.is_before(before, *b, befores)) {
                return true;
            }
        }

        false
    }

    fn is_ok(&self, befores: &HashMap<i32, HashSet<i32>>) -> Result<(), usize> {
        for i in 0..self.page_numbers.len() {
            for b in self.page_numbers.iter().take(i) {
                if !self.is_before(*b, self.page_numbers[i], befores) {
                    return Err(i);
                }
            }
        }

        return Ok(());
    }

    fn get_middle(&self) -> i32 {
        self.page_numbers[self.page_numbers.len() / 2]
    }

    fn fix(&mut self, i: usize) {
        let val = self.page_numbers[i - 1];
        self.page_numbers[i - 1] = self.page_numbers[i];
        self.page_numbers[i] = val;
    }
}

#[derive(Debug)]
pub struct Day {
    befores: HashMap<i32, HashSet<i32>>,
    sequences: Vec<Sequence>,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let xs: Vec<&str> = s.split("\n\n").collect();
        let mut befores: HashMap<i32, HashSet<i32>> = HashMap::new();

        xs[0]
            .split('\n')
            .into_iter()
            .map(|s| {
                let ss: Vec<i32> = s
                    .split('|')
                    .into_iter()
                    .map(|s| s.parse::<i32>().unwrap())
                    .collect();
                (ss[0], ss[1])
            })
            .for_each(|(bef, aft)| {
                befores
                    .entry(aft)
                    .and_modify(|v| {
                        v.insert(bef);
                    })
                    .or_insert(HashSet::from([bef]));
            });

        let sequences: Vec<Sequence> = xs[1]
            .split('\n')
            .into_iter()
            .map(|s| {
                let page_numbers = s
                    .split(',')
                    .into_iter()
                    .map(|s| s.parse::<i32>().unwrap())
                    .collect();
                Sequence { page_numbers }
            })
            .collect();

        Ok(Day { sequences, befores })
    }
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        self.sequences
            .iter()
            .filter(|sequence| sequence.is_ok(&self.befores).is_ok())
            .map(|s| s.get_middle())
            .sum::<i32>()
            .to_string()
    }

    fn part2(&mut self) -> String {
        let mut total = 0;

        for sequence in self.sequences.iter_mut() {
            let mut error = false;

            while let Err(i) = sequence.is_ok(&self.befores) {
                sequence.fix(i);
                error = true;
            }

            if error {
                total += sequence.get_middle();
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
        assert_eq!(actual, "143");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(INPUT).part2();
        assert_eq!(actual, "123");
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

    const PUZZLE_INPUT: &str = include_str!("../data/day05");

    const INPUT: &str = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47";
}
