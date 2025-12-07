use std::{num::ParseIntError, ops::RangeInclusive, str::FromStr};

use crate::reader::Reader;

type CustomResult<T> = Result<T, ParseIntError>;

#[derive(Debug, Clone)]
pub struct Input {
    ranges: Vec<RangeInclusive<usize>>,
    available: Vec<usize>,
}

impl Input {
    fn count_fresh_ingredients(&self) -> usize {
        let mut count = 0;
        for id in self.available.iter() {
            if self.ranges.iter().any(|r| r.contains(id)) {
                count += 1;
            }
        }
        count
    }

    fn get_exclusive_range(
        ranges: &Vec<RangeInclusive<usize>>,
        start: usize,
        end: usize,
    ) -> Option<RangeInclusive<usize>> {

        let mut start = start;
        let mut end = end;

        for r in ranges.iter() {
            if r.contains(&start) {
                if r.contains(&end) {
                    return None;
                } else {
                    start = *r.end() + 1;
                    break;
                }
            } else if r.contains(&end) {
                end = *r.start() - 1;
                break;
            } 
        }

        Some(start..=end)
    }

    fn new_fresh(&mut self) -> usize {
        let mut has_changed = true;

        loop {
            let mut new_ranges = vec![];

            for r in self.ranges.drain(..) {
                let start = *r.start();
                let end = *r.end();
                if let Some(opt) = Self::get_exclusive_range(&new_ranges, start, end) {
                    
                    if !has_changed {
                        has_changed = *opt.start() != start || *opt.end() != end;
                    }

                    new_ranges.push(opt);
                }
            }

            self.ranges.extend(new_ranges);

            if !has_changed {
                break;
            }

            has_changed = false;
        }

        self.ranges.iter().map(|r| r.end() - r.start() + 1).sum()
    }
}

impl FromStr for Input {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut ranges: Vec<RangeInclusive<usize>> = vec![];
        let mut available: Vec<usize> = vec![];

        let mut reader: Reader = Reader::new(s);

        while !reader.done() {
            let start = reader.read_int().unwrap() as usize;
            reader.read_if('-');
            let end = reader.read_int().unwrap() as usize;
            reader.read_if('\n');
            ranges.push(start..=end);

            if reader.read_if('\n') {
                break;
            }
        }

        while !reader.done() {
            let id = reader.read_int().unwrap() as usize;
            available.push(id);
            reader.read_if('\n');
        }

        ranges.sort_by(|r1, r2| r1.start().cmp(r2.start()));

        Ok(Self { ranges, available })
    }
}

#[aoc_generator(day5)]
pub fn input_generator(input: &str) -> CustomResult<Input> {
    Ok(Input::from_str(input).unwrap())
}

#[aoc(day5, part1)]
pub fn part1(input: &Result<Input, ParseIntError>) -> usize {
    let input = input.as_ref().unwrap();
    input.count_fresh_ingredients()
}

#[aoc(day5, part2)]
pub fn part2(input: &Result<Input, ParseIntError>) -> usize {
    let mut input = input.clone().unwrap();
    input.new_fresh()
}
