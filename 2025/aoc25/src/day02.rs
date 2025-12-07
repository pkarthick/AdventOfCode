use std::{num::ParseIntError, ops::RangeInclusive};

use crate::reader::Reader;
type CustomResult<T> = Result<T, ParseIntError>;

pub struct Input {
    ranges: Vec<RangeInclusive<u64>>,
}

#[aoc_generator(day2)]
pub fn input_generator(input: &str) -> CustomResult<Input> {
    let mut reader = Reader::new(input);

    let mut ranges: Vec<RangeInclusive<u64>> = vec![];

    while !reader.done() {
        if reader.read_if('\n') {
            break;
        }

        let mut start = 0;
        let mut end = 0;

        if let Some(s) = reader.read_int() {
            start = s as u64;
        }
        reader.read_if('-');
        if let Some(e) = reader.read_int() {
            end = e as u64;
        }

        reader.read_if(',');
        ranges.push(start..=end);
    }

    Ok(Input { ranges })
}

#[aoc(day2, part1)]
pub fn part1(input: &Result<Input, ParseIntError>) -> u64 {
    let input = input.as_ref().unwrap();

    let mut sum = 0;

    for r in input.ranges.clone() {

        let mut i = *r.start();

        while i <= *r.end() {
            
            let c = number_of_digits(i as usize) / 2;
            let half_digits = 10_u32.pow(c) as u64;

            if i % half_digits == i / half_digits {
                sum += i;
                i += 10;
            } else {
                i += 1;
            }
        }
    }

    sum
}

fn number_of_digits(i: usize) -> u32 {
    let mut i = i;
    let mut count = 0;
    while i > 0 {
        count += 1;
        i /= 10;
    }
    count
}

fn has_pattern(i: u64) -> bool {
    let slen = number_of_digits(i as usize);
    let mut digits = slen/2;

    while digits > 0 {
        if slen % digits == 0 {
            let mut part_len = digits;
            while part_len > 0 {
                if repetitive_digits(i, part_len) {
                    return true;
                }
                part_len -= 1;
            }
        }
        digits -= 1;
    }

    false
}

fn repetitive_digits(i: u64, digits_count: u32) -> bool {
    let powers_of_10 = 10_u32.pow(digits_count) as u64;
    let part = i % powers_of_10;
    let mut num = i / powers_of_10;
    
    while num > 0 {
        if num % powers_of_10 == part {
            num = num / powers_of_10;
        } else {
            return false;
        }
    }

    true
}

#[aoc(day2, part2)]
pub fn part2(input: &Result<Input, ParseIntError>) -> u64 {
    input
        .as_ref()
        .unwrap()
        .ranges
        .iter()
        .map(|r| {
            r.clone()
                .into_iter()
                .filter_map(|i| if has_pattern(i) { Some(i) } else { None })
                .sum::<u64>()
        })
        .sum()
}
