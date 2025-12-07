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
        for i in r {
            let s = i.to_string();
            let slen = s.len() as u32;
            let c = slen / 2;

            let half_digits = 10_u32.pow(c) as u64;

            if i / half_digits == i % half_digits {
                sum += i;
            }
        }
    }

    sum
}

fn has_pattern(i: u64) -> bool {
    let slen = i.to_string().len();

    for digits in (1..=(slen / 2)).into_iter().rev() {
        if slen % digits == 0 {
            for part_len in (1..=digits).into_iter().rev() {
                let part_digits = 10_u32.pow(part_len as u32) as u64;

                let part = i % part_digits;

                let mut num = i / part_digits;
                let mut pattern = true;

                while num > 0 {
                    if num % part_digits == part {
                        num = num / part_digits;
                    } else {
                        pattern = false;
                        break;
                    }
                }

                if pattern {
                    return true;
                }
            }
        }
    }

    true
}

#[aoc(day2, part2)]
pub fn part2(input: &Result<Input, ParseIntError>) -> u64 {
    let input = input.as_ref().unwrap();

    let mut sum = 0;

    for r in input.ranges.clone() {
        // let mut i = *r.start();

        for i in r {

            if has_pattern(i) {
                sum += i;
            }

            // let slen = i.to_string().len();

            // 'outer: for digits in (1..=(slen / 2)).into_iter().rev() {
            //     if slen % digits == 0 {
            //         for part_len in (1..=digits).into_iter().rev() {
            //             let part_digits = 10_u32.pow(part_len as u32) as u64;

            //             let part = i % part_digits;

            //             let mut num = i / part_digits;
            //             let mut pattern = true;

            //             while num > 0 {
            //                 if num % part_digits == part {
            //                     num = num / part_digits;
            //                 } else {
            //                     pattern = false;
            //                     break;
            //                 }
            //             }

            //             if pattern {
            //                 sum += i;
            //                 break 'outer;
            //             }
            //         }
            //     }
            // }

        }
    }

    sum
}
