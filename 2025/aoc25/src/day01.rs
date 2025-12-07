use std::num::ParseIntError;

use crate::reader::Reader;
type CustomResult<T> = Result<T, ParseIntError>;

enum Direction {
    Left,
    Right,
}

struct Instruction {
    dir: Direction,
    times: i64,
}

pub struct Input {
    instructions: Vec<Instruction>,
}

#[aoc_generator(day1)]
pub fn input_generator(input: &str) -> CustomResult<Input> {
    let mut reader = Reader::new(input);

    let mut instructions: Vec<Instruction> = vec![];

    while !reader.done() {
        if let Some(d) = reader.read_char() {
            if d == 'L' {
                if let Some(c) = reader.read_int() {
                    instructions.push(Instruction {
                        dir: Direction::Left,
                        times: c,
                    });
                }
            } else if d == 'R' {
                if let Some(c) = reader.read_int() {
                    instructions.push(Instruction {
                        dir: Direction::Right,
                        times: c,
                    });
                }
            }
        } else {
        }

        reader.read_if('\n');
    }

    Ok(Input { instructions })
}

#[aoc(day1, part1)]
pub fn part1(input: &Result<Input, ParseIntError>) -> i64 {
    let input = input.as_ref().unwrap();
    let mut zero_count = 0;
    let mut pos = 50;

    for ins in input.instructions.iter() {
        let times = ins.times % 100;

        match ins.dir {
            Direction::Left => {
                match times.cmp(&pos) {
                    std::cmp::Ordering::Less => {
                        pos -= times;
                    }
                    std::cmp::Ordering::Equal => {
                        zero_count += 1;
                        pos = 0;
                    }
                    std::cmp::Ordering::Greater => {
                        pos += 100;
                        pos -= times;
                    }
                }

            }
            Direction::Right => {
                let rem = 100 - pos;
                match times.cmp(&rem) {
                    std::cmp::Ordering::Less => pos += times,
                    std::cmp::Ordering::Equal => {
                        zero_count += 1;
                        pos = 0;
                    }
                    std::cmp::Ordering::Greater => {
                        pos = times - rem;
                    }
                }
            }
        }
    }

    zero_count
}

#[aoc(day1, part2)]
pub fn part2(input: &Result<Input, ParseIntError>) -> i64 {
    let input = input.as_ref().unwrap();

    let mut zero_count = 0;
    let mut pos = 50;

    for ins in input.instructions.iter() {
        let times = ins.times % 100;

        zero_count += ins.times / 100;

        match ins.dir {
            Direction::Left => {
                match times.cmp(&pos) {
                    std::cmp::Ordering::Less => {
                        pos -= times;
                    }
                    std::cmp::Ordering::Equal => {
                        zero_count += 1;
                        pos = 0;
                    }
                    std::cmp::Ordering::Greater => {
                        if pos > 0 {
                            zero_count += 1;
                        }
                        pos += 100;
                        pos -= times;
                    }
                }

            }
            Direction::Right => {
                let rem = 100 - pos;
                match times.cmp(&rem) {
                    std::cmp::Ordering::Less => {
                        pos += times
                    },
                    std::cmp::Ordering::Equal => {
                        zero_count += 1;
                        pos = 0;
                    }
                    std::cmp::Ordering::Greater => {
                        pos = times - rem;
                        zero_count+=1;
                    }
                }
            }
        }
    }

    zero_count
}
