use std::num::ParseIntError;

type CustomResult<T> = Result<T, ParseIntError>;

#[derive(Clone)]
pub struct Input {
    rolls: Vec<Vec<bool>>,
}

impl Input {
    fn is_roll_accessible(&self, r: usize, c: usize) -> bool {
        if self.rolls[r][c] {
            let rc = self.rolls.len() as i32;
            let cc = self.rolls[r].len() as i32;
            let r = r as i32;
            let c = c as i32;

            let positions = [
                (r - 1, c - 1),
                (r - 1, c),
                (r - 1, c + 1),
                (r, c - 1),
                (r, c + 1),
                (r + 1, c - 1),
                (r + 1, c),
                (r + 1, c + 1),
            ];
            let mut count = 0;

            for (r1, c1) in positions {
                if r1 >= 0 && c1 >= 0 && r1 < rc && c1 < cc && self.rolls[r1 as usize][c1 as usize]
                {
                    count += 1;
                }
            }

            count < 4
        } else {
            false
        }
    }

    fn remove_rolls(&mut self) -> i64 {
        let mut roll_removed = true;
        let mut removed = 0_i64;
        loop {
            if !roll_removed {
                break;
            }
            roll_removed = false;
            for r in 0..self.rolls.len() {
                for c in 0..self.rolls[r].len() {
                    if self.rolls[r][c] {
                        let rc = self.rolls.len() as i32;
                        let cc = self.rolls[r].len() as i32;
                        let r = r as i32;
                        let c = c as i32;

                        let positions = [
                            (r - 1, c - 1),
                            (r - 1, c),
                            (r - 1, c + 1),
                            (r, c - 1),
                            (r, c + 1),
                            (r + 1, c - 1),
                            (r + 1, c),
                            (r + 1, c + 1),
                        ];
                        let mut count = 0;

                        for (r1, c1) in positions {
                            if r1 >= 0
                                && c1 >= 0
                                && r1 < rc
                                && c1 < cc
                                && self.rolls[r1 as usize][c1 as usize]
                            {
                                count += 1;
                            }
                        }

                        self.rolls[r as usize][c as usize] = !(count < 4);

                        if !roll_removed {
                            roll_removed = count < 4;
                            
                        }

                        removed += if count < 4 { 1 } else {0};

                    }
                }
            }
        }
        removed
    }
}

#[aoc_generator(day4)]
pub fn input_generator(input: &str) -> CustomResult<Input> {
    let rolls: Vec<Vec<bool>> = input
        .lines()
        .map(|s| s.chars().map(|c| c == '@').collect())
        .collect();
    Ok(Input { rolls })
}

#[aoc(day4, part1)]
pub fn part1(input: &Result<Input, ParseIntError>) -> i64 {
    let input = input.as_ref().unwrap();
    let mut count = 0;

    for r in 0..input.rolls.len() {
        for c in 0..input.rolls[r].len() {
            if input.is_roll_accessible(r, c) {
                count += 1;
            }
        }
    }

    count
}

#[aoc(day4, part2)]
pub fn part2(input: &Result<Input, ParseIntError>) -> i64 {
    let mut input = input.clone().unwrap();
    input.remove_rolls()
}
