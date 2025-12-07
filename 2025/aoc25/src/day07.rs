use std::{
    collections::{HashMap, HashSet},
    num::ParseIntError,
    str::FromStr,
};

type CustomResult<T> = Result<T, ParseIntError>;

#[derive(Debug, Clone, PartialEq)]
enum Cell {
    Empty,
    Splitter,
}

#[derive(Debug, Clone)]
pub struct Input {
    rows: Vec<Vec<Cell>>,
    start: (usize, usize),
}

impl Input {
    fn count_splitters(&self) -> usize {
        let sr = self.start.0;
        let sc = self.start.1;

        let mut pending_splitters = vec![(sr, sc)];
        let mut distinct_splitters = HashSet::new();
        let mut existing_splitters = HashSet::new();

        let mut index = 0;

        while index < pending_splitters.len() {
            let (r,c) = pending_splitters[index];

            if self.rows[r][c] == Cell::Empty {
                if r + 1 < self.rows.len() {
                    pending_splitters.push((r + 1, c));
                }
            } else if self.rows[r][c] == Cell::Splitter {
                distinct_splitters.insert((r, c));
                if c > 0 {
                    if !existing_splitters.contains(&(r, c - 1)) {
                        pending_splitters.push((r, c - 1));
                        existing_splitters.insert((r, c - 1));
                    }
                }
                if c < self.rows[r].len() - 1 {
                    if !existing_splitters.contains(&(r, c + 1)) {
                        pending_splitters.push((r, c + 1));
                        existing_splitters.insert((r, c + 1));
                    }
                }
            }

            index += 1;
            
        }

        distinct_splitters.len()
    }

    fn count_timeline(
        &self,
        (r, c): (usize, usize),
        counter: &mut HashMap<(usize, usize), usize>,
    ) -> usize {
        if let Some(count) = counter.get(&(r, c)) {
            *count
        } else {
            let mut count = 0;

            if self.rows[r][c] == Cell::Empty {
                if r + 1 < self.rows.len() {
                    count += self.count_timeline((r + 1, c), counter);
                } else {
                    count += 1;
                }
            } else {
                count += self.count_timeline((r, c - 1), counter);
                count += self.count_timeline((r, c + 1), counter);
            }

            counter.insert((r, c), count);

            count
        }
    }

    fn count_timelines(&self) -> usize {
        let mut counter = HashMap::new();
        self.count_timeline(self.start, &mut counter)
    }
}

impl FromStr for Input {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut rows: Vec<Vec<Cell>> = vec![];
        let mut sr = 0;
        let mut sc = 0;

        for (r, l) in s.lines().enumerate() {
            let mut row = vec![];
            for (c, ch) in l.chars().enumerate() {
                let cell = match ch {
                    '.' => Cell::Empty,
                    'S' => {
                        sr = r;
                        sc = c;
                        Cell::Empty
                    }
                    '^' => Cell::Splitter,
                    _ => panic!(),
                };
                row.push(cell);
            }
            rows.push(row);
        }
        Ok(Input {
            rows,
            start: (sr, sc),
        })
    }
}

#[aoc_generator(day7)]
pub fn input_generator(input: &str) -> CustomResult<Input> {
    Ok(Input::from_str(input).unwrap())
}

#[aoc(day7, part1)]
pub fn part1(input: &Result<Input, ParseIntError>) -> usize {
    let input = input.as_ref().unwrap();
    input.count_splitters()
}

#[aoc(day7, part2)]
pub fn part2(input: &Result<Input, ParseIntError>) -> usize {
    let input = input.clone().unwrap();
    input.count_timelines()
}
