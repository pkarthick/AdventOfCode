use std::{num::ParseIntError, str::FromStr};

type CustomResult<T> = Result<T, ParseIntError>;

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Multiply,
}

struct Column {
    nums: Vec<usize>,
    operator: Operator,
}

pub struct Input {
    columns: Vec<Column>,
}

impl FromStr for Input {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines: Vec<String> = s.lines().map(|s| s.to_string()).collect();

        let operators = lines.last().unwrap();

        let operators: Vec<Operator> = operators
            .split_ascii_whitespace()
            .map(|s| match s {
                "*" => Operator::Multiply,
                "+" => Operator::Add,
                _ => panic!(),
            })
            .collect();

        let mut columns_data: Vec<Vec<usize>> = vec![];

        for _ in 0..operators.len() {
            columns_data.push(vec![]);
        }

        for line in &lines[0..lines.len() - 1] {
            for (word_index, word) in line.split_ascii_whitespace().enumerate() {
                let num: usize = word.parse().unwrap();
                columns_data[word_index].push(num);
            }
        }

        let mut columns = vec![];

        for (c, operator) in operators.iter().enumerate() {
            let column = Column {
                operator: *operator,
                nums: columns_data[c].clone(),
            };
            columns.push(column);
        }

        Ok(Self { columns })
    }
}

pub struct Input2 {
    columns: Vec<Column>,
}

impl FromStr for Input2 {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines: Vec<String> = s.lines().map(|s| s.to_string()).collect();

        let operators = lines.last().unwrap();

        let operators: Vec<Operator> = operators
            .split_ascii_whitespace()
            .map(|s| match s {
                "*" => Operator::Multiply,
                "+" => Operator::Add,
                _ => panic!(),
            })
            .collect();

        let mut columns_data: Vec<Vec<usize>> = vec![];

        // for _ in 0..operators.len() {
        //     columns_data.push(vec![]);
        // }

        let mut words = vec![];
        
        for i in 0..lines[0].len() {
            let mut s = String::new();
            for line in &lines[0..lines.len() - 1] {
                match line.chars().nth(i) {
                    Some(ch) => s.push(ch),
                    None => todo!(),
                }
            }

            let s = s.trim();

            if s.trim().is_empty() {
                columns_data.push(words.clone());
                words.clear();
            } else {
                words.push(s.parse::<usize>().unwrap());
            }
        }

        columns_data.push(words);

        let mut columns = vec![];

        for (c, operator) in operators.iter().enumerate() {
            let column = Column {
                operator: *operator,
                nums: columns_data[c].clone(),
            };
            columns.push(column);
        }

        Ok(Self { columns })
    }
}

#[aoc_generator(day6, part1)]
pub fn input_generator(input: &str) -> CustomResult<Input> {
    Ok(Input::from_str(input).unwrap())
}

#[aoc_generator(day6, part2)]
pub fn input_generator2(input: &str) -> CustomResult<Input2> {
    Ok(Input2::from_str(input).unwrap())
}

#[aoc(day6, part1)]
pub fn part1(input: &Result<Input, ParseIntError>) -> usize {
    let input = input.as_ref().unwrap();

    let mut sum = 0;

    for column in input.columns.iter() {
        sum += match column.operator {
            Operator::Add => column.nums.iter().sum(),
            Operator::Multiply => column.nums.iter().fold(1, |acc, b| acc * *b),
        }
    }

    sum
}

#[aoc(day6, part2)]
pub fn part2(input: &Result<Input2, ParseIntError>) -> usize {
    let input = input.as_ref().unwrap();

    let mut sum = 0;

    for column in input.columns.iter() {
        sum += match column.operator {
            Operator::Add => column.nums.iter().sum(),
            Operator::Multiply => column.nums.iter().fold(1, |acc, b| acc * *b),
        }
    }

    sum
}
