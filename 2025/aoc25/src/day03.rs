use std::{cmp::max, num::ParseIntError};


type CustomResult<T> = Result<T, ParseIntError>;

pub struct Input {
    banks: Vec<String>,
}

#[aoc_generator(day3)]
pub fn input_generator(input: &str) -> CustomResult<Input> {
    let banks: Vec<String> = input.lines().map(|s| s.to_string()).collect();
    Ok(Input { banks })
}

#[aoc(day3, part1)]
pub fn part1(input: &Result<Input, ParseIntError>) -> i64 {
   let input = input.as_ref().unwrap();

    let mut sum = 0;

    for bank in input.banks.iter() {
        let digits: Vec<u8> = bank.chars().into_iter().map(|c| c as u8 - 48).collect();

        let (mut d1, mut d2, si) = if digits[0] > digits[1] { (digits[0], digits[1], 2) } else { (digits[1], digits[2], 3) };
        
        for i in si..digits.len() {
            match digits[i].cmp(&d1) {
                std::cmp::Ordering::Less => d2 = max::<u8>(digits[i], d2),
                std::cmp::Ordering::Equal => d2 = max::<u8>(digits[i], d2),
                std::cmp::Ordering::Greater => {
                    if i < digits.len() - 1 {
                        d1 = digits[i];
                        d2 = digits[i+1]
                    } else {
                        d2 = max::<u8>(digits[i], d2)
                    }
                }
            }
        }

        let joltage = d1 * 10 + d2;
        sum += joltage as i64;

    }

    sum
}

fn max_digit_index(digits: &[u8], start: usize, rem: usize) -> usize {

    let mut max_index = start;

    for (index, d) in digits.iter().enumerate().skip(start) {
        if index < digits.len() - rem {
            if *d > digits[max_index] {
                max_index = index;
            }
        } else {
            break;
        }
    }

    return max_index;

}

#[aoc(day3, part2)]
pub fn part2(input: &Result<Input, ParseIntError>) -> i64 {
    let input = input.as_ref().unwrap();

    let mut sum = 0;

    for bank in input.banks.iter() {
        
        let digits: Vec<u8> = bank.chars().into_iter().map(|c| c as u8 - 48).collect();
        
        let mut index = max_digit_index(&digits, 0, 11);
        let mut indices: Vec<usize> = vec![index];

        while indices.len() < 12 {
            index = max_digit_index(&digits, index+1, 11-indices.len());
            indices.push(index);
        }

        let mut joltage = 0 as u64;

        for index in indices {
            joltage = joltage * 10 + digits[index] as u64;
        }

        sum += joltage as i64;

    }

    sum
}
