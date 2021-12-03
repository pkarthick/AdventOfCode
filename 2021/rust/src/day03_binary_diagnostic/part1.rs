use core::*;
use std::num;

struct PartOne {}

impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        3
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let lines = input.lines().collect::<Vec<&str>>();
        let number_of_bits = lines[0].len();
        let total = lines.len();

        let (gamma, epsilon) = (0..number_of_bits)
            .into_iter()
            .fold((0,0),|(o, z), pos| {
                let ones = lines
                    .iter()
                    .filter(move |l| l.chars().nth(pos).unwrap() == '1')
                    .count();
                let zeroes = total - ones; 
                let p = 1 << number_of_bits - pos - 1;
                if ones > zeroes { (o + p, z) } else { (o, z+p) }
            });

        (gamma * epsilon).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let part = PartOne {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
