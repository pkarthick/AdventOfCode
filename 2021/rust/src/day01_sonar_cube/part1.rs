use core::*;

struct PartOne {}

impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        1
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

fn count_increases(depths: Vec<i32>) -> usize {
    depths
    .iter()
    .zip(&depths[1..])
    .filter(|(x, y)| x < y)
    .count()
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let depths: Vec<i32> = input.lines().map(|x| x.parse::<i32>().unwrap()).collect();
        let count = count_increases(depths);
        count.to_string()
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
