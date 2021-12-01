use core::*;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        1
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

fn count_increases(depths: Vec<i32>) -> usize {
    depths
        .iter()
        .zip(&depths[1..])
        .filter(|(x, y)| x < y)
        .count()
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let depths: Vec<i32> = input.lines().map(|x| x.parse::<i32>().unwrap()).collect();

        let depths: Vec<i32> = depths
            .iter()
            .zip(&depths[1..])
            .map(|(x, y)| x + y)
            .zip(&depths[2..])
            .map(|(x, y)| x + y)
            .collect();

        let count = count_increases(depths);
        count.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let part = PartTwo {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
