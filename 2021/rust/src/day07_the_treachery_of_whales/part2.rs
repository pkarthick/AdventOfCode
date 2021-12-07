use core::*;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        7
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let crab_positions: Vec<i32> = input
            .split(',')
            .map(|x| x.parse::<i32>().unwrap())
            .collect();
        let max = crab_positions.iter().max().unwrap();

        let costs: Vec<i32> = (0..=*max)
            .scan(0, |state, x| {
                *state += x;
                Some(*state)
            })
            .collect();

        (0..*max)
            .map(|new_pos| {
                crab_positions.iter().fold(0, |a, pos| {
                    let d = (new_pos - pos).abs() as usize;
                    a + costs[d]
                })
            })
            .min()
            .unwrap()
            .to_string()
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
