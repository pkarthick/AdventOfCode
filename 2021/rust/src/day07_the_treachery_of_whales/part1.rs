use core::*;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        7
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let crab_positions: Vec<i32> = input
            .split(',')
            .map(|x| x.parse::<i32>().unwrap())
            .collect();
        let max = crab_positions.iter().max().unwrap();

        (0..*max)
            .map(|new_pos| {
                crab_positions.iter().fold(0, |a, pos| {
                    let cost = (new_pos - pos).abs();
                    a + cost
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
        let part = PartOne {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
