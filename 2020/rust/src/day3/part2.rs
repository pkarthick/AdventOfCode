use core::*;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        3
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        input
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test() {
        let part = PartTwo{};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
