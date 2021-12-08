use core::*;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        8
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        input
            .lines()
            .fold(0_usize, |acc, l| {
                acc + l
                    .split(" | ")
                    .into_iter()
                    .skip(1)
                    .next()
                    .unwrap()
                    .split(' ')
                    .filter(|x| [2_usize, 3, 4, 7].contains(&x.len()))
                    .count()
            })
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
