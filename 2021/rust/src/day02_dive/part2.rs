use core::*;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        2
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        
        let (depth, width, _) = input
            .lines()
            .map(|x: &str| {
                let mut v = x.split_whitespace();
                (v.next().unwrap(), v.next().unwrap().parse::<i32>().unwrap())
            })
            .fold((0, 0, 0), |(depth, width, aim), (dir, dis)| match dir {
                "forward" => (depth + (aim * dis), width + dis, aim),
                "up" => (depth, width, aim - dis),
                "down" => (depth, width, aim + dis),
                _ => panic!("Unexpected direction!"),
            });

        (depth * width).to_string()
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
