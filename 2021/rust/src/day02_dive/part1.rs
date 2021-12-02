use core::*;

struct PartOne {}

impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        2
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {

        let (depth, width) = input
            .lines()
            .map(|x: &str| {
                let mut v = x.split_whitespace();
                (v.next().unwrap(), v.next().unwrap().parse::<i32>().unwrap())
            })
            .fold((0, 0), |(depth, width), (dir, dis)| match dir {
                "forward" => (depth, width + dis),
                "up" => (depth - dis, width),
                "down" => (depth + dis, width),
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
        let part = PartOne {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
