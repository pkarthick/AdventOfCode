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
        fn get_tuple(x: &str) -> (&str, i32) {
            let v: Vec<&str> = x.split_ascii_whitespace().collect();
            (&v[0], v[1].parse::<i32>().unwrap())
        }

        let dirs: Vec<(&str, i32)> = input.lines().map(get_tuple).collect();

        let (depth, width) = dirs
            .iter()
            .fold((0, 0), |(depth, width), (dir, dis)| match *dir {
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
