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
        fn get_tuple(x: &str) -> (&str, i32) {
            let v: Vec<&str> = x.split_ascii_whitespace().collect();
            (&v[0], v[1].parse::<i32>().unwrap())
        }

        let dirs: Vec<(&str, i32)> = input.lines().map(get_tuple).collect();

        let (depth, width, _) = dirs.iter().fold((0, 0, 0), |(depth, width, aim), (dir, dis)| match *dir {
            "forward" => (depth + (aim * dis), width + dis, aim),
            "up" => (depth, width, aim - dis),
            "down" => (depth, width, aim+ dis),
            _ => panic!("Unexpected direction!")
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
