use core::*;

struct PasswordInfo {
    min: usize,
    max: usize,
    ch: char,
    password: String,
}

impl PasswordInfo {
    fn new(s: &str) -> Self {
        let v: Vec<&str> = s.trim().split(&['-', ' ', ':'][..]).collect();
        PasswordInfo {
            min: v[0].parse::<usize>().unwrap(),
            max: v[1].parse::<usize>().unwrap(),
            ch: v[2].chars().nth(0).unwrap(),
            password: v[4].into(),
        }
    }

    fn is_valid(&self) -> bool {
        let instances = &self.password[..];
        let min = instances.chars().nth(self.min - 1).unwrap();
        let max = instances.chars().nth(self.max - 1).unwrap();

        (min == self.ch && max != self.ch) || (min != self.ch && max == self.ch)
    }
}

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
        let passwords: Vec<PasswordInfo> = input.split('\n').map(PasswordInfo::new).collect();

        passwords
            .into_iter()
            .filter(|pi| pi.is_valid())
            .count()
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
