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
        let times = &self.password[..].matches(self.ch).collect::<Vec<_>>().len();
        *times >= self.min && *times <= self.max
    }
}

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
        let part = PartOne {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
