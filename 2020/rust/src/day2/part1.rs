struct PasswordInfo {
    min: usize,
    max: usize,
    ch: String,
    password: String
}

fn create_input(s: String) -> Vec<PasswordInfo> {

    s.split('\n').map(|s| {
        let v: Vec<&str> = s.trim().split(&['-', ' ', ':'][..]).collect();
        PasswordInfo {
            min: v[0].parse::<usize>().unwrap(),
            max: v[1].parse::<usize>().unwrap(),
            ch: v[2].into(),
            password: v[4].into(),
        }
    }).collect::<Vec<PasswordInfo>>()

}

fn process_input(passwords: Vec<PasswordInfo>) -> Result<usize, String> {
    let count = passwords.into_iter().filter(|pi| {
        let times = &pi.password[..].matches(&pi.ch).collect::<Vec<_>>().len();
        *times >= pi.min && *times <= pi.max
    }).count();
    Ok(count)
}

#[cfg(test)]
mod tests {
    use crate::core::*;

    use super::*;

    #[test]
    fn test() {
        let part = Part::new(2, PartKind::One, create_input, process_input);
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
