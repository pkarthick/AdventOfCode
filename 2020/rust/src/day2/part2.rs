struct PasswordInfo {
    min: usize,
    max: usize,
    ch: char,
    password: String
}

fn create_input(s: String) -> Vec<PasswordInfo> {

    s.split('\n').map(|s| {
        let v: Vec<&str> = s.trim().split(&['-', ' ', ':'][..]).collect();
        PasswordInfo {
            min: v[0].parse::<usize>().unwrap(),
            max: v[1].parse::<usize>().unwrap(),
            ch: v[2].chars().nth(0).unwrap(),
            password: v[4].into(),
        }
    }).collect::<Vec<PasswordInfo>>()

}

fn process_input(passwords: Vec<PasswordInfo>) -> Result<usize, String> {
    let count = passwords.into_iter().filter(|pi| {
        let instances = &pi.password[..];
        let min = instances.chars().nth(pi.min-1).unwrap();
        let max = instances.chars().nth(pi.max-1).unwrap();

        (min == pi.ch && max != pi.ch) || (min != pi.ch && max == pi.ch)
        
    }).count();
    Ok(count)
}

#[cfg(test)]
mod tests {
    use crate::core::*;

    use super::*;

    #[test]
    fn test() {
        let part = Part::new(2, PartKind::Two, create_input, process_input);
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }    
}
