use std::cmp::Ordering;

use crate::core::*;

struct Input {
    vec: Vec<i32>,
    total: i32,
}

fn pair(v: &[i32], total: i32) -> Option<i32> {
    let mut s = 0;
    let mut e = v.len() - 1;

    while s < e {
        match (v[s] + v[e]).cmp(&total) {
            Ordering::Equal => return Some(v[s] * v[e]),
            Ordering::Less => s += 1,
            Ordering::Greater => e -= 1,
        }
    }

    None
}

fn create_input(s: String) -> Input {
    Input {
        vec: to_vec(&s, Some(true)),
        total: 2020,
    }
}

fn process_input(input: Input) -> Result<i32, String> {
    if let Some(r) = pair(&input.vec[..], input.total) {
        Ok(r)
    } else {
        Err("Failed!".into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let part = Part::<Input, i32>::new(1, PartKind::One, create_input, process_input);
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
