use std::cmp::Ordering;

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

fn triple(input: Input) -> Option<i32> {
    let v = &input.vec[..];
    let total = input.total;

    for (i, x) in v.iter().enumerate() {
        if let Some(prod) = pair(&v[i + 1..], total - x) {
            return Some(prod * *x);
        }
    }

    None
}


fn create_input(s: String) -> Input {
    Input {
        vec: crate::core::to_vec(&s, Some(true)),
        total: 2020,
    }
}

fn process_input(input: Input) -> Result<i32, String> {
    if let Some(r) = triple(input) {
        Ok(r)
    } else {
        Err("Failed!".into())
    }
}


#[cfg(test)]
mod tests {
    use crate::core::*;

    use super::*;

    #[test]
    fn test() {
        let part = Part::new(1, PartKind::Two, create_input, process_input);
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }    
}
