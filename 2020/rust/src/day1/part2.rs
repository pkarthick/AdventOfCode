use std::cmp::Ordering;
use core::*;

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

fn triple(v: Vec<i32>) -> i32 {
    for (i, x) in v.iter().enumerate() {
        if let Some(prod) = pair(&v[i + 1..], 2020 - x) {
            return prod * *x;
        }
    }

    panic!("Unexpected scenario!")
}

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        1
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let mut vec = input
            .split('\n')
            .map(|s| s.parse().unwrap())
            .collect::<Vec<i32>>();

        vec.sort();

        triple(vec).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test() {
        let part = PartTwo{};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
