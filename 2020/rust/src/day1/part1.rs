use std::cmp::Ordering;
use core::*;

fn pair(v: &[i32], total: i32) -> i32 {
    let mut s = 0;
    let mut e = v.len() - 1;

    while s < e {
        match (v[s] + v[e]).cmp(&total) {
            Ordering::Equal => return v[s] * v[e],
            Ordering::Less => s += 1,
            Ordering::Greater => e -= 1,
        }
    }

    panic!("Unexpected scenario");
}

struct PartOne {}

impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        1
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let mut vec = input
            .split('\n')
            .map(|s| s.parse().unwrap())
            .collect::<Vec<i32>>();

        vec.sort();

        pair(&vec[..], 2020).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test() {
        let part = PartOne{};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
