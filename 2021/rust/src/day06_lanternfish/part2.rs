use core::*;
use std::collections::HashMap;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        6
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {

        let mut age_groups: HashMap<usize, usize> = HashMap::new();

        for age in 0..=8 {
            age_groups.insert(age, 0);
        }

        for age in input.split(',').map(|x| x.parse::<usize>().unwrap()) {
            age_groups.entry(age).and_modify(|e| *e += 1);
        }

        for _ in 0..256 {
            let new_count = *age_groups.get(&0).unwrap();

            for age in 1..=8 {
                age_groups.insert(age - 1, *age_groups.get(&age).unwrap());
            }

            age_groups.entry(8).and_modify(|e| *e = new_count);
            age_groups.entry(6).and_modify(|e| *e += new_count);

        }

        age_groups.values().into_iter().sum::<usize>().to_string()
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
