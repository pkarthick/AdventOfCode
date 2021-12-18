use core::*;
use std::collections::HashMap;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        14
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let template = input.lines().next().unwrap();

        let rules: HashMap<(char, char), char> = input
            .lines()
            .skip(2)
            .map(|l| {
                let lxs: Vec<&str> = l.split(" -> ").collect();
                (
                    (
                        lxs[0].chars().nth(0).unwrap(),
                        lxs[0].chars().nth(1).unwrap(),
                    ),
                    lxs[1].chars().nth(0).unwrap(),
                )
            })
            .collect();

        let v = (0..10).fold(template.to_string(), |template, _| {
            let chars: Vec<char> = template.chars().into_iter().collect();

            let mut v =
                chars
                    .iter()
                    .zip(chars.iter().skip(1))
                    .fold(String::new(), |mut acc, (f, s)| {
                        acc.push(*f);
                        if rules.contains_key(&(*f, *s)) {
                            acc.push(*rules.get(&(*f, *s)).unwrap());
                        }
                        acc
                    });

            v.push(*chars.iter().last().unwrap());
            v
        });

        let mut hm: HashMap<char, i64> = HashMap::new();

        for c in v.chars() {
            let entry = hm.entry(c).or_insert(0);
            *entry += 1;
        }

        let min = hm.values().min().unwrap();
        let max = hm.values().max().unwrap();

        (max-min).to_string()
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
