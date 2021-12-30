use core::*;
use std::collections::HashMap;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        14
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let template = input.lines().next().unwrap();

        let mut map: HashMap<String, String> = HashMap::new();

        let rules: HashMap<(char, char), char> = input
            .lines()
            .skip(2)
            .map(|l| {
                let lxs: Vec<&str> = l.split(" -> ").collect();

                let mut val = String::new();
                val.push(lxs[0].chars().nth(0).unwrap());
                val.push(lxs[1].chars().nth(0).unwrap());

                map.insert(lxs[0].into(), val);

                (
                    (
                        lxs[0].chars().nth(0).unwrap(),
                        lxs[0].chars().nth(1).unwrap(),
                    ),
                    lxs[1].chars().nth(0).unwrap(),
                )
            })
            .collect();

        let times = 4;

        let mut cycles: HashMap<&(char, char), Vec<Vec<(char, char)>>> = HashMap::new();

        for key in rules.keys().into_iter() {
            cycles.insert(key, extend_times(key, &rules, times));
        }

        let chars: Vec<char> = template.chars().into_iter().collect();

        let mut count_map: HashMap<char, usize> = HashMap::new();

        for (f, s) in chars.iter().zip(chars.iter().skip(1)) {
            if let Some(hm) = cycles.get(&(*f, *s)) {
                println!("{}", "hm");
            } else {
                panic!("Not found!");
            }
        }

        let min = count_map.values().min().unwrap();
        let max = count_map.values().max().unwrap();

        (max - min).to_string()
    }
}

fn extend_times(
    (f, s): &(char, char),
    rules: &HashMap<(char, char), char>,
    times: usize,
) -> Vec<Vec<(char, char)>> {
    let mut f1 = *f;
    let mut res: String = String::new();
    let mut pairs_list: Vec<Vec<(char, char)>> = vec![vec![(*f, *s)]];

    (0..times).fold(pairs_list, |mut pairs_list, _| {
        let mut pairs: Vec<(char, char)> = vec![];
        let mut last = s;

        for (f, s) in pairs_list.last().unwrap().iter() {
            
            if let Some(ch) = rules.get(&(f1, *s)) {
                pairs.push((*f, *ch));
                // pairs.push((*ch, *s));
                last = ch;
            }
        }

        pairs.push((*last, *s));
        
        pairs_list.push(pairs);

        pairs_list
    })

    // res.push(*f);

    // let mut count = 0_usize;

    // while let Some(ch) = rules.get(&(f1, *s)) {
    //     res.insert(1, *ch);
    //     v.push(res.clone());

    //     f1 = *ch;

    //     if count == times - 1 {
    //         // res.entry(*s).and_modify(|v| *v += 1).or_insert(1);
    //         break;
    //     }

    //     count += 1;
    // }

    // v
}

fn extend_times1(
    (f, s): &(char, char),
    rules: &HashMap<(char, char), char>,
    times: usize,
) -> HashMap<char, usize> {
    let mut s1 = *s;
    let mut res: HashMap<char, usize> = HashMap::new();

    res.insert(*f, 1);

    let mut count = 0_usize;

    while let Some(c) = rules.get(&(*f, s1)) {
        res.entry(*c).and_modify(|v| *v += 1).or_insert(1);

        s1 = *c;

        if count == times - 1 {
            // res.entry(*s).and_modify(|v| *v += 1).or_insert(1);
            break;
        }

        count += 1;
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let part = PartTwo {};
        assert!(part.test_sample().is_ok());
        // assert!(part.test_puzzle().is_ok());
    }
}
