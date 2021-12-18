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

        let mut map1: HashMap<(char, char), Vec<HashMap<char, i32>>> = HashMap::new();

        get_count(rules, &mut map1);

        // let v: Vec<char> = template.chars().into_iter().collect();

        // let mut cc: HashMap<char, i32> = HashMap::new();

        // for (f, s) in v.iter().zip(v.iter().skip(1)) {
        //     if let Some(hm) = map1.get(&(*f, *s)) {
        //         for (k, v) in hm {
        //             *cc.entry(*k).or_insert(0) += v;
        //         }
        //     } else {
        //         panic!("Unexpected pair!");
        //     }
        // }

        // print!("{:?}", 'B');

        // let c = cc.get(&'N');

        let result = extend_polymer_times(20, template, &mut map);

        // println!("{:?}", v);

        let mut hm: HashMap<char, i32> = HashMap::new();

        for c in result.chars() {
            let entry = hm.entry(c).or_insert(0);
            *entry += 1;
        }

        let min = hm.values().min().unwrap();
        let max = hm.values().max().unwrap();

        (max - min).to_string()
    }
}

fn get_count(
    rules: HashMap<(char, char), char>,
    map: &mut HashMap<(char, char), Vec<HashMap<char, i32>>>,
) {
    for (f, s) in rules.keys() {

        let mut v: Vec<char> = vec![*f, *s];

        let mut vhm: Vec<HashMap<char, i32>> = vec![];
                
        for _ in 0 .. 40 {

            let vlen = v.len();

            for i in 0 .. vlen-1 {
                let (f,s) = (v[i*2], v[i*2+1]);
                if let Some(c) = rules.get(&(f, s)) {
                    v.insert(i*2+1, *c);
                    // v1.insert(i*2+2, s);
                } else {
                    panic!("!!!");
                }
            }

            let mut hm: HashMap<char, i32> = HashMap::new();

            for c in v.iter() {
                let entry = hm.entry(*c).or_insert(0);
                *entry += 1;
            }

            vhm.push(hm);
    
         }

         map.insert((*f, *s), vhm);

        println!("{:?}", v.len());
    }
}

fn extend_polymer_times(times: usize, template: &str, map: &mut HashMap<String, String>) -> String {
    (0..times).fold(template.to_string(), |template, i| {
        println!("{:?}", i);

        extend_polymer(template, map)
    })
}

fn extend_polymer(template: String, map: &mut HashMap<String, String>) -> String {
    let mut result = String::new();
    let chars: Vec<char> = template.chars().into_iter().collect();
    for start in 2..=template.len() {
        let sub = template[0..start].to_string();

        if map.contains_key(&sub) {
            let s = map.get(&sub).unwrap().clone();
            // println!("Found substring {:?}", s);
            result.replace_range(.., &s);
        } else {
            if sub.len() == 2 {
                map.insert(sub.clone(), sub.clone());
            } else {
                let sub1 = sub[sub.len() - 2..sub.len()].to_string();

                if let Some(s) = map.get(&sub1) {
                    result.push_str(&s);
                } else {
                    result.push_str(&sub1);
                    map.insert(sub1.clone(), sub1.clone());
                }
            }
        }

        map.insert(sub.to_string(), result.clone());
    }
    map.insert(template, result.clone());
    result.push(*chars.iter().last().unwrap());
    result.clone()
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
