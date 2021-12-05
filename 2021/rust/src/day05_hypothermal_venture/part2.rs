use core::*;
use std::{cmp, collections::HashMap};

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        5
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let map: HashMap<(i32, i32), usize> = input.lines().fold(HashMap::new(), |mut map, l| {
            let v: Vec<(i32, i32)> = l
                .split(" -> ")
                .map(|csv| {
                    let tup = csv
                        .split(',')
                        .map(|n| n.parse::<i32>().unwrap())
                        .collect::<Vec<i32>>();
                    (tup[0], tup[1])
                })
                .collect();

            let (x1, y1) = v[0];
            let (x2, y2) = v[1];

            fn diff(a: i32, b: i32) -> i32 {
                match a.cmp(&b) {
                    cmp::Ordering::Less => 1,
                    cmp::Ordering::Equal => 0,
                    cmp::Ordering::Greater => -1,
                }
            }

            let xd = diff(x1, x2);
            let yd = diff(y1, y2);

            let mut x = x1;
            let mut y = y1;

            loop {
                let v = if let Some(v) = map.get(&(x, y)) {
                    *v + 1
                } else {
                    1
                };
                map.insert((x, y), v);

                if (x, y) == (x2, y2) {
                    break;
                }

                x += xd;
                y += yd;
            }

            map
        });

        map.values().filter(|x| **x > 1).count().to_string()
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
