use core::*;
use std::collections::HashMap;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        5
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let map: HashMap<(usize, usize), usize> =
            input.lines().fold(HashMap::new(), |mut map, l| {
                let v: Vec<(usize, usize)> = l
                    .split(" -> ")
                    .map(|csv| {
                        let tup = csv
                            .split(',')
                            .map(|n| n.parse::<usize>().unwrap())
                            .collect::<Vec<usize>>();
                        (tup[0], tup[1])
                    })
                    .collect();

                let (x1, y1) = v[0];
                let (x2, y2) = v[1];

                if y1 == y2 {
                    let (x1, x2) = if x1 <= x2 { (x1, x2) } else { (x2, x1) };
                    (x1..=x2).map(|x| (x, y1)).into_iter().for_each(|k| {
                        let v = map.entry(k).or_insert(0);
                        *v += 1;
                    });
                } else if x1 == x2 {
                    let (y1, y2) = if y1 <= y2 { (y1, y2) } else { (y2, y1) };
                    (y1..=y2).map(|y| (x1, y)).into_iter().for_each(|k| {
                        let v = map.entry(k).or_insert(0);
                        *v += 1;
                    });
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
        let part = PartOne {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
