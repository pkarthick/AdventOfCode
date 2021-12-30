use core::*;
use std::collections::{HashMap, HashSet, VecDeque};

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        22
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

struct Cube {
    on: bool,
    x: (i64, i64),
    y: (i64, i64),
    z: (i64, i64),
}

impl Cube {
    fn new(s: &str) -> Self {
        let items: Vec<&str> = s.split(&[' ', ','][..]).collect();

        let on: bool = items[0] == "on";

        let mut xyz: HashMap<&str, (i64, i64)> = items
            .iter()
            .skip(1)
            .map(|s| {
                let dim_range = s.split('=').collect::<Vec<&str>>();
                let (dim, range) = (dim_range[0], dim_range[1].split(".."));

                let mut iter = range.into_iter();
                let start = iter.next().unwrap().parse::<i64>().unwrap();
                let end = iter.next().unwrap().parse::<i64>().unwrap();

                if start <= end {
                    (dim, (start, end))
                } else {
                    (dim, (end, start))
                }
            })
            .collect();

        Cube {
            on,
            x: xyz.remove("x").unwrap(),
            y: xyz.remove("y").unwrap(),
            z: xyz.remove("z").unwrap(),
        }
    }
}

fn get_inclusive_range((on_s, on_e): (i32, i32), (off_s, off_e): (i32, i32)) -> Option<(i32, i32)> {
    if on_e < off_s || on_s > off_e {
        None
    } else {
        match (on_s.cmp(&off_s), on_e.cmp(&off_e)) {
            (std::cmp::Ordering::Less, std::cmp::Ordering::Less) => Some((off_s, on_e)),
            (std::cmp::Ordering::Less, std::cmp::Ordering::Equal) => Some((off_s, on_e)),
            (std::cmp::Ordering::Less, std::cmp::Ordering::Greater) => Some((off_s, off_e)),
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Less) => Some((on_s, on_e)),
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Equal) => Some((on_s, on_e)),
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Greater) => Some((on_s, off_e)),
            (std::cmp::Ordering::Greater, std::cmp::Ordering::Less) => Some((on_s, on_e)),
            (std::cmp::Ordering::Greater, std::cmp::Ordering::Equal) => Some((on_s, on_e)),
            (std::cmp::Ordering::Greater, std::cmp::Ordering::Greater) => None,
        }
    }
}

fn split_range(
    (on_s, on_e): (i64, i64),
    (off_s, off_e): (i64, i64),
) -> (Vec<(i64, i64)>, Vec<(i64, i64)>) {
    let mut on_ranges = vec![];
    let mut off_ranges = vec![];

    if !(on_e < off_s || on_s > off_e) {
        match (on_s.cmp(&off_s), on_e.cmp(&off_e)) {
            (std::cmp::Ordering::Less, std::cmp::Ordering::Less) => {
                on_ranges.push((on_s, off_s - 1));
                off_ranges.push((on_e, off_e));
            }
            (std::cmp::Ordering::Less, std::cmp::Ordering::Equal) => {
                on_ranges.push((on_s, off_s));
            }
            (std::cmp::Ordering::Less, std::cmp::Ordering::Greater) => {
                on_ranges.push((on_s, off_s - 1));
                on_ranges.push((off_e + 1, on_e));
            }
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Less) => {
                off_ranges.push((on_e, off_e));
            }
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Equal) => {}
            (std::cmp::Ordering::Equal, std::cmp::Ordering::Greater) => {
                on_ranges.push((off_e, on_e));
            }
            (std::cmp::Ordering::Greater, std::cmp::Ordering::Less) => {
                off_ranges.push((off_s, on_s));
                off_ranges.push((on_e, off_e));
            }
            (std::cmp::Ordering::Greater, std::cmp::Ordering::Equal) => {
                off_ranges.push((off_s, on_s));
            }
            (std::cmp::Ordering::Greater, std::cmp::Ordering::Greater) => {
                off_ranges.push((on_e, off_s));
                on_ranges.push((off_e+1, on_e)); 
            }
        }
    } else {
        off_ranges.push((off_s, off_e)); //off
    }

    (on_ranges, off_ranges)
}

fn split_cubes(on_cubes: &mut Vec<Cube>, off: Cube) -> Vec<Cube> {
    let mut off_cubes = vec![];
    let mut split_cubes = vec![];

    while let Some(on) = on_cubes.pop() {
        let (onxs, offxs) = split_range(on.x, off.x);
        let (onys, offys) = split_range(on.y, off.y);
        let (onzs, offzs) = split_range(on.z, off.z);

        if !onxs.is_empty() && !onys.is_empty() && !onzs.is_empty() {
            for xrange in onxs {
                for yrange in onys.iter() {
                    for zrange in onzs.iter() {
                        on_cubes.push(Cube {
                            on: true,
                            x: xrange,
                            y: *yrange,
                            z: *zrange,
                        });
                    }

                    for zrange in offzs.iter() {
                        off_cubes.push(Cube {
                            on: false,
                            x: xrange,
                            y: *yrange,
                            z: *zrange,
                        });
                    }
                }

                for yrange in offys.iter() {
                    off_cubes.push(Cube {
                        on: false,
                        x: xrange,
                        y: *yrange,
                        z: off.z,
                    });
                }
            }

            for xrange in offxs {
                off_cubes.push(Cube {
                    on: false,
                    x: xrange,
                    y: off.y,
                    z: off.z,
                });
            }
        } else {
            split_cubes.push(on);
        }
    }

    split_cubes
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let mut cubes: VecDeque<Cube> = input.lines().map(Cube::new).collect();

        let mut on_cubes: Vec<Cube> = vec![];
        let mut all_splits = vec![];

        while let Some(cube) = cubes.pop_front() {
            if cube.on {
                on_cubes.push(cube);
            } else {
                let mut xs = split_cubes(&mut on_cubes, cube);
                all_splits.append(&mut xs);
            }
        }

        // let mut set: HashSet<(i64, i64, i64)> = HashSet::new();

        // for c in all_splits.iter() {
        //     let mut x = c.x.0;

        //     while x <= c.x.1 {

        //         let mut y = c.y.0;

        //         while y <= c.y.1 {

        //             let mut z = c.z.0;

        //             while z <= c.z.1 {
                        
        //                 set.insert((x, y, z));

        //                 z += 1;
        //             }
                    
        //             y += 1;
                    
        //         }

        //         x += 1;
        //     }
        // }
        

        let mut count1: Vec<((i64,i64),(i64,i64),(i64,i64))> = all_splits
        .iter()
        .map(|c| (c.x, c.y, c.y))
        .collect();

        count1.sort();

        for x in count1.iter() {
            println!("{:?}", x);
        }

        let count = all_splits
            .into_iter()
            .map(|c| (c.x.1 - c.x.0).abs() * (c.y.1 - c.y.0).abs() * (c.z.1 - c.z.0).abs())
            .sum::<i64>();

        count.to_string()
    }
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
