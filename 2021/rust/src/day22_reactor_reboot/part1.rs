use core::*;
use std::collections::HashMap;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        22
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}

struct Cube {
    on: bool,
    x: Option<(i32, i32)>,
    y: Option<(i32, i32)>,
    z: Option<(i32, i32)>,
}

impl Cube {
    fn new(s: &str) -> Self {
        let items: Vec<&str> = s.split(&[' ', ','][..]).collect();

        let on: bool = items[0] == "on";

        let mut xyz: HashMap<&str, Option<(i32, i32)>> = items
            .iter()
            .skip(1)
            .map(|s| {
                let dim_range = s.split('=').collect::<Vec<&str>>();
                let (dim, range) = (dim_range[0], dim_range[1].split(".."));

                let mut iter = range.into_iter();
                let start = iter.next().unwrap().parse::<i32>().unwrap();
                let end = iter.next().unwrap().parse::<i32>().unwrap();

                let (start, end) = if start <= end {
                    (start, end)
                } else {
                    (end, start)
                };

                let set = get_inclusive_range(start, end);
                (dim, set)
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

fn get_inclusive_range(start: i32, end: i32) -> Option<(i32, i32)> {
    match ((-50).cmp(&start), 50.cmp(&end)) {
        (std::cmp::Ordering::Less, std::cmp::Ordering::Less) => None,
        (std::cmp::Ordering::Less, std::cmp::Ordering::Equal) => Some((start, end)),
        (std::cmp::Ordering::Less, std::cmp::Ordering::Greater) => Some((start, end)),
        (std::cmp::Ordering::Equal, std::cmp::Ordering::Less) => Some((-50, 50)),
        (std::cmp::Ordering::Equal, std::cmp::Ordering::Equal) => Some((-50, 50)),
        (std::cmp::Ordering::Equal, std::cmp::Ordering::Greater) => Some((-50, end)),
        (std::cmp::Ordering::Greater, std::cmp::Ordering::Less) => Some((-50, 50)),
        (std::cmp::Ordering::Greater, std::cmp::Ordering::Equal) => Some((-50, 50)),
        (std::cmp::Ordering::Greater, std::cmp::Ordering::Greater) => None,
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let cubes: Vec<Cube> = input.lines().map(Cube::new).collect();

        let filtered_cubes: Vec<&Cube> = cubes
            .iter()
            .filter(|c| c.x.is_some() && c.y.is_some() && c.z.is_some())
            .collect();

        let mut cuboids: HashMap<(i32, i32, i32), bool> = HashMap::new();

        for cube in filtered_cubes {
            let (xs, xe) = cube.x.unwrap();
            let (ys, ye) = cube.y.unwrap();
            let (zs, ze) = cube.z.unwrap();

            for x in xs..=xe {
                for y in ys..=ye {
                    for z in zs..=ze {
                        cuboids
                            .entry((x, y, z))
                            .and_modify(|e| *e = cube.on)
                            .or_insert(cube.on);
                    }
                }
            }
        }

        cuboids.values().filter(|x| **x).count().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let part = PartOne {};

        assert!(part.test_sample().is_ok());
        // assert!(part.test_puzzle().is_ok());
    }
}
