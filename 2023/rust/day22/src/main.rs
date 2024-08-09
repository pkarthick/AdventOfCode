use std::{collections::HashMap, fs};

#[derive(Debug, Clone)]
struct Coordinate {
    x: i32,
    y: i32,
    z: i32,
}

#[derive(Debug, Clone)]
struct Brick {
    id: usize,
    end1: Coordinate,
    end2: Coordinate,
}

impl Brick {
    fn new((id, l): (usize, &str)) -> Self {
        let ends = l.split('~');
        let mut iter = ends.into_iter();
        let end1: Vec<i32> = iter
            .next()
            .unwrap()
            .split(",")
            .map(|s| s.parse::<i32>().unwrap())
            .collect();

        let end1 = Coordinate {
            x: end1[0],
            y: end1[1],
            z: end1[2],
        };

        let end2: Vec<i32> = iter
            .next()
            .unwrap()
            .split(",")
            .map(|s| s.parse::<i32>().unwrap())
            .collect();

        let end2 = Coordinate {
            x: end2[0],
            y: end2[1],
            z: end2[2],
        };

        Brick { id, end1, end2 }
    }

    fn overlaps(&self, br: &Brick) -> bool {
        if (br.end1.x < self.end1.x && br.end2.x < self.end2.x)
            || (br.end1.x > self.end1.x && br.end2.x > self.end2.x)
        {
            false
        } else if (br.end1.y < self.end1.y && br.end2.y < self.end2.y)
            || (br.end1.y > self.end1.y && br.end2.y > self.end2.y)
        {
            false
        } else {
            true
        }
    }
}

#[derive(Debug)]
struct Tower {
    bricks: Vec<Brick>,
    supports: HashMap<usize, Vec<usize>>,
    supported_by: HashMap<usize, Vec<usize>>,
}

impl Tower {
    fn new() -> Self {
        let supports: HashMap<usize, Vec<usize>> = HashMap::new();
        let supported_by: HashMap<usize, Vec<usize>> = HashMap::new();

        let bricks = Vec::new();

        Tower {
            bricks,
            supports,
            supported_by,
        }
    }

    fn add_brick(&mut self, brick: Brick) {
        let mut pos = None;

        for (i, br) in self.bricks.iter().enumerate() {
            if br.overlaps(&brick) {
                self.supports
                    .entry(br.id)
                    .and_modify(|vec| vec.push(brick.id))
                    .or_insert(vec![brick.id]);

                self.supported_by
                    .entry(brick.id)
                    .and_modify(|vec| vec.push(br.id))
                    .or_insert(vec![br.id]);

                if pos.is_none() {
                    pos = Some(i);
                }
            } else {
                if pos.is_some() {
                    break;
                }
            }
        }

        if self.bricks.is_empty() {
            self.bricks.push(brick.clone());
        } else {
            self.bricks
                .insert(*pos.get_or_insert(self.bricks.len() - 1), brick.clone());
        }
    }
}

fn main() {
    let s = fs::read_to_string("../../input/day22").unwrap();

    let mut bricks: Vec<Brick> = s.lines().enumerate().map(Brick::new).collect();

    // bricks.sort_by(|b1, b2| b1.end2.z.cmp(&b2.end1.z));

    let mut zmax = 1;

    bricks.iter_mut().for_each(|brick| {
        let zh = brick.end1.z - brick.end2.z;
        brick.end2.z = zmax;
        zmax += zh;
        brick.end1.z = zmax;
    });

    let mut tower = Tower::new();

    while let Some(brick) = bricks.pop() {
        tower.add_brick(brick);
    }

    let mut count = 0;

    for brick in tower.bricks {
        if let Some(supported_bricks) = tower.supports.get(&brick.id) {
            if supported_bricks
                .into_iter()
                .all(|brick_id| tower.supported_by[brick_id].len() > 1)
            {
                count += 1;
            }
        } else {
            count += 1;
        }
    }

    println!("{count:?}");
}
