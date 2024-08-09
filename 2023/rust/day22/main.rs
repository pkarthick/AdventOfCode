use std::{
    cmp::{max, min},
    collections::{HashMap, VecDeque},
    fs,
};

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
            // let zmin1 = min(br.end1.z, br.end2.z);
            // let zmin = min(self.end1.z, self.end2.z);
            // // zmin + 1 == zmin1
            // zmin1 != zmin
            true
        }
    }
}

#[derive(Debug)]
struct Tower {
    minx: i32,
    maxx: i32,
    miny: i32,
    maxy: i32,
    minz: i32,
    maxz: i32,

    bricks: VecDeque<Brick>,
    supports: HashMap<usize, Vec<usize>>,
    supported_by: HashMap<usize, Vec<usize>>,
}

impl Tower {
    fn new(brick: Brick) -> Self {
        let supports: HashMap<usize, Vec<usize>> = HashMap::new();
        let supported_by: HashMap<usize, Vec<usize>> = HashMap::new();

        let minx = min(brick.end1.x, brick.end2.x);

        let maxx = max(brick.end1.x, brick.end2.x);

        let miny = min(brick.end1.y, brick.end2.y);

        let maxy = max(brick.end1.y, brick.end2.y);

        let minz = min(brick.end1.z, brick.end2.z);

        let maxz = max(brick.end1.z, brick.end2.z);

        let mut bricks = VecDeque::new();
        bricks.push_front(brick);

        Tower {
            minx,
            maxx,
            miny,
            maxy,
            minz,
            maxz,
            bricks,
            supports,
            supported_by,
        }
    }

    fn add_brick(&mut self, brick: Brick) {
        let minx = min(brick.end1.x, brick.end2.x);
        if self.minx > minx {
            self.minx = minx;
        }
        let maxx = max(brick.end1.x, brick.end2.x);
        if self.maxx < maxx {
            self.maxx = maxx;
        }

        let miny = min(brick.end1.y, brick.end2.y);
        if self.miny > miny {
            self.miny = miny;
        }
        let maxy = max(brick.end1.y, brick.end2.y);
        if self.maxy < maxy {
            self.maxy = maxy;
        }

        let minz = min(brick.end1.z, brick.end2.z);
        if self.minz > minz {
            self.minz = minz;
        }
        let maxz = max(brick.end1.z, brick.end2.z);
        if self.maxz < maxz {
            self.maxz = maxz;
        }

        self.fall_down(brick);
    }

    fn fall_down(&mut self, brick: Brick) {
        let mut found = false;
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
                    found = true;
                }
            } else {
                if found {
                    break;
                }
            }
        }

        self.bricks
            .insert(*pos.get_or_insert(self.bricks.len() - 1), brick.clone());
    }

    fn overlaps(&self, br: &Brick) -> bool {
        if (br.end1.x < self.minx && br.end2.x < self.minx)
            || (br.end1.x > self.maxx && br.end2.x > self.maxx)
        {
            false
        } else if (br.end1.y < self.miny && br.end2.y < self.miny)
            || (br.end1.y > self.maxy && br.end2.y > self.maxy)
        {
            false
        } else {
            // let zmin1 = min(br.end1.z, br.end2.z);
            // let zmin = min(self.end1.z, self.end2.z);
            // // zmin + 1 == zmin1
            // zmin1 != zmin
            true
        }
    }
}

fn main() {
    let s = fs::read_to_string("../../input/day22").unwrap();

    let mut bricks: VecDeque<Brick> = s.lines().enumerate().map(Brick::new).collect();

    let mut tower = Tower::new(bricks.pop_front().unwrap());

    // let mut towers: Vec<Tower> = vec![tower];

    while let Some(brick) = bricks.pop_front() {
        // let mut added = false;
        // for tower in towers.iter_mut() {
        // if tower.overlaps(&brick) {
        tower.add_brick(brick.clone());
        // added = true;
        // break;
        // }
        // }
        // if !added {
        //     towers.push(Tower::new(brick));
        // }
    }

    let mut count = 0;

    for brick in tower.bricks {
        if let Some(supported_bricks) = tower.supports.get(&brick.id) {
            if !supported_bricks.is_empty() {
                let mut supported = true;
                for brick_id in supported_bricks {
                    if tower.supported_by[brick_id].len() == 1 {
                        supported = false;
                        break;
                    }
                }
                if supported {
                    count += 1;
                }
            }
        } else {
            count += 1;
        }
    }

    println!("{count:?}");
}
