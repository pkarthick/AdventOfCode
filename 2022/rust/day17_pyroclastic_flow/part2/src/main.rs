use std::collections::{HashMap, HashSet};

type Shape = Vec<Vec<u8>>;

#[derive(Debug, Clone, PartialEq)]
enum Direction {
    Left,
    Right,
}

const ROCKS: usize = 2022;

const MAX_WIDTH: usize = 7;

#[derive(Clone)]
struct FallingShape {
    shape: Shape,
    row: usize,
    col: usize,
}

struct Chamber {
    spaces: Vec<[u8; MAX_WIDTH]>,
    falling_shapes: Vec<Shape>,
    dirs: Vec<Direction>,
    dir_index: usize,
    shape_index: usize,
    floater: FallingShape,
    removed_height: usize,
    keys: HashMap<(String, usize), (usize, usize)>,
    blocked: [bool; MAX_WIDTH],
    cache: HashMap<(String, usize, usize), HashSet<(usize, usize)>>,
}

impl Chamber {
    fn new(dirs: Vec<Direction>) -> Self {
        let spaces: Vec<[u8; MAX_WIDTH]> = (0..4).map(|_| [0; MAX_WIDTH]).collect();

        let falling_shapes = vec![
            vec![vec![1, 1, 1, 1]],
            vec![vec![0, 1, 0], vec![1, 1, 1], vec![0, 1, 0]],
            vec![vec![0, 0, 1], vec![0, 0, 1], vec![1, 1, 1]],
            vec![vec![1], vec![1], vec![1], vec![1]],
            vec![vec![1, 1], vec![1, 1]],
        ];

        let shape = falling_shapes[0].clone();

        Self {
            spaces: spaces,
            falling_shapes: falling_shapes,
            dir_index: 0,
            shape_index: 0,
            dirs,
            floater: FallingShape {
                shape: shape,
                row: 0,
                col: 2,
            },
            removed_height: 0,
            blocked: [false; MAX_WIDTH],
            cache: HashMap::new(),
            keys: HashMap::new(),
        }
    }

    fn has_more_shapes(&mut self) -> bool {
        self.shape_index < ROCKS - 1
    }

    fn get_next_shape(&mut self) {
        let available = self.empty_rows_count();
        self.shape_index += 1;
        let shape = self.falling_shapes[self.shape_index % 5].clone();
        let required = 3 + shape.len();

        let row = if required > available {
            self.add_empty_rows(3 + shape.len() - available);
            0
        } else {
            available - required
        };

        self.floater = FallingShape {
            shape,
            row: row,
            col: 2,
        };
    }

    fn add_empty_rows(&mut self, count: usize) {
        let mut j = 0;
        while j < count {
            self.spaces.insert(0, [0; MAX_WIDTH]);
            j += 1;
        }
    }

    fn empty_rows_count(&mut self) -> usize {
        let mut i = 0;

        loop {
            if i == self.spaces.len() {
                return self.spaces.len();
            }
            if !self.spaces[i].iter().all(|x| *x == 0) {
                return i;
            }
            i += 1;
        }
    }

    fn get_next_direction(&mut self) -> Direction {
        if self.dir_index < self.dirs.len() {
            let dir = self.dirs[self.dir_index].clone();
            self.dir_index += 1;
            dir
        } else {
            self.dir_index = 1;
            self.dirs[0].clone()
        }
    }

    fn move_shape(&mut self, dir: &Direction) {
        match dir {
            Direction::Left => {
                if self.floater.col > 0 {
                    self.floater.col -= 1;
                    if !self.check() {
                        self.floater.col += 1;
                    }
                }
            }
            Direction::Right => {
                if self.floater.col + self.floater.shape[0].len() < MAX_WIDTH {
                    self.floater.col += 1;
                    if !self.check() {
                        self.floater.col -= 1;
                    }
                }
            }
        }
    }

    fn process(&mut self) {
        loop {
            let dir = self.get_next_direction();
            self.move_shape(&dir);
            self.floater.row += 1;
            if !self.check() {
                self.floater.row -= 1;
                self.patch_floater();

                if self.has_more_shapes() {
                    if self.blocked.iter().all(|x| *x) {
                        // self.fun_name();
                    }

                    self.get_next_shape();
                } else {
                    break;
                }
            }
        }
    }

    fn get_key(&self, start: usize, count: usize) -> Option<String> {
        if count > 4 && start < self.spaces.len() && start+count <= self.spaces.len() {
            let mut s = String::new();
            for i in start..start + count {
                if i >= self.spaces.len() {
                    return None;
                }
                for d in self.spaces[i] {
                    let ch = if d == 0 { '0' } else { '1' };
                    s.push(ch);
                }
            }
            Some(s)
        } else {
            None
        }
    }

    fn patch_floater(&mut self) {
        for (r, row) in self.floater.shape.iter().enumerate() {
            for (c, cell) in row.iter().enumerate() {
                if *cell == 1 {
                    self.spaces[self.floater.row + r][self.floater.col + c] = *cell;
                    self.blocked[self.floater.col + c] = true;
                }
            }
        }

        let start = self.empty_rows_count();
        let height = self.spaces.len() - start;

        let mut s = String::new();
        for i in start..self.spaces.len() {
            for d in self.spaces[i] {
                let ch = if d == 0 { '0' } else { '1' };
                s.push(ch);
            }
            let count = i - start + 1;
            if let Some((shape_index, previous_height)) = self.keys.insert((s.clone(), count), (self.shape_index, self.spaces.len() - start)) {
                if let Some(s1) = self.get_key(i + 1, count) {
                    if  s == s1 {

                        let pattern_height = height - previous_height;
                        let prefix_height = previous_height - pattern_height;

                        let rocks_per_pattern = self.shape_index - shape_index;
                        let prefix_rocks = shape_index - rocks_per_pattern;
                        let patterns_count = (ROCKS - prefix_rocks) / rocks_per_pattern;
                        let remaining_rocks = ROCKS - prefix_rocks - (patterns_count * rocks_per_pattern);
                        let pattern_height = count;

                        let height_without_suffix = patterns_count * pattern_height + prefix_height;
                        
                        println!("{} {} {} {} {} {}", height_without_suffix, rocks_per_pattern, patterns_count, remaining_rocks, prefix_height, pattern_height);
                        panic!("Found it!");
                    }
                }
            } else {

            }
        }

        // if self.spaces.len() > start + 57 {
        //     let mut s = String::new();

        //     for i in start..start + 57 {
        //         for d in self.spaces[i] {
        //             let ch = if d == 0 { '0' } else { '1' };
        //             s.push(ch);
        //         }
        //     }

        //     self.cache
        //         .entry((s.clone(), 0, 0))
        //         .and_modify(|v| {
        //             if v.insert((start, self.spaces.len()-start)) {
        //                 // println!("Here");
        //                 // println!("{:?}", s);
        //                 // let mut v1: Vec<_> = v.iter().collect();
        //                 // v1.sort();
        //                 // println!("{:?}", v1);
        //                 // println!("{:?}", self.spaces.len());
        //                 // println!();
        //             }
        //         })
        //         .or_insert(HashSet::from([(start, self.spaces.len()-start)]));
        // }
    }

    fn check(&mut self) -> bool {
        if self.floater.row + self.floater.shape.len() - 1 < self.spaces.len() {
            for c in 0..self.floater.shape[0].len() {
                for (r, shape_row) in self.floater.shape.iter().enumerate() {
                    if shape_row[c] == 1
                        && self.spaces[self.floater.row + r][self.floater.col + c] == 1
                    {
                        return false;
                    }
                }
            }
            return true;
        }
        return false;
    }

    fn find_pattern(&mut self) {
        println!("len: {}", self.spaces.len());

        let mut map: HashMap<[u8; MAX_WIDTH], Vec<usize>> = HashMap::new();

        for i in 0..self.spaces.len() {
            map.entry(self.spaces[i])
                .and_modify(|c| {
                    c.push(i);
                })
                .or_insert(vec![i]);

            // println!("{} elem count: {}", i, count);
        }

        let mut i = 0;
        let mut stretch = 0;
        let mut max_stretch = 0;
        let mut start = 0;

        for v in map.values_mut() {
            v.sort();
        }

        println!("keys {}", map.keys().len());

        loop {
            if i == 30 {
                break;
            }

            let vec: &Vec<_> = map.get(&self.spaces[i]).unwrap();

            if vec.len() > 1 {
                stretch += 1;
                if stretch > max_stretch {
                    max_stretch = stretch;
                }
            } else {
                start = i;
            }

            println!("{:?}", self.spaces[i]);
            println!("{:?}", vec);

            i += 1;
        }

        // let mut values: Vec<_> = map.values().collect();
        // values.sort();

        // println!("{:?}", values);
    }

    fn remove_empty_rows(&mut self) {
        let count = self.empty_rows_count();
        for _ in 0..count {
            self.spaces.remove(0);
        }
    }
}

fn main() {
    // let mut line = String::new();
    // let _ = std::io::stdin().read_line(&mut line).unwrap();

    let line = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

    let dirs: Vec<Direction> = line
        .chars()
        .map(|c| {
            if c == '>' {
                Direction::Right
            } else {
                Direction::Left
            }
        })
        .collect();

    let mut chamber = Chamber::new(dirs);
    chamber.process();
    chamber.remove_empty_rows();
    // chamber.find_pattern();

    println!("{:?}", chamber.spaces.len());
}
