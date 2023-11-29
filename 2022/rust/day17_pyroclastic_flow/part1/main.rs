type Shape = Vec<Vec<u8>>;

#[derive(Debug, Clone, PartialEq)]
enum Direction {
    Left,
    Right,
    Down,
}

const ROCKS: usize = 2023;

const MAX_WIDTH: usize = 7;

const MAX_HEIGHT: usize = ROCKS * 4 + 3;

#[derive(Clone)]
struct FallingShape {
    shape: Shape,
    row: usize,
    col: usize,
    initial_row: usize,
}

struct Chamber {
    spaces: [[u8; MAX_WIDTH]; MAX_HEIGHT],
    falling_shapes: Vec<Shape>,
    max_height: usize,
    dirs: Vec<Direction>,
    dir_index: usize,
    shape_index: usize,
    floater: FallingShape,
    heights: Vec<usize>,
}

impl Chamber {
    fn new(dirs: Vec<Direction>) -> Self {
        Self {
            spaces: [[0; MAX_WIDTH]; MAX_HEIGHT],
            falling_shapes: vec![
                vec![vec![1, 1, 1, 1]],
                vec![vec![0, 1, 0], vec![1, 1, 1], vec![0, 1, 0]],
                vec![vec![0, 0, 1], vec![0, 0, 1], vec![1, 1, 1]],
                vec![vec![1], vec![1], vec![1], vec![1]],
                vec![vec![1, 1], vec![1, 1]],
            ],
            max_height: MAX_HEIGHT,
            dir_index: 0,
            shape_index: 0,
            dirs,
            floater: FallingShape {
                shape: vec![vec![1, 1, 1, 1]],
                initial_row: MAX_HEIGHT - 1 - 3,
                row: MAX_HEIGHT - 1 - 3,
                col: 2,
            },
            heights: vec![],
        }
    }

    fn has_more_shapes(&mut self) -> bool {
        self.shape_index < ROCKS
    }

    fn get_next_shape(&mut self) -> bool {
        self.shape_index += 1;
        let shape = self.falling_shapes[self.shape_index % 5].clone();
        let row_count = shape.len();

        self.floater = FallingShape {
            shape,
            initial_row: self.max_height - row_count - 3,
            row: self.max_height - row_count - 3,
            col: 2,
        };
        self.heights.push(MAX_HEIGHT - self.max_height);
        self.has_more_shapes()
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
                    if !self.check(dir) {
                        self.floater.col += 1;
                    }
                }
                self.floater.row += 1;
                if !self.check(&Direction::Down) {
                    self.floater.row -= 1;
                    self.patch_floater();
                    self.get_next_shape();
                }
            }
            Direction::Right => {
                if self.floater.col + self.floater.shape[0].len() < MAX_WIDTH {
                    self.floater.col += 1;
                    if !self.check(dir) {
                        self.floater.col -= 1;
                    }
                }
                self.floater.row += 1;
                if !self.check(&Direction::Down) {
                    self.floater.row -= 1;
                    self.patch_floater();
                    self.get_next_shape();
                }
            }
            Direction::Down => {
                self.floater.row += 1;
                if self.check(dir) {
                    self.floater.row += 1;
                    if !self.check(&Direction::Down) {
                        self.floater.row -= 1;
                        self.patch_floater();
                        self.get_next_shape();
                    }
                } else {
                    self.floater.row -= 1;
                    self.patch_floater();
                    self.get_next_shape();
                }
            }
        }
    }

    fn process(&mut self) {
        while self.has_more_shapes() {
            let dir = self.get_next_direction();
            self.move_shape(&dir);
            self.patch_floater();
            self.get_next_shape();
        }
    }

    fn patch_floater(&mut self) {
        for (r, row) in self.floater.shape.iter().enumerate() {
            for (c, cell) in row.iter().enumerate() {
                if *cell == 1 {
                    self.spaces[self.floater.row + r][self.floater.col + c] = *cell;
                }
            }
        }
        if self.floater.row <= self.max_height {
            self.max_height = self.floater.row;
            println!(
                "{} {} {} {} {}",
                self.shape_index % 5,
                self.shape_index + 1,
                MAX_HEIGHT - self.max_height,
                MAX_HEIGHT - self.max_height + 3 + self.floater.shape.len(),
                MAX_HEIGHT - self.floater.initial_row,
            );
        } else {
            println!(
                "{} {} {} {} {} !",
                self.shape_index % 5,
                self.shape_index + 1,
                MAX_HEIGHT - self.max_height,
                MAX_HEIGHT - self.max_height + 3 + self.floater.shape.len(),
                MAX_HEIGHT - self.floater.initial_row,
            );
        }
    }

    fn check(&mut self, _dir: &Direction) -> bool {
        if self.floater.row + self.floater.shape.len() - 1 < MAX_HEIGHT {
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

    fn check1(&mut self, dir: &Direction) -> bool {
        match dir {
            Direction::Left => {
                // if self.floater.col >= 0 {
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
                // } else {
                //     return false;
                // }
            }
            Direction::Right => {
                // if self.floater.col + self.floater.shape[0].len() - 1 < MAX_WIDTH {
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
                // } else {
                //     return false;
                // }
            }
            Direction::Down => {
                if self.floater.row + self.floater.shape.len() - 1 < MAX_HEIGHT {
                    for (r, shape_row) in self.floater.shape.iter().rev().enumerate() {
                        for (c, cell) in shape_row.iter().enumerate() {
                            if *cell == 1
                                && self.spaces[self.floater.row + self.floater.shape.len() - 1 - r]
                                    [self.floater.col + c]
                                    == 1
                            {
                                return false;
                            }
                        }
                    }
                    return true;
                } else {
                    return false;
                }
            }
        }

        // if row + self.floater.shape.len() <= MAX_HEIGHT
        //     && col + self.floater.shape[0].len() <= MAX_WIDTH
        // {
        //     for (r, shape_row) in self.floater.shape.iter().enumerate() {
        //         for (c, _) in shape_row.iter().enumerate() {
        //             if self.spaces[row + r][col + c] == 1 {
        //                 return false;
        //             }
        //         }
        //     }
        //     true
        // } else {
        //     false
        // }
    }
}

fn main() {
    // let mut line = String::new();
    // let _ = std::io::stdin().read_line(&mut line).unwrap();
    //
    // if let Some(li) = line.strip_suffix('\n') {
    //     line = String::from(li);
    // }

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

    // let mut result = 0_usize;

    // for row in chamber.spaces[chamber.max_height..].iter() {
    //     for cell in row {
    //         result += (*cell) as usize;
    //     }
    // }

    println!("{:?}", MAX_HEIGHT - chamber.max_height);
}
