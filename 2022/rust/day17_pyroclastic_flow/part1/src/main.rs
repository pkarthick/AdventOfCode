type Shape = Vec<Vec<u8>>;

#[derive(Debug, Clone, PartialEq)]
enum Direction {
    Left,
    Right,
}

const ROCKS: usize = 2023;

const MAX_WIDTH: usize = 7;

const MAX_HEIGHT: usize = ROCKS * 2 + 3;

#[derive(Clone)]
struct FallingShape {
    shape: Shape,
    row: usize,
    col: usize,
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
                row: MAX_HEIGHT - 1 - 3,
                col: 2,
            },
            heights: vec![],
        }
    }

    fn has_more_shapes(&mut self) -> bool {
        self.shape_index < ROCKS - 2
    }

    fn get_next_shape(&mut self) -> bool {
        self.shape_index += 1;
        let shape = self.falling_shapes[self.shape_index % 5].clone();
        let row_count = shape.len();

        self.floater = FallingShape {
            shape,
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
                    self.get_next_shape();
                } else {
                    break;
                }
            }
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
        }
    }

    fn check(&mut self) -> bool {
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
}

fn main() {
    let mut line = String::new();
    let _ = std::io::stdin().read_line(&mut line).unwrap();
    
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
    println!("{:?}", MAX_HEIGHT - chamber.max_height);
}
