use std::{
    collections::{HashMap, HashSet},
    fs,
    str::FromStr,
};

#[derive(Clone)]
pub struct Input {
    grid: Vec<Vec<Pipe>>,
    start_pos: (usize, usize),
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Direction {
    East,
    West,
    North,
    South,
}

#[derive(PartialEq, Clone)]
pub enum Pipe {
    V(Edge), // |
    H(Edge), // -
    L,       // L
    J,       // J
    D7,      // 7
    F,       // F
    G,       // .
    S,       //S
}

#[derive(PartialEq, Eq, Clone)]
enum Edge {
    Left,
    Top,
    Right,
    Bottom,
    Slash,
}

// fn get_pipe(c: char) -> Pipe {
//     match c {
//         '|' => Pipe::V,
//         '-' => Pipe::H,
//         'L' => Pipe::L,
//         'J' => Pipe::J,
//         '7' => Pipe::D7,
//         'F' => Pipe::F,
//         'S' => Pipe::S,
//         '.' => Pipe::G,
//         _ => Pipe::G,
//     }
// }

fn horizontal_edge(left: char, right: char) -> Pipe {
    match (left, right) {
        ('.', _) => Pipe::H(Edge::Top),
        (_, '.') => Pipe::H(Edge::Top),

        ('F', _) => Pipe::H(Edge::Top),
        ('J', _) => Pipe::H(Edge::Top),
        ('-', '7') => Pipe::H(Edge::Top),
        ('-', 'F') => Pipe::H(Edge::Top),

        ('|', '-') => Pipe::H(Edge::Top),
        ('-', '-') => Pipe::H(Edge::Top),
        ('-', '|') => Pipe::H(Edge::Top),

        ('|', 'F') => Pipe::H(Edge::Top),
        ('|', '|') => Pipe::H(Edge::Top),
        ('|', '7') => Pipe::H(Edge::Top),
        ('7', _) => Pipe::H(Edge::Top),

        (_, 'J') => Pipe::H(Edge::Bottom),
        (_, 'L') => Pipe::H(Edge::Top),
        ('L', _) => Pipe::H(Edge::Bottom),

        _ => panic!("horizontal {left} {right}"),
    }
}

impl Input {
    fn new(s: &str) -> Self {
        let mut start_pos = (0, 0);
        let mut pipes_grid: Vec<Vec<Pipe>> = vec![];

        let grid: Vec<Vec<char>> = s
            .split("\n")
            .map(|l| l.chars().into_iter().collect())
            .collect();

        for (r, l) in grid.iter().enumerate() {
            let mut cells: Vec<Pipe> = vec![];
            for (c, ch) in l.iter().enumerate() {
                if *ch == 'S' {
                    start_pos = (r, c);
                }
                let pipe = match *ch {
                    '|' => Pipe::V(Edge::Left),
                    '-' if c == 0 || c == 139 => Pipe::H(Edge::Top),
                    '-' => horizontal_edge(grid[r][c - 1], grid[r][c + 1]),
                    'L' => Pipe::L,
                    'J' => Pipe::J,
                    '7' => Pipe::D7,
                    'F' => Pipe::F,
                    'S' => Pipe::S,
                    '.' => Pipe::G,
                    _ => panic!(),
                };
                cells.push(pipe)
            }
            pipes_grid.push(cells);
        }

        let (r, c) = start_pos;

        pipes_grid[r][c] = Pipe::V(Edge::Left);

        Self {
            grid: pipes_grid,
            start_pos,
        }
    }

    fn traverse_path(
        &self,
        ((r, c), onward_dir): ((usize, usize), Direction),
        tiles: &mut Vec<((usize, usize), Direction)>,
    ) {
        if tiles.len() > 0 && self.start_pos == (r, c) {
            tiles.push(((r, c), onward_dir));
        } else {
            let target = self.find_target_direction_and_coordinate(((r, c), onward_dir));
            tiles.push(target.clone());
            self.traverse_path(target, tiles)
        }
    }

    fn find_target_direction_and_coordinate(
        &self,
        ((r, c), onward_dir): ((usize, usize), Direction), // (r, c): (usize, usize),
                                                           // onward_dir: Direction,
    ) -> ((usize, usize), Direction) {
        let onward_dir = match self.grid[r][c] {
            Pipe::V(_) => onward_dir,
            Pipe::H(_) => onward_dir,
            Pipe::L => match onward_dir {
                Direction::East => todo!(),
                Direction::West => Direction::North,
                Direction::North => todo!(),
                Direction::South => Direction::East,
            },
            Pipe::J => match onward_dir {
                Direction::East => Direction::North,
                Direction::West => todo!(),
                Direction::South => Direction::West,
                Direction::North => todo!(),
            },
            Pipe::D7 => match onward_dir {
                Direction::East => Direction::South,
                Direction::West => todo!(),
                Direction::North => Direction::West,
                Direction::South => Direction::South,
            },
            Pipe::F => match onward_dir {
                Direction::East => todo!(),
                Direction::West => Direction::South,
                Direction::North => Direction::East,
                Direction::South => todo!(),
            },
            Pipe::G => todo!(),
            Pipe::S => todo!(),
        };

        let onward_pos = if onward_dir == Direction::East {
            (r, c + 1)
        } else if onward_dir == Direction::West {
            (r, c - 1)
        } else if onward_dir == Direction::North {
            (r - 1, c)
        } else {
            (r + 1, c)
        };

        (onward_pos, onward_dir)
    }

    fn mark_sea(
        &mut self,
        (r, c): (i32, i32),
        visited_tiles: &HashSet<(usize, usize)>,
        visited_locations: &mut HashSet<(usize, usize)>,
        direction: Direction,
    ) {
        if r >= 0 && r < 140 && c >= 0 && c < 140 {
            let ru = r as usize;
            let cu = c as usize;

            if !visited_locations.contains(&(ru, cu)) {
                if !visited_tiles.contains(&(ru, cu)) {
                    self.grid[ru][cu] = Pipe::S;
                    visited_locations.insert((ru, cu));
                    // self.mark_sea((r - 1, (c - 1)), visited_tiles, visited_locations);
                    // self.mark_sea((r - 1, (c + 1)), visited_tiles, visited_locations);
                    // self.mark_sea((r + 1, (c - 1)), visited_tiles, visited_locations);
                    self.mark_sea(
                        (r, (c - 1)),
                        visited_tiles,
                        visited_locations,
                        Direction::West,
                    );
                    self.mark_sea(
                        (r - 1, c),
                        visited_tiles,
                        visited_locations,
                        Direction::North,
                    );
                    // self.mark_sea((r + 1, (c + 1)), visited_tiles, visited_locations);
                    self.mark_sea(
                        (r + 1, c),
                        visited_tiles,
                        visited_locations,
                        Direction::South,
                    );
                    self.mark_sea(
                        (r, (c + 1)),
                        visited_tiles,
                        visited_locations,
                        Direction::East,
                    );
                } else {
                    match direction {
                        Direction::East => {
                            if [Pipe::J, Pipe::D7].contains(&self.grid[ru][cu]) {
                                self.grid[ru][cu] = Pipe::S;
                                // visited_locations.insert((ru, cu));
                            }
                        }
                        Direction::West => {
                            if [Pipe::F, Pipe::L].contains(&self.grid[ru][cu]) {
                                self.grid[ru][cu] = Pipe::S;
                                // visited_locations.insert((ru, cu));
                            }
                        }
                        Direction::North => {
                            if [Pipe::D7, Pipe::F].contains(&self.grid[ru][cu]) {
                                self.grid[ru][cu] = Pipe::S;
                                // visited_locations.insert((ru, cu));
                            }
                        }
                        Direction::South => {
                            if [Pipe::L].contains(&self.grid[ru][cu]) {
                                self.grid[ru][cu] = Pipe::S;
                                // visited_locations.insert((ru, cu));
                            }
                        }
                    }
                    visited_locations.insert((ru, cu));
                }
            }
        }
    }

    fn part1(&mut self) -> String {
        let target = self.find_target_direction_and_coordinate((self.start_pos, Direction::North));

        let mut visited_tiles: Vec<((usize, usize), Direction)> = vec![];
        self.traverse_path(target, &mut visited_tiles);

        let mut visited_locations: HashSet<(usize, usize)> = HashSet::new();

        self.mark_sea(
            (0, 0),
            &visited_tiles.iter().map(|(rc, _)| *rc).collect(),
            &mut visited_locations,
            Direction::East,
        );

        let mut s = String::new();

        for r in 0..self.grid.len() {
            for c in 0..self.grid[0].len() {
                if (r, c) == self.start_pos {
                    s.push('*')
                } else if visited_tiles
                    .iter()
                    .find(|((rr, cc), _dir)| *rr == r && *cc == c)
                    .is_none()
                {
                    if self.grid[r][c] == Pipe::S {
                        s.push('S')
                    } else {
                        s.push(' ')
                    }
                } else {
                    let ch = match self.grid[r][c] {
                        Pipe::V(_) => '|',
                        Pipe::H(_) => '-',
                        Pipe::L => 'L',
                        Pipe::J => 'J',
                        Pipe::D7 => '7',
                        Pipe::F => 'F',
                        Pipe::G => ' ',
                        Pipe::S => ' ',
                        _ => panic!(),
                    };
                    s.push(ch);
                }
            }
        }

        let new_count = s.chars().filter(|c| *c == '.').count();
        let farthest_distance = (visited_tiles.len() + 1) / 2;

        println!("{new_count:?}");
        println!("{s:?}");
        println!("{farthest_distance:?}");

        farthest_distance.to_string()
    }
}

impl FromStr for Input {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Input::new(s))
    }
}

fn main() {
    let contents = fs::read_to_string("../../input/day10").unwrap();
    let mut input = contents.parse::<Input>().unwrap();
    let _x = input.part1();
    // println!("{}", input.part1());
}
