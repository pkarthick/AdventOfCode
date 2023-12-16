use std::{collections::HashSet, fs};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Element {
    Empty,
    Vertical,
    Horizontal,
    Backslash,
    Slash,
}

impl Element {
    fn new(ch: char) -> Self {
        match ch {
            '.' => Element::Empty,
            '|' => Element::Vertical,
            '-' => Element::Horizontal,
            '\\' => Element::Backslash,
            '/' => Element::Slash,
            _ => panic!("Unexpected element!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Direction {
    East,
    South,
    North,
    West,
}

#[derive(Debug, Clone)]
struct Contraption {
    elements: Vec<Vec<Element>>,
    pending: Vec<(Direction, Position)>,
    energized: Vec<Vec<bool>>,
    visited: HashSet<(Position, Direction)>,
    height: usize,
    width: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Position {
    row: usize,
    col: usize,
    height: usize,
    width: usize,
}

impl Position {
    fn can_move(&self, direction: &Direction) -> bool {
        match direction {
            Direction::East => self.col < self.width - 1,
            Direction::South => self.row < self.height - 1,
            Direction::North => self.row > 0,
            Direction::West => self.col > 0,
        }
    }

    fn move_towards_direction(&mut self, direction: &Direction) -> bool {
        if self.can_move(direction) {
            match direction {
                Direction::East => self.col += 1,
                Direction::South => self.row += 1,
                Direction::North => self.row -= 1,
                Direction::West => self.col -= 1,
            }
            true
        } else {
            false
        }
    }
}

impl Contraption {
    fn new(input: String) -> Self {
        let lines: Vec<&str> = input.split('\n').collect();
        let elements: Vec<Vec<Element>> = lines
            .into_iter()
            .map(|l| l.chars().map(Element::new).collect())
            .collect();

        let height = elements.len();
        let width = elements[0].len();

        let energized: Vec<Vec<bool>> = (0..height)
            .into_iter()
            .map(|_| (0..width).into_iter().map(|_| false).collect())
            .collect();

        Contraption {
            elements,
            pending: vec![],
            energized,
            visited: HashSet::new(),
            height,
            width,
        }
    }

    fn add_initial_position(&mut self, (dir, pos): (Direction, Position)) {
        self.check_direction(pos, dir);
    }

    fn process_pending(&mut self) {
        while let Some((direction, mut position)) = self.pending.pop() {
            if !self
                .visited
                .contains(&(position.clone(), direction.clone()))
            {
                self.visited.insert((position.clone(), direction.clone()));
                self.energized[position.row][position.col] = true;

                if position.move_towards_direction(&direction) {
                    self.check_direction(position, direction);
                }
            }
        }
    }

    fn check_direction(&mut self, position: Position, direction: Direction) {
        match self.elements[position.row][position.col] {
            Element::Empty => {
                self.pending.push((direction.clone(), position.clone()));
            }
            Element::Vertical => {
                if direction == Direction::East || direction == Direction::West {
                    self.pending.push((Direction::North, position.clone()));
                    self.pending.push((Direction::South, position.clone()));
                } else {
                    self.pending.push((direction.clone(), position));
                }
            }
            Element::Horizontal => {
                if direction == Direction::North || direction == Direction::South {
                    self.pending.push((Direction::East, position.clone()));
                    self.pending.push((Direction::West, position.clone()));
                } else {
                    self.pending.push((direction.clone(), position.clone()));
                }
            }
            Element::Backslash => {
                let new_direction = match direction {
                    Direction::East => Direction::South,
                    Direction::South => Direction::East,
                    Direction::North => Direction::West,
                    Direction::West => Direction::North,
                };

                self.pending.push((new_direction.clone(), position.clone()));
            }
            Element::Slash => {
                let new_direction = match direction {
                    Direction::East => Direction::North,
                    Direction::South => Direction::West,
                    Direction::North => Direction::East,
                    Direction::West => Direction::South,
                };

                self.pending.push((new_direction.clone(), position.clone()));
            }
        }
    }

    fn get_energized_count(&self) -> usize {
        self.energized
            .iter()
            .map(|r| r.iter().filter(|c| **c).count())
            .sum()
    }

    fn get_border_starting_positions(&self) -> Vec<Self> {
        let height = self.height;
        let width = self.width;

        let mut left: Vec<Self> = (0..height)
            .into_iter()
            .map(|r| {
                let mut contraption = self.clone();
                contraption.add_initial_position((
                    Direction::East,
                    Position {
                        row: r,
                        col: 0,
                        height,
                        width,
                    },
                ));
                contraption
            })
            .collect();

        let mut right: Vec<Self> = (0..height)
            .into_iter()
            .map(|r| {
                let mut contraption = self.clone();
                contraption.add_initial_position((
                    Direction::West,
                    Position {
                        row: r,
                        col: width - 1,
                        height,
                        width,
                    },
                ));
                contraption
            })
            .collect();

        let mut top: Vec<Self> = (0..width)
            .into_iter()
            .map(|col| {
                let mut contraption = self.clone();
                contraption.add_initial_position((
                    Direction::South,
                    Position {
                        row: 0,
                        col: col,
                        height,
                        width,
                    },
                ));
                contraption
            })
            .collect();

        let mut bottom: Vec<Self> = (0..width)
            .into_iter()
            .map(|col| {
                let mut contraption = self.clone();
                contraption.add_initial_position((
                    Direction::North,
                    Position {
                        row: contraption.height - 1,
                        col: col,
                        height,
                        width,
                    },
                ));
                contraption
            })
            .collect();

        let mut start = vec![];
        start.append(&mut left);
        start.append(&mut top);
        start.append(&mut right);
        start.append(&mut bottom);
        start
    }
}

fn main() {
    let input = read_input("../../input/day16");
    part1(input.clone());
    part2(input);
}

fn part1(input: String) {
    let mut contraption = Contraption::new(input);

    contraption.add_initial_position((
        Direction::East,
        Position {
            row: 0,
            col: 0,
            height: contraption.height,
            width: contraption.width,
        },
    ));

    contraption.process_pending();
    let count = contraption.get_energized_count();

    println!("Answer for Part 1: {count:?}")
}

fn part2(input: String) {
    let contraption = Contraption::new(input);

    let count = contraption
        .get_border_starting_positions()
        .iter_mut()
        .map(|contraption| {
            contraption.process_pending();
            contraption.get_energized_count()
        })
        .max()
        .unwrap();

    println!("Answer for Part 2: {count:?}")
}

fn read_input(path: &str) -> String {
    fs::read_to_string(path).unwrap()
}
