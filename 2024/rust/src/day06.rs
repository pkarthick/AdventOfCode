use std::{collections::HashSet, str::FromStr};

use crate::day::AoCDay;

pub struct Day {
    sr: usize,
    sc: usize,
    obstructions: Vec<Vec<bool>>,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut obstructions: Vec<Vec<bool>> = vec![];

        let mut sr = 0;
        let mut sc = 0;

        for (r, line) in s.split("\n").enumerate() {
            let mut row = vec![];
            for (c, ch) in line.char_indices() {
                if ch == '^' {
                    sr = r;
                    sc = c;
                }
                row.push(ch == '#');
            }
            obstructions.push(row);
        }

        Ok(Day {
            sr,
            sc,
            obstructions,
        })
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
enum Direction {
    North,
    East,
    West,
    South,
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }
    fn get_next(&self, r: usize, c: usize, dir: Direction) -> Option<(usize, usize, Direction)> {
        match dir {
            Direction::North if r == 0 => None,
            Direction::North => {
                if !self.obstructions[r - 1][c] {
                    Some((r - 1, c, Direction::North))
                } else {
                    Some((r, c, Direction::East))
                }
            }

            Direction::East if c == self.obstructions[r].len() - 1 => None,
            Direction::East => {
                if !self.obstructions[r][c + 1] {
                    Some((r, c + 1, Direction::East))
                } else {
                    Some((r, c, Direction::South))
                }
            }

            Direction::West if c == 0 => None,
            Direction::West => {
                if !self.obstructions[r][c - 1] {
                    Some((r, c - 1, Direction::West))
                } else {
                    Some((r, c, Direction::North))
                }
            }
            Direction::South if r == self.obstructions.len() - 1 => None,

            Direction::South => {
                if !self.obstructions[r + 1][c] {
                    Some((r + 1, c, Direction::South))
                } else {
                    Some((r, c, Direction::West))
                }
            }
        }
    }
    fn count_visited(
        &self,
        (r, c, d): (usize, usize, Direction),
        visited: &mut HashSet<(usize, usize, Direction)>,
    ) -> bool {
        visited.insert((r, c, d.clone()));

        match self.get_next(r, c, d) {
            None => false,
            Some(rcd) => self.count_visited(rcd, visited),
        }
    }

    fn check_loop(
        &self,
        (r, c, dir): (usize, usize, Direction),
        visited: &mut HashSet<(usize, usize, Direction)>,
    ) -> bool {
        if visited.contains(&(r, c, dir)) {
            return true;
        }

        visited.insert((r, c, dir));

        match self.get_next(r, c, dir) {
            None => false,
            Some((r1, c1, d1)) => {
                let mut rr = r1;
                let mut cc = c1;
                let mut dd = d1;
                while let Some((r2, c2, d2)) = self.get_next(rr, cc, dd) {
                    if dd != d2 {
                        return self.check_loop((r2, c2, d2), visited);
                    } else {
                        rr = r2;
                        cc = c2;
                        dd = d2;
                    }
                }
                false
            }
        }
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        let mut visited: HashSet<(usize, usize, Direction)> = HashSet::new();
        self.count_visited((self.sr, self.sc, Direction::North), &mut visited);
        let hs: HashSet<(usize, usize)> = visited.into_iter().map(|(r, c, _)| (r, c)).collect();
        hs.len().to_string()
    }

    fn part2(&mut self) -> String {
        let mut visited: HashSet<(usize, usize, Direction)> = HashSet::new();
        self.count_visited((self.sr, self.sc, Direction::North), &mut visited);
        let hs: HashSet<(usize, usize)> = visited.into_iter().map(|(r, c, _)| (r, c)).collect();
        let mut count = 0;

        for (r, c) in hs.into_iter() {
            if !self.obstructions[r][c] {
                self.obstructions[r][c] = true;
                let mut visited: HashSet<(usize, usize, Direction)> = HashSet::new();

                if self.check_loop((self.sr, self.sc, Direction::North), &mut visited) {
                    count += 1;
                }
                self.obstructions[r][c] = false;
            }
        }

        count.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...";

    #[test]
    fn test_part1_input() {
        let actual = Day::new(INPUT).part1();
        assert_eq!(actual, "41");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(INPUT).part2();
        assert_eq!(actual, "6");
    }

    #[test]
    fn test_part1_puzzle_input() {
        let mut day = Day::new(PUZZLE_INPUT);
        let actual = day.part1();
        println!("{actual}");
        assert_eq!(true, true);
    }

    #[test]
    fn test_part2_puzzle_input() {
        let mut day = Day::new(PUZZLE_INPUT);
        let actual = day.part2();
        println!("{actual}");
        assert_eq!(true, true);
    }

    const PUZZLE_INPUT: &str = include_str!("../data/day06");
}
