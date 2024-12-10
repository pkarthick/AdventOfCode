use std::{
    collections::{HashMap, HashSet}, str::FromStr
};

use crate::day::AoCDay;

pub struct Day {
    antennas: HashMap<char, Vec<(i8, i8)>>,
    row_count: usize,
    col_count: usize,
    antenna_locations: HashSet<(i8, i8)>,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut antennas = HashMap::new();
        let mut antenna_locations: HashSet<(i8, i8)> = HashSet::new();

        let lines: Vec<&str> = s.split('\n').collect();
        let row_count = lines.len();
        let col_count = lines[0].len();

        for (r, line) in lines.into_iter().enumerate() {
            for (c, ch) in line.chars().enumerate().filter(|(_, ch)| *ch != '.') {
                let rc = (r as i8, c as i8);
                antennas
                    .entry(ch)
                    .and_modify(|v: &mut Vec<(i8, i8)>| v.push(rc))
                    .or_insert(vec![rc]);
                antenna_locations.insert(rc);
            }
        }

        Ok(Day {
            antennas,
            row_count,
            col_count,
            antenna_locations,
        })
    }
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }

    fn combinations(locs: &Vec<(i8, i8)>) -> Vec<((i8, i8), (i8, i8))> {
        let mut combs = vec![];

        for i in 0..locs.len() {
            for j in i + 1..locs.len() {
                combs.push((locs[i], locs[j]));
            }
        }

        combs
    }

    fn get_antinode_locations(&self, part1: bool) -> HashSet<(i8, i8)> {
        let mut uniq: HashSet<(i8, i8)> = HashSet::new();

        for (_a, locs) in self.antennas.iter() {
            for ((r1, c1), (r2, c2)) in Day::combinations(locs) {

                let rd = r1 - r2;
                let cd = c1 - c2;

                self.insert_antinodes(r1, c1, rd, cd, &mut uniq, part1);
                self.insert_antinodes(r2, c2, -rd, -cd, &mut uniq, part1);

            }
        }

        return uniq;
    }

    fn insert_antinodes(&self, r: i8, c: i8, rd: i8, cd: i8, uniq: &mut HashSet<(i8, i8)>, part1: bool) {
        let mut rr = r;
        let mut cc = c;

        loop {
            rr += rd;
            cc += cd;

            if 0 <= rr && rr < self.row_count as i8 && 0 <= cc && cc < self.col_count as i8 {
                uniq.insert((rr, cc));
            } else {
                break;
            }

            if part1 {
                break;
            }
        }
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        self.get_antinode_locations(true).len().to_string()
    }

    fn part2(&mut self) -> String {
        self.get_antinode_locations(false).union(&self.antenna_locations).count().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1_input() {
        let actual = Day::new(INPUT).part1();
        assert_eq!(actual, "14");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(INPUT).part2();
        assert_eq!(actual, "34");
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

    const PUZZLE_INPUT: &str = include_str!("../data/day08");

    const INPUT: &str = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............";
}
