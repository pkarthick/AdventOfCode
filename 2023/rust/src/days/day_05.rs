use std::{ops::Range, str::FromStr};

pub struct Input {
    seeds: Vec<Range<i64>>,
    sections: Vec<Section>,
}

#[derive(Clone)]
pub struct Line {
    dest: Range<i64>,
    src: Range<i64>,
}

impl Line {
    fn new(s: &str) -> Self {
        let nums: Vec<i64> = s
            .split_ascii_whitespace()
            .map(|x| x.parse::<i64>().unwrap())
            .collect();
        Line {
            dest: nums[0]..nums[0] + nums[2],
            src: nums[1]..nums[1] + nums[2],
        }
    }

    fn map_destination(&self, seed: &Range<i64>) -> (Vec<Range<i64>>, Vec<Range<i64>>) {
        if seed.end <= self.src.start || seed.start >= self.src.end {
            (vec![], vec![seed.clone()])
        } else {
            let offset = self.dest.start - self.src.start;
            match (seed.start.cmp(&self.src.start), seed.end.cmp(&self.src.end)) {
                (std::cmp::Ordering::Less, std::cmp::Ordering::Less) => (
                    vec![self.dest.start..seed.end + offset],
                    vec![seed.start..self.src.start],
                ),
                (std::cmp::Ordering::Less, std::cmp::Ordering::Equal) => {
                    (vec![self.dest.clone()], vec![seed.start..self.src.start])
                }
                (std::cmp::Ordering::Less, std::cmp::Ordering::Greater) => (
                    vec![self.dest.clone()],
                    vec![(seed.start..self.src.start), (self.src.end..seed.end)],
                ),
                (std::cmp::Ordering::Equal, std::cmp::Ordering::Less) => (
                    vec![self.dest.start..self.dest.start + (seed.end - seed.start)],
                    vec![],
                ),
                (std::cmp::Ordering::Equal, std::cmp::Ordering::Equal) => {
                    (vec![self.dest.clone()], vec![])
                }
                (std::cmp::Ordering::Equal, std::cmp::Ordering::Greater) => {
                    (vec![self.dest.clone()], vec![(self.src.end..seed.end)])
                }
                (std::cmp::Ordering::Greater, std::cmp::Ordering::Less) => (
                    vec![
                        (self.dest.start + (seed.start - self.src.start)
                            ..self.dest.start + (seed.end - self.src.start)),
                    ],
                    vec![],
                ),
                (std::cmp::Ordering::Greater, std::cmp::Ordering::Equal) => (
                    vec![(self.dest.start + (seed.start - self.src.start)..self.dest.end)],
                    vec![],
                ),
                (std::cmp::Ordering::Greater, std::cmp::Ordering::Greater) => (
                    vec![(self.dest.start + (seed.start - self.src.start)..self.dest.end)],
                    vec![(self.src.end..seed.end)],
                ),
            }
        }
    }
}

impl FromStr for Line {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Line::new(s))
    }
}

pub struct Section {
    lines: Vec<Line>,
}

impl Section {
    fn new(sec: &&str) -> Self {
        let lines = sec
            .lines()
            .skip(1)
            .map(|l| l.parse::<Line>().unwrap())
            .collect();
        Section { lines }
    }

    fn lowest_by_range(&self, r: Range<i64>) -> (bool, Vec<Range<i64>>) {
        let mut mapped = vec![];
        let mut unmapped = vec![r.clone()];
        let mut lines = self.lines.clone();

        for l in lines.drain(..) {
            let mut left_over = vec![];
            while let Some(r) = unmapped.pop() {
                let (mut m, mut u) = l.map_destination(&r);
                mapped.append(&mut m);
                left_over.append(&mut u);
            }
            unmapped.append(&mut left_over);
            if unmapped.is_empty() {
                break;
            }
        }

        let no_mapping = mapped.is_empty();
        mapped.append(&mut unmapped);
        (no_mapping, mapped)
    }
}

impl Input {
    fn new(s: &str) -> Self {
        let sections: Vec<&str> = s.split("\n\n").into_iter().collect();

        let seeds: Vec<i64> = sections[0]
            .lines()
            .nth(0)
            .unwrap()
            .split_ascii_whitespace()
            .skip(1)
            .map(|x| x.trim().parse::<i64>().unwrap())
            .collect();

        let seeds = seeds
            .chunks(2)
            .into_iter()
            .map(|c| c[0]..c[0] + c[1])
            .collect();

        let sections: Vec<Section> = sections[1..].iter().map(Section::new).collect();

        Self { seeds, sections }
    }

    fn lowest_location(&self) -> i64 {
        let ranges = self.seeds.clone();

        let location_ranges = self.sections.iter().fold(ranges, |mut ranges, section| {
            ranges
                .drain(..)
                .flat_map(|r| {
                    let (no_mapping, section_output) = section.lowest_by_range(r.clone());
                    if no_mapping {
                        vec![r]
                    } else {
                        section_output
                    }
                })
                .collect()
        });

        location_ranges.iter().map(|r| r.start).min().unwrap()
    }
}

impl FromStr for Input {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Input::new(s))
    }
}

impl crate::day::DayTrait<Input, Input> for Day {
    fn day_number(&self) -> &'static str {
        "05"
    }

    fn part1_input(&self) -> &'static str {
        "day05"
    }

    fn part2_input(&self) -> &'static str {
        "day05"
    }

    fn part1(&self, _input: Input) -> String {
        "<<result>>".to_string()
    }

    fn part2(&self, input: Input) -> String {
        input.lowest_location().to_string()
    }
}

pub struct Day {}
