use core::*;
use std::collections::HashSet;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        8
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}


impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        input
            .lines()
            .map(|l| {
                let frags: Vec<&str> = l.split(" | ").collect();

                let mut input_segments: Vec<HashSet<char>> = frags[0]
                    .split(' ')
                    .map(|x| x.chars().collect::<HashSet<char>>())
                    .collect();

                input_segments.sort_by(|x, y| x.len().cmp(&y.len()));
                let one = &input_segments[0];
                let seven = &input_segments[1];
                let four = &input_segments[2];
                let eight = &input_segments[9];
                let (nine_index, others) = if four.is_subset(&input_segments[6]) {
                    (6, (7_usize, 8_usize))
                } else if four.is_subset(&input_segments[7]) {
                    (7, (6, 8))
                } else {
                    (8, (6, 7))
                };
                let nine = &input_segments[nine_index];
                let (zero, six) = if one.is_subset(&input_segments[others.0]) {
                    (&input_segments[others.0], &input_segments[others.1])
                } else {
                    (&input_segments[others.1], &input_segments[others.0])
                };
                let (three_index, others) = if one.is_subset(&input_segments[3]) {
                    (3, (4_usize, 5_usize))
                } else if one.is_subset(&input_segments[4]) {
                    (4, (3, 5))
                } else {
                    (5, (3, 4))
                };
                let three = &input_segments[three_index];
                let (five, two) = if four
                    .union(&input_segments[others.0])
                    .map(|x| *x)
                    .collect::<HashSet<char>>()
                    .is_subset(nine)
                {
                    (&input_segments[others.0], &input_segments[others.1])
                } else {
                    (&input_segments[others.1], &input_segments[others.0])
                };

                let all_sets = [zero, one, two, three, four, five, six, seven, eight, nine];

                let pows = [1000, 100, 10, 1];

                frags[1]
                    .split(' ')
                    .map(|x| x.chars().collect::<HashSet<char>>())
                    .enumerate()
                    .fold(0, |acc, (powi, out_seg)| {
                        let (digit, _) = all_sets
                            .iter()
                            .enumerate()
                            .find(|(_, digit_set)| out_seg == ***digit_set)
                            .unwrap();
                        acc + digit * pows[powi]
                    })
            })
            .sum::<usize>()
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let part = PartTwo {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
