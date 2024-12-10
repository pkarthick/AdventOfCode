use std::str::FromStr;

use crate::day::AoCDay;

struct Block {
    free: bool,
    num: Option<usize>,
    size: usize,
}

pub struct Day {
    blocks: Vec<Block>,
    data: Vec<Option<usize>>,
}

impl FromStr for Day {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut blocks = vec![];
        let mut data = vec![];

        for size in s.chars().map(|c| c.to_digit(10).unwrap() as usize) {
            if blocks.len() % 2 == 0 {
                blocks.push(Block {
                    free: false,
                    num: Some(blocks.len() / 2),
                    size,
                });
                data.extend(
                    std::iter::repeat(Some(blocks.len() / 2))
                        .take(size as usize)
                        .collect::<Vec<Option<usize>>>(),
                );
            } else {
                blocks.push(Block {
                    free: true,
                    num: None,
                    size,
                });
                data.extend(
                    std::iter::repeat(None)
                        .take(size as usize)
                        .collect::<Vec<Option<usize>>>(),
                );
            }
        }

        Ok(Day { blocks, data })
    }
}

impl Day {
    pub fn new(input: &str) -> Self {
        Day::from_str(input).unwrap()
    }

    fn defrag(&mut self) {
        let mut end = self.blocks.len() - 1;

        loop {
            while end > 0 {
                if self.blocks[end].free {
                    if end == 0 {
                        break;
                    }
                    end -= 1;
                } else {
                    break;
                }
            }

            if end == 0 {
                break;
            }

            let mut changed = false;

            for start in 0..end + 1 {
                // let mut self.blocks[start] = &mut self.blocks[start];
                // let mut end_block = &mut self.blocks[end];

                if self.blocks[start].free {
                    if self.blocks[start].size == self.blocks[end].size {
                        changed = true;
                        self.blocks[start].num = self.blocks[end].num;
                        self.blocks[end].num = None;
                        self.blocks[start].free = false;
                        self.blocks[end].free = true;
                        break;
                    } else if self.blocks[start].size > self.blocks[end].size {
                        changed = true;
                        let start_size = self.blocks[start].size;
                        self.blocks[start].num = self.blocks[end].num;
                        self.blocks[end].num = None;
                        self.blocks[start].free = false;
                        self.blocks[end].free = true;
                        self.blocks[start].size = self.blocks[end].size;
                        self.blocks.insert(
                            start + 1,
                            Block {
                                free: true,
                                num: None,
                                size: start_size - self.blocks[end].size,
                            },
                        );
                        break;
                    }
                }
            }
            if !changed {
                end -= 1
            }
        }
    }
}

impl AoCDay for Day {
    fn part1(&mut self) -> String {
        let mut start = 0;
        let mut end = self.data.len() - 1;
        let mut total: usize = 0;

        while start <= end {
            while start <= end && self.data[start].is_some() {
                total += self.data[start].unwrap() as usize * start;
                start += 1;
            }

            while start <= end && self.data[start].is_none() {
                while self.data[end].is_none() {
                    end -= 1;
                }

                total += self.data[end].unwrap() as usize * start;
                start += 1;
                end -= 1;
            }
        }

        total.to_string()
    }

    fn part2(&mut self) -> String {
        self.defrag();

        let mut i = 0;
        let mut total = 0;

        for block in self.blocks.iter() {
            if !block.free {
                for _ in 0..block.size {
                    total += i * block.num.unwrap();
                    i += 1;
                }
            } else {
                i += block.size;
            }
        }

        return total.to_string();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1_input() {
        let actual = Day::new(INPUT).part1();
        assert_eq!(actual, "1928");
    }

    #[test]
    fn test_part2_input() {
        let actual = Day::new(INPUT).part2();
        assert_eq!(actual, "2858");
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

    const PUZZLE_INPUT: &str = include_str!("../data/day09");

    const INPUT: &str = "2333133121414131402";
}
