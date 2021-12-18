use core::*;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        15
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

#[derive(Debug)]
struct Position {
    risk_level: usize,
    row: usize,
    col: usize,
    from_left: Option<usize>,
    from_top: Option<usize>,
    from_bottom: Option<usize>,
    from_right: Option<usize>,
    smallest: usize,
    size: usize,
}

impl Position {
    fn new(risk_level: usize, row: usize, col: usize, size: usize) -> Self {
        if row == 0 && col == 0 {
            Position {
                risk_level,
                row,
                col,
                from_left: Some(0),
                from_right: Some(0),
                from_top: Some(0),
                from_bottom: Some(0),
                smallest: 0,
                size,
            }
        } else {
            Position {
                risk_level,
                row,
                col,
                from_left: None,
                from_right: None,
                from_top: None,
                from_bottom: None,
                smallest: 0,
                size,
            }
        }
    }

    fn get_minimum_risk_level(&self) -> Option<usize> {
        if self.row == 0 && self.col == 0 {
            Some(0)
        } else {
            [
                self.from_left,
                self.from_top,
                self.from_bottom,
                self.from_right,
            ]
            .iter()
            .filter(|x| x.is_some())
            .map(|x| x.unwrap())
            .min()
            .clone()
        }
    }

    fn process_from_right(&mut self, smallest: Option<usize>) {
        if let Some(smallest) = smallest {
            if self.col < self.size - 1 {
                if let Some(from_right) = self.from_right {
                    if smallest < from_right {
                        self.from_right = Some(smallest + self.risk_level)
                    }
                } else {
                    self.from_right = Some(smallest + self.risk_level);
                }
            }
        }
    }

    fn process_from_bottom(&mut self, smallest: Option<usize>) {
        if let Some(smallest) = smallest {
            if self.row < self.size - 1 {
                if let Some(from_bottom) = self.from_bottom {
                    if smallest < from_bottom {
                        self.from_bottom = Some(smallest + self.risk_level)
                    }
                } else {
                    self.from_bottom = Some(smallest + self.risk_level);
                }
            }
        }
    }

    fn process_from_left(&mut self, smallest: Option<usize>) {
        if let Some(smallest) = smallest {
            if self.col > 0 {
                if let Some(from_left) = self.from_left {
                    if smallest < from_left {
                        self.from_left = Some(smallest + self.risk_level)
                    }
                } else {
                    self.from_left = Some(smallest + self.risk_level);
                }
            }
        }
    }

    fn process_from_top(&mut self, smallest: Option<usize>) {
        if let Some(smallest) = smallest {
            if self.row > 0 {
                if let Some(from_top) = self.from_top {
                    if smallest < from_top {
                        self.from_top = Some(smallest + self.risk_level)
                    }
                } else {
                    self.from_top = Some(smallest + self.risk_level);
                }
            }
        }
    }
}

#[derive(Debug)]
struct Cavern {
    positions: Vec<Vec<Position>>,
    size: usize,
}

impl Cavern {
    fn get_bigger_positions(positions: Vec<Vec<Position>>, size: usize) -> Vec<Vec<Position>> {
        let mut larger_positions: Vec<Vec<Position>> = vec![];

        let mut ri = 0;

        for rt in 0..5 {
            for row in positions.iter() {
                let mut larger_row: Vec<Position> = vec![];
                let mut ci = 0;
                for ct in 0..5 {
                    for pos in row.iter() {
                        let new_risk_level = pos.risk_level + rt + ct;
                        let new_risk_level = if new_risk_level > 9 {
                            new_risk_level - 9
                        } else {
                            new_risk_level
                        };

                        let pos = Position::new(new_risk_level, ri, ci, size);
                        larger_row.push(pos);
                        ci += 1;
                    }
                }
                larger_positions.push(larger_row);
                ri += 1;
            }
        }

        larger_positions
    }

    fn new(positions: Vec<Vec<Position>>) -> Self {
        let size = positions.len() * 5;
        let positions = Cavern::get_bigger_positions(positions, size);
        Cavern { positions, size }
    }

    fn process(&mut self) {
        for ind in 0..self.size {
            for r in 0..self.size {
                let c = ind;
                if r > 0 {
                    let smallest_from_top = self.positions[r - 1][c].get_minimum_risk_level();
                    self.positions[r][c].process_from_top(smallest_from_top);
                }
                if c > 0 {
                    let smallest_from_left = self.positions[r][c - 1].get_minimum_risk_level();
                    self.positions[r][c].process_from_left(smallest_from_left);
                }
            }

            for c in 0..self.size {
                let r = ind;
                if r > 0 {
                    let smallest_from_top = self.positions[r - 1][c].get_minimum_risk_level();
                    self.positions[r][c].process_from_top(smallest_from_top);
                }
                if c > 0 {
                    let smallest_from_left = self.positions[r][c - 1].get_minimum_risk_level();
                    self.positions[r][c].process_from_left(smallest_from_left);
                }
            }

            for c in 0..self.size {
                let r = ind;
                if c > 0 {
                    let smallest_from_right =
                        self.positions[r][self.size - c].get_minimum_risk_level();
                    self.positions[r][self.size - c - 1].process_from_right(smallest_from_right);
                }
            }

            for r in 0..self.size {
                let c = ind;
                if r > 0 {
                    let smallest_from_bottom = self.positions[r][c].get_minimum_risk_level();
                    self.positions[r - 1][c].process_from_bottom(smallest_from_bottom);
                }
            }
        }
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let size = input.lines().count();

        let positions: Vec<Vec<Position>> = input
            .lines()
            .enumerate()
            .map(|(row, l)| {
                l.chars()
                    .enumerate()
                    .map(|(col, c)| {
                        let risk_level = c as usize - 48;
                        Position::new(risk_level, row, col, size)
                    })
                    .collect()
            })
            .collect();

        let mut cavern = Cavern::new(positions);
        let size = cavern.size;

        cavern.process();

        let pos = &cavern.positions[size - 1][size - 1];

        pos.get_minimum_risk_level().unwrap().to_string()
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
