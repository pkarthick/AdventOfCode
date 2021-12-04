use core::*;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        4
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}
#[derive(Debug)]
struct LuckyNumber {
    num: usize,
    marked: bool,
}
struct Board {
    rows: Vec<Vec<LuckyNumber>>,
    done: bool
}

impl Board {
    fn new(lines: Vec<&&str>) -> Self {
        let rows = lines
            .into_iter()
            .map(|s| {
                s.split(' ')
                    .filter(|t| t.len() > 0)
                    .map(|x| LuckyNumber {
                        num: x.parse::<usize>().unwrap(),
                        marked: false,
                    })
                    .collect::<Vec<LuckyNumber>>()
            })
            .collect::<Vec<Vec<LuckyNumber>>>();
        Board {
            rows,
            done: false
        }
    }

    fn mark_board(&mut self, lucky_num: usize) -> Option<usize> {
        let mut unmarked = 0_usize;
        let mut row_complete = false;
        let mut col_index = None;

        for row in self.rows.iter_mut() {
            let mut all = true;

            for (ci, ln) in row.iter_mut().enumerate() {
                if !ln.marked {
                    if ln.num == lucky_num {
                        ln.marked = true;
                        col_index = Some(ci);
                    } else {
                        all = false;
                        unmarked += ln.num;
                    }
                }
            }

            if all {
                row_complete = true;
                self.done = true;
            }
        }

        if row_complete {
            return Some(unmarked * lucky_num);
        } else {
            if let Some(ci) = col_index {
                if self.rows.iter().map(|r| &r[ci]).all(|ln| ln.marked) {
                    self.done = true;
                    return Some(unmarked * lucky_num);
                }
            }
        }

        None
    }
}

struct Bingo {
    boards: Vec<Board>,
    lucky_numbers: Vec<usize>,
}

impl Bingo {
    fn new(lines: Vec<&str>) -> Self {
        let lucky_numbers: Vec<usize> = lines[0]
            .split(',')
            .map(|x| x.parse::<usize>().unwrap())
            .collect();
        let mut start = 2;
        let mut boards: Vec<Board> = vec![];
        while start < lines.len() {
            let board = lines.iter().skip(start).take(5).collect::<Vec<&&str>>();
            let board = Board::new(board);
            boards.push(board);
            start += 6;
        }
        Bingo {
            boards,
            lucky_numbers,
        }
    }

    fn get_final_score(&mut self) -> Option<usize> {
        let mut total_boards = self.boards.len();

        for ln in self.lucky_numbers.iter() {
            for board in self.boards.iter_mut() {
                if !board.done {

                    if let Some(total) = board.mark_board(*ln) {
                        total_boards -= 1;

                        if total_boards == 0 {
                            return Some(total);
                        }
                    }
                }
            }
        }

        None
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let mut bingo = Bingo::new(input.lines().collect::<Vec<&str>>());
        bingo.get_final_score().unwrap().to_string()
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
