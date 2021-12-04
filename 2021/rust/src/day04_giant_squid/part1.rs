use core::*;

struct PartOne {}
impl PartSpec for PartOne {
    fn get_day(&self) -> i32 {
        4
    }
    fn get_part_kind(&self) -> PartKind {
        PartKind::One
    }
}
#[derive(Debug)]
struct LuckyNumber {
    num: usize,
    marked: bool,
}
struct Board {
    rows: Vec<Vec<LuckyNumber>>,
    unmarked: usize,
}

impl Board {
    fn new(lines: Vec<&&str>) -> Self {
        let rows = lines
            .into_iter()
            .map(|s| {
                s.split(' ')
                    .filter(|t| t.len() > 0)
                    .map(|n| LuckyNumber {
                        num: n.parse::<usize>().unwrap(),
                        marked: false,
                    })
                    .collect::<Vec<LuckyNumber>>()
            })
            .collect::<Vec<Vec<LuckyNumber>>>();

        let unmarked: usize = rows
            .iter()
            .map(|r| r.into_iter().map(|ln| ln.num).sum::<usize>())
            .sum();

        Board { rows, unmarked }
    }

    fn mark_board(&mut self, lucky_num: usize) -> Option<usize> {
        let mut row_complete = false;
        let mut col_index = None;

        for row in self.rows.iter_mut() {
            let mut all = true;

            for (ci, ln) in row.iter_mut().enumerate().filter(|(_, ln)| !ln.marked) {
                if ln.num == lucky_num {
                    ln.marked = true;
                    self.unmarked -= ln.num;
                    col_index = Some(ci);
                } else {
                    all = false;
                }
            }

            if all {
                row_complete = true;
            }
        }

        if row_complete {
            return Some(self.unmarked * lucky_num);
        } else {
            if let Some(ci) = col_index {
                if self.rows.iter().map(|r| &r[ci]).all(|ln| ln.marked) {
                    return Some(self.unmarked * lucky_num);
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
    fn new(input: String) -> Self {
        let lines = input.lines().collect::<Vec<&str>>();
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
        for ln in self.lucky_numbers.iter() {
            for board in self.boards.iter_mut() {
                if let Some(total) = board.mark_board(*ln) {
                    return Some(total);
                }
            }
        }

        None
    }
}

impl TestPart for PartOne {
    fn process_input(&self, input: String) -> String {
        let mut bingo = Bingo::new(input);
        bingo.get_final_score().unwrap().to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let part = PartOne {};
        assert!(part.test_sample().is_ok());
        assert!(part.test_puzzle().is_ok());
    }
}
