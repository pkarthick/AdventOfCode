use std::{collections::HashSet, fs, ops::Range};

#[derive(Debug, Clone)]
struct Instruction {
    direction: char,
    size: usize,
    color: String,
}

impl Instruction {
    fn new(l: &str) -> Self {
        let mut iter = l.split(' ').into_iter();
        let direction = iter.next().unwrap().chars().nth(0).unwrap();
        let size = iter.next().unwrap().parse::<usize>().unwrap();
        let color = iter.next().unwrap().to_string();
        Self {
            direction,
            size,
            color,
        }
    }
}

fn find_connected_cells(
    (r, c): (i32, i32),
    perimeter: &Vec<Range<(i32, i32)>>,
    visited: &mut HashSet<(i32, i32)>,
) {
    if !perimeter
        .iter()
        .any(|p| p.contains(&(r, c)) && !visited.contains(&(r, c)))
    {
        visited.insert((r, c));
        find_connected_cells((r + 1, c), perimeter, visited);
        find_connected_cells((r - 1, c), perimeter, visited);
        find_connected_cells((r, c + 1), perimeter, visited);
        find_connected_cells((r, c - 1), perimeter, visited);
    }
}

fn hexa_to_decimal(s: &str) -> (i32, u32) {
    let decimal = s
        .chars()
        .take(5)
        .fold(0, |acc, ch| acc * 16 + ch.to_digit(16).unwrap());
    let direction = s.chars().nth(5).unwrap().to_digit(10).unwrap();
    (decimal as i32, direction)
}

fn simplify(
    ins: &mut Vec<Instruction>,
    prefix: &mut Vec<Instruction>,
    extra: usize,
) -> Vec<Instruction> {
    if ins.len() == 4 {
        ins.to_vec()
    } else {
        match ins.as_slice() {
            [Instruction {
                direction: 'L',
                size: lsize,
                color: _,
            }, Instruction {
                direction: 'D',
                size: dsize,
                color: _,
            }, Instruction {
                direction: 'R',
                size: rsize,
                color: _,
            }, ..] => {
                if lsize == rsize {
                    println!("");
                    ins.remove(0);
                    ins.remove(1);
                    simplify(ins, prefix, extra + (dsize + 1) * lsize)
                } else {
                    simplify(ins, prefix, extra)
                }
            }
            [_, ..] => {
                prefix.push(ins.remove(0));
                simplify(ins, prefix, extra)
            }
            [] => simplify(prefix, &mut vec![], extra),
        }
    }
}

fn main() {
    let input = fs::read_to_string("../../input/day18s").unwrap();

    let mut max_row: i32 = 0;
    let mut max_col: i32 = 0;
    let mut min_row: i32 = 0;
    let mut min_col: i32 = 0;
    let mut row = 0_i32;
    let mut col = 0_i32;
    let mut perimeter = vec![];

    let mut ins: Vec<_> = input.lines().map(Instruction::new).collect();

    let simplified = simplify(&mut ins, &mut vec![], 0);

    for l in input.lines() {
        let ins = Instruction::new(l);
        // let (r, c) = (row, col);

        // let (size, direction) = hexa_to_decimal(&ins.color[2..ins.color.len() - 1].to_string());

        let (r, c, color) = (row, col, &ins.color[1..ins.color.len() - 2].to_string());

        let size = ins.size as i32;
        let direction = match ins.direction {
            'R' => 0,
            'D' => 1,
            'L' => 2,
            'U' => 3,
            _ => panic!(),
        };

        match direction {
            0 => {
                perimeter.push((r, c)..(r, c + size));
                col += size as i32;
            }
            2 => {
                perimeter.push((r, col - (size - 1))..(r, col + 1));
                col -= size as i32;
            }
            3 => {
                perimeter.push((r - (size - 1), col)..(r + 1, col));
                row -= size as i32;
            }
            1 => {
                perimeter.push((r, c)..(r + size, c));
                row += size as i32;
            }
            _ => panic!("Unexpected direction!"),
        }

        if col > max_col {
            max_col = col;
        } else if col < min_col {
            min_col = col;
        }

        if row > max_row {
            max_row = row;
        } else if row < min_row {
            min_row = row;
        }
    }

    let mut visited: HashSet<(i32, i32)> = HashSet::new();

    find_connected_cells((-1, 461935), &perimeter, &mut visited);

    let l = perimeter.len() + visited.len();
    println!("{l:?}");
}
