use std::fs;

enum List<'a, T: Copy + Clone + std::cmp::PartialEq> {
    Empty,
    Cons(T, Box<&'a List<'a, T>>, usize),
}

impl<'a, T: std::cmp::PartialEq + Clone> List<'a, T> {
    fn new() -> Self {
        List::Empty
    }

    fn clone(&self) -> Self {
        match self {
            List::Empty => List::Empty,
            List::Cons(t1, b, l) => List::Cons(t1.clone(), b.clone(), *l),
        }
    }

    fn push(&'a self, t: T) -> Self {
        List::Cons(t, Box::new(self), self.len() + 1)
    }

    fn len(&self) -> usize {
        match self {
            List::Empty => 0,
            List::Cons(_, _, l) => *l,
        }
    }

    fn contains(&self, t: &T) -> bool {
        match self {
            List::Empty => false,
            List::Cons(t1, b, l) => {
                if t1 == t {
                    true
                } else {
                    b.contains(t)
                }
            }
        }
    }

    fn position(&self, t: T) -> Option<usize> {
        match self {
            List::Empty => None,
            List::Cons(t1, b, l) => {
                if *t1 == t {
                    Some(0)
                } else {
                    b.position(t).and_then(|l| Some(l + 1))
                }
            }
        }
    }
}

fn main() {
    let ls: Vec<Vec<char>> = fs::read_to_string("../../input/day23")
        .unwrap()
        .lines()
        .map(|l| l.chars().collect())
        .collect();

    let visited = List::Empty;
    let mut max_visited = None;

    let mut max_steps = 0;
    // let mut max_steps_by_location: HashMap<(usize, usize), usize> = HashMap::new();

    reach_end(&ls, (0, 1), 1, &mut max_steps, visited, &mut max_visited);

    println!("{max_steps:?}");
}

fn reach_end<'a, 'b>(
    cells: &Vec<Vec<char>>,
    (r, c): (usize, usize),
    steps: usize,
    max_steps: &mut usize,
    visited: List<'a, (usize, usize)>,
    max_visited: &'a mut Option<List<(usize, usize)>>,
) {
    if !visited.contains(&(r, c)) {
        if (r, c) == (cells.len() - 1, cells[0].len() - 2) {
            if steps > *max_steps {
                *max_steps = steps;

                let visited = visited.push((r, c)).clone();
                *max_visited = Some(visited);
                println!("max_steps: {steps:?}");
            } else {
                panic!("was missing?!")
            }
        } else {
            if let Some(max_visited_inner) = max_visited {
                panic!("Hello")

                // if let Some(s) = max_visited_inner
                //     .iter()
                //     .position(|(r1, c1)| (*r1, *c1) == (r, c))
                // {
                //     if steps < s {
                //         return;
                //     } else if steps > s {
                //         let diff = s - steps;

                //         let mut suffix: Vec<_> = max_visited_inner
                //             .iter()
                //             .skip_while(|(r1, c1)| (r, c) != (*r1, *c1))
                //             .map(|rc| *rc)
                //             .collect();

                //         let mut visited = visited.clone();
                //         visited.append(&mut suffix);

                //         *max_steps += diff;

                //         *max_visited = Some(visited);
                //         return;
                //     }
                // }
            }

            let visited = visited.push((r, c));

            if let Some(max_visited_inner) = max_visited {
                if max_visited_inner.contains(&(r + 1, c)) {
                    panic!("Hello")
                }
            }

            if r < cells.len() - 1 && cells[r + 1][c] != '#' && !visited.contains(&(r + 1, c)) {
                reach_end(
                    cells,
                    (r + 1, c),
                    steps + 1,
                    max_steps,
                    visited,
                    max_visited,
                )
            }

            if let Some(max_visited_inner) = max_visited {
                if max_visited_inner.contains(&(r - 1, c)) {
                    panic!("Hello")
                }
            }

            if r > 0 && cells[r - 1][c] != '#' && !visited.contains(&(r - 1, c)) {
                reach_end(
                    cells,
                    (r - 1, c),
                    steps + 1,
                    max_steps,
                    visited,
                    max_visited,
                )
            }

            if c < cells[0].len() - 1 && cells[r][c + 1] != '#' && !visited.contains(&(r, c + 1)) {
                reach_end(
                    cells,
                    (r, c + 1),
                    steps + 1,
                    max_steps,
                    visited,
                    max_visited,
                )
            }

            if c > 0 && cells[r][c - 1] != '#' && !visited.contains(&(r, c - 1)) {
                reach_end(
                    cells,
                    (r, c - 1),
                    steps + 1,
                    max_steps,
                    visited,
                    max_visited,
                )
            }
        }
    } else {
        if let Some(max_visited_inner) = max_visited {
            // if let Some(s) = max_visited_inner
            //     .iter()
            //     .position(|(r1, c1)| (*r1, *c1) == (r, c))
            // {
            //     if steps < s {
            //         return;
            //     } else if steps > s {
            //         let diff = s - steps;

            //         let mut suffix: Vec<_> = max_visited_inner
            //             .iter()
            //             .skip_while(|(r1, c1)| (r, c) != (*r1, *c1))
            //             .map(|rc| *rc)
            //             .collect();

            //         let mut visited = visited.clone();
            //         visited.append(&mut suffix);

            //         *max_steps += diff;

            //         *max_visited = Some(visited);
            //         return;
            //     }
            // }
        }
    }
}
