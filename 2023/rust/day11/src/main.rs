use std::fs;

struct Universe {
    data: Vec<Vec<bool>>,
    galaxy_locations: Vec<(usize, usize)>,
    empty_row_indices: Vec<usize>,
    empty_col_indices: Vec<usize>,
}

impl Universe {
    fn new(contents: String) -> Self {
        let data: Vec<Vec<bool>> = contents
            .split("\n")
            .map(|l| l.chars().map(|c| c == '#').collect())
            .collect();

        let mut universe = Self {
            data,
            galaxy_locations: vec![],
            empty_row_indices: vec![],
            empty_col_indices: vec![],
        };

        universe.populate_galaxy_locations();

        universe
    }

    fn is_col_empty(&self, c: usize) -> bool {
        (0..self.data.len()).all(|r| !self.data[r][c])
    }

    fn is_row_empty(&self, r: usize) -> bool {
        self.data[r].iter().all(|b| !*b)
    }

    fn populate_galaxy_locations(&mut self) {
        for r in 0..self.data.len() {
            for c in 0..self.data[0].len() {
                if self.data[r][c] {
                    self.galaxy_locations.push((r, c))
                }
            }
        }
    }

    fn expand(&mut self) {
        self.empty_row_indices = (0_usize..self.data.len())
            .into_iter()
            .filter(|r| self.is_row_empty(*r))
            .collect();

        self.empty_col_indices = (0_usize..self.data[0].len())
            .into_iter()
            .filter(|c| self.is_col_empty(*c))
            .collect();
    }

    fn distance_between(
        &self,
        times: usize,
        (r1, c1): (usize, usize),
        (r2, c2): &(usize, usize),
    ) -> usize {
        let (r1, r2) = if r1 >= *r2 { (r1, *r2) } else { (*r2, r1) };
        let (c1, c2) = if c1 >= *c2 { (c1, *c2) } else { (*c2, c1) };

        let empty_rows = self
            .empty_row_indices
            .iter()
            .filter(|r| **r > r2 && **r < r1)
            .count();
        let empty_cols = self
            .empty_col_indices
            .iter()
            .filter(|c| **c > c2 && **c < c1)
            .count();

        let rows_expansion = empty_rows * (times - 1);
        let cols_expansion = empty_cols * (times - 1);

        (r1 - r2) + (c1 - c2) + rows_expansion + cols_expansion
    }

    fn sum_of_distances(&self, times: usize) -> usize {
        let mut total = 0;
        let mut galaxy_locations = self.galaxy_locations.clone();

        while let Some(pair) = galaxy_locations.pop() {
            for pair1 in galaxy_locations.iter() {
                let dis = self.distance_between(times, pair, pair1);
                total += dis;
            }
        }

        total
    }
}

fn main() {
    let contents = fs::read_to_string("../../input/day11").unwrap();

    let mut universe = Universe::new(contents);
    universe.expand();

    let times = 2;
    let total = universe.sum_of_distances(times);

    println!("Answer for Part 1: {total:?}");

    let times = 1000000;
    let total = universe.sum_of_distances(times);

    println!("Answer for Part 2: {total:?}");
}
