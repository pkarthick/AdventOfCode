pub struct Grid<Cell: Copy> {
    pub cells: Vec<Vec<Cell>>,
}

pub enum HDir {
    Left,
    Right,
}

pub enum VDir {
    Up,
    Down,
}

impl<Cell: Copy> Grid<Cell> {
    pub fn new(input: &str, mapper: fn(s: &str) -> Vec<Cell>) -> Grid<Cell> {
        let cells: Vec<Vec<Cell>> = input.split('\n').map(mapper).collect();
        Grid { cells }
    }

    pub fn read_column(&self, ci: usize) -> Vec<Cell> {
        (0..self.cells.len()).map(|ri| self.cells[ri][ci]).collect()
    }

    pub fn read_horizontal(
        &self,
        row: usize,
        column: usize,
        direction: HDir,
        count: usize,
    ) -> Vec<Cell> {
        match direction {
            HDir::Right => (column..column + count)
                .filter(|c| *c < self.cells[row].len())
                .map(|c| self.cells[row][c])
                .collect(),
            HDir::Left => {
                let c = if column >= count - 1 {
                    column - count + 1
                } else {
                    0
                };
                (c..c + count)
                    .filter(|c| *c < self.cells[row].len())
                    .map(|c| self.cells[row][c])
                    .collect()
            }
        }
    }

    pub fn read_vertical(
        &self,
        row: usize,
        column: usize,
        direction: VDir,
        count: usize,
    ) -> Vec<Cell> {
        match direction {
            VDir::Down => (row..row + count)
                .filter(|r| *r < self.cells.len())
                .map(|r| self.cells[r][column])
                .collect(),
            VDir::Up => {
                let r = if row >= count - 1 { row - count + 1 } else { 0 };
                (r..r + count)
                    .filter(|r| *r < self.cells.len())
                    .map(|r| self.cells[r][column])
                    .collect()
            }
        }
    }

    pub fn read_diagonal(
        &self,
        row: usize,
        column: usize,
        horizontal_direction: HDir,
        vertical_direction: VDir,
        count: usize,
    ) -> Vec<Cell> {
        let r = row as i32;
        let c = column as i32;
        let count = count as i32;

        let mut result = vec![];

        let (cs, rs) = match (horizontal_direction, vertical_direction) {
            (HDir::Left, VDir::Up) => (-1, -1),
            (HDir::Left, VDir::Down) => (-1, 1),
            (HDir::Right, VDir::Up) => (1, -1),
            (HDir::Right, VDir::Down) => (1, 1),
        };

        let rlen = self.cells.len() as i32;

        for d in 0..count {
            let rr = r + (d * rs);

            if rr >= 0 && rr < rlen {
                let clen = self.cells[rr as usize].len() as i32;

                let cc = c + (d * cs);
                if cc >= 0 && cc < clen {
                    result.push(self.cells[rr as usize][cc as usize]);
                }
            }
        }

        result
    }
}
