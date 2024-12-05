pub struct Grid<Cell: Copy> {
    pub cells: Vec<Vec<Cell>>,
}

pub enum HorizontalDirection {
    Left,
    Right,
}

pub enum VerticalDirection {
    Up,
    Down,
}

impl<Cell: Copy> Grid<Cell> {
    pub fn new(input: &str, mapper: fn(s: &str) -> Vec<Cell>) -> Grid<Cell> {
        let cells: Vec<Vec<Cell>> = input.split('\n').into_iter().map(mapper).collect();
        Grid { cells }
    }

    pub fn read_column(&self,column: usize) -> Vec<Cell> {
        let mut vec = vec![];
        for r in 0..self.cells.len() {
            vec.push(self.cells[r][column]);
        }
        vec
    }

    pub fn read_horizontal(
        &self,
        row: usize,
        column: usize,
        direction: HorizontalDirection,
        count: usize,
    ) -> Vec<Cell> {
        match direction {
            HorizontalDirection::Right => (column..column + count)
                .filter(|c| *c < self.cells[row].len())
                .map(|c| self.cells[row][c])
                .collect(),
            HorizontalDirection::Left => {
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
        direction: VerticalDirection,
        count: usize,
    ) -> Vec<Cell> {
        match direction {
            VerticalDirection::Down => (row..row + count)
                .filter(|r| *r < self.cells.len())
                .map(|r| self.cells[r][column])
                .collect(),
            VerticalDirection::Up => {
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
        horizontal_direction: HorizontalDirection,
        vertical_direction: VerticalDirection,
        count: usize,
    ) -> Vec<Cell> {
        let r = row as i32;
        let c = column as i32;
        let count = count as i32;

        let mut result = vec![];

        let (cs, rs) = match (horizontal_direction, vertical_direction) {
            (HorizontalDirection::Left, VerticalDirection::Up) => (-1, -1),
            (HorizontalDirection::Left, VerticalDirection::Down) => (-1, 1),
            (HorizontalDirection::Right, VerticalDirection::Up) => (1, -1),
            (HorizontalDirection::Right, VerticalDirection::Down) => (1, 1),
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
