
// pub type Position = (usize, usize);

// pub trait CellHandler {
//     fn new(dpos: Position, ch: char) -> Self;
//     fn is_start(&self) -> bool;
//     fn is_end(&self) -> bool;
//     fn is_wall(&self) -> bool;
//     fn is_empty(&self) -> bool;
// }

// pub trait ArrayHandler<TCell> {
//     fn get_size(&self) -> (usize, usize);
//     fn in_bounds(&self, pos: Position) -> bool;
//     fn get(&self, pos: Position) -> Option<&TCell>;
//     fn get_start_pos(&self) -> Position;
//     fn get_end_pos(&self) -> Position;
// }

// #[derive(Debug)]
// pub struct Array2D<TCell: CellHandler> {
//     pub data: Vec<Vec<TCell>>,
//     start_pos: Position,
//     end_pos: Position,
// }

// impl <TCell: CellHandler> Array2D<TCell> {
//     pub fn new(data: &str) -> Self {
//         let data: Vec<Vec<TCell>> = data
//             .split('\n')
//             .enumerate()
//             .map(|(r, line)| {
//                 line.chars()
//                     .into_iter()
//                     .enumerate()
//                     .map(|(c, ch)| TCell::new((r, c), ch))
//                     .collect()
//             })
//             .collect();
//         let rc = data.len();
//         let cc = data[0].len();

//         let (mut sr, mut sc, mut er, mut ec) = (0, 0, rc - 1, cc - 1);

//         let mut row_iter = data.iter().enumerate();

//         while let Some((r, row)) = row_iter.next() {
//             let mut cell_iter = row.iter().enumerate();
//             while let Some((c, cell)) = cell_iter.next() {
//                 if cell.is_start() {
//                     sr = r;
//                     sc = c;
//                 } else if cell.is_end() {
//                     er = r;
//                     ec = c;
//                 }
//             }
//         }

//         Self {
//             data,
//             start_pos: (sr, sc),
//             end_pos: (er, ec),
//         }
//     }

//     pub fn navigate_to_end(
//         &self,
//         (r, c): (usize, usize),
//         path: Vec<(usize, usize)>,
//         shortest_path: &mut Option<Vec<(usize, usize)>>,
//     ) {
//         let valid = matches! (shortest_path, Some(shortest) if shortest.len() <= path.len());

//         if shortest_path.is_none() || valid {
//             if (r, c) == self.end_pos {
//                 if let Some(shortest) = shortest_path {
//                     if path.len() < shortest.len() {
//                         *shortest_path = Some(path);
//                     }
//                 } else {
//                     *shortest_path = Some(path);
//                 }
//             } else {
//                 let adjacent_positions = self.find_adjacent_positions((r, c));

//                 for (r, c) in adjacent_positions.into_iter() {
//                     if !path.contains(&(r, c)) {
//                         let mut path = path.clone();
//                         path.push((r, c));
//                         self.navigate_to_end((r, c), path, shortest_path);
//                     }
//                 }
//             }
//         }
//     }

//     fn find_adjacent_positions(&self, (r, c): (usize, usize)) -> Vec<(usize, usize)> {
//         let adjacent_positions: Vec<(usize, usize)> = match (r, c) {
//             (0, 0) => vec![(1, 0), (0, 1)],
//             (0, _) => vec![(1, c), (0, c + 1), (0, c - 1)],
//             (_, 0) => vec![(r + 1, 0), (r - 1, 0), (r, 1)],
//             _ => vec![(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)],
//         };

//         adjacent_positions
//             .into_iter()
//             .filter(|(r1, c1)| {
//                 self.get((*r1, *c1))
//                     .map(|cell| !cell.is_wall())
//                     .unwrap_or(false)
//             })
//             .collect()
//     }

//     fn is_cheat_ok(
//         &self,
//         (r1, c1): (usize, usize),
//         (r2, c2): (usize, usize),
//         cheat_count: usize,
//     ) -> bool {
//         if r1 == r2 && c2.abs_diff(c1) == cheat_count {
//             if c2 > c1 {
//                 self.get((r1, c2 - 1)).is_some_and(|cell| cell.is_wall())
//             } else {
//                 self.get((r1, c1 - 1)).is_some_and(|cell| cell.is_wall())
//             }
//         } else if c1 == c2 && r2.abs_diff(r1) == cheat_count {
//             if r2 > r1 {
//                 self.get((r2 - 1, c1)).is_some_and(|cell| cell.is_wall())
//             } else {
//                 self.get((r1 - 1, c1)).is_some_and(|cell| cell.is_wall())
//             }
//         } else {
//             false
//         }
//     }
// }

// impl <TCell: CellHandler> ArrayHandler<TCell> for Array2D<TCell> {
//     fn get_size(&self) -> (usize, usize) {
//         (self.data.len(), self.data.first().unwrap().len())
//     }

//     fn in_bounds(&self, (r, c): Position) -> bool {
//         let (rc, cc) = self.get_size();
//         return r < rc && c < cc;
//     }

//     fn get(&self, (r, c): (usize, usize)) -> Option<&TCell> {
//         if r < self.data.len() {
//             if c < self.data[r].len() {
//                 Some(&self.data[r][c])
//             } else {
//                 None
//             }
//         } else {
//             None
//         }
//     }
    
//     fn get_start_pos(&self) -> Position {
//         self.start_pos
//     }
    
//     fn get_end_pos(&self) -> Position {
//         self.end_pos
//     }

    
// }
