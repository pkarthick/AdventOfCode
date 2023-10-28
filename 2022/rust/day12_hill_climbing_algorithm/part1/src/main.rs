use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
struct Neighbours {
    left: Option<Elevation>,
    up: Option<Elevation>,
    down: Option<Elevation>,
    right: Option<Elevation>,
}

#[derive(Debug)]
struct Hill {
    elevation: Vec<Vec<char>>,
    neighbours: Vec<Vec<Neighbours>>,
    next_pairs: HashMap<char, HashSet<(usize, usize)>>,
    start_location: (usize, usize),
    end_location: (usize, usize),
    elevation_count: usize,
}

#[derive(Debug, PartialEq, Clone)]
enum Elevation {
    Low,
    Previous,
    Same,
    Next,
    High,
}

fn compare((r1, c1): (usize, usize), (r2, c2): (usize, usize)) -> usize {
    let rd = if r1 > r2 { r1 - r2 } else { r2 - r1 };
    let cd = if c1 > c2 { c1 - c2 } else { c2 - c1 };
    rd + cd
}

fn is_adjacent(a: char, b: char) -> Option<Elevation> {
    if a == b {
        Some(Elevation::Same)
    } else if a == 'S' && b == 'a' {
        Some(Elevation::Same)
    } else if (a == 'S' && b == 'b') || (a == 'z' && b == 'E') {
        Some(Elevation::Next)
    } else {
        let a = a as i8;
        let b = b as i8;

        let elevation = match b - a {
            0 => Elevation::Same,
            -1 => Elevation::Previous,
            1 => Elevation::Next,
            x if x < -1 => Elevation::Low,
            _ => Elevation::High,
        };
        Some(elevation)
    }
}

impl Hill {
    fn render_relative_positions(
        &self,
        relative_positions: &Vec<Vec<usize>>,
        (r1, c1): (usize, usize),
        (r2, c2): (usize, usize),
    ) {
        for (r, row) in relative_positions.iter().enumerate() {
            for (c, cell) in row.iter().enumerate() {
                if r == r1 && c == c1 {
                    print!("^^^,{} ", self.elevation[r][c]);
                } else if r == r2 && c == c2 {
                    print!("$$$,{} ", self.elevation[r][c]);
                } else if *cell == 0 || *cell == self.elevation_count {
                    print!("{:>5} ", "");
                } else {
                    print!("{:>3},{} ", cell, self.elevation[r][c]);
                }
            }
            println!();
        }
        println!("-----------    DONE ---------------");
        println!();
    }

    fn new(lines: Vec<&str>) -> Self {
        let elevation: Vec<Vec<char>> = lines
            .into_iter()
            .map(|l| l.trim().chars().collect())
            .collect();

        let row_count = elevation.len();
        let col_count = elevation[0].len();
        let elevation_count = row_count * col_count;
        let mut start_location = None;
        let mut end_location = None;

        let mut neighbours: Vec<Vec<Neighbours>> = (0..row_count)
            .into_iter()
            .map(|_| {
                (0..col_count)
                    .into_iter()
                    .map(|_| Neighbours {
                        left: None,
                        up: None,
                        down: None,
                        right: None,
                    })
                    .collect()
            })
            .collect();

        let mut next_pairs: HashMap<char, HashSet<(usize, usize)>> = HashMap::new();

        for r in 0..elevation.len() {
            for c in 0..col_count {
                if elevation[r][c] == 'S' {
                    start_location = Some((r, c));
                } else if elevation[r][c] == 'E' {
                    end_location = Some((r, c));
                }

                let mut set = HashSet::new();

                if c > 0 {
                    neighbours[r][c].left = is_adjacent(elevation[r][c], elevation[r][c - 1]);
                    if let Some(Elevation::Next) = neighbours[r][c].left {
                        set.insert((r, c - 1));
                    }
                }

                if r > 0 {
                    neighbours[r][c].up = is_adjacent(elevation[r][c], elevation[r - 1][c]);
                    if let Some(Elevation::Next) = neighbours[r][c].up {
                        set.insert((r - 1, c));
                    }
                }

                if r < row_count - 1 {
                    neighbours[r][c].down = is_adjacent(elevation[r][c], elevation[r + 1][c]);
                    if let Some(Elevation::Next) = neighbours[r][c].down {
                        set.insert((r + 1, c));
                    }
                }

                if c < col_count - 1 {
                    neighbours[r][c].right = is_adjacent(elevation[r][c], elevation[r][c + 1]);
                    if let Some(Elevation::Next) = neighbours[r][c].right {
                        set.insert((r, c + 1));
                    }
                }

                next_pairs
                    .entry(elevation[r][c])
                    .and_modify(|s| *s = s.union(&set).into_iter().map(|(r, c)| (*r, *c)).collect())
                    .or_insert(set);
            }
        }

        next_pairs.insert('E', HashSet::from([end_location.unwrap()]));

        Self {
            elevation,
            elevation_count,
            neighbours,
            next_pairs,
            start_location: start_location.unwrap(),
            end_location: end_location.unwrap(),
        }
    }

    fn find_locations_at_start(&self) -> Vec<(usize, usize)> {
        let mut locations = vec![];

        let (r, c) = self.start_location;

        if r > 0 && self.elevation[r - 1][c] == 'b' {
            locations.push((r - 1, c));
        }

        if r < self.elevation.len() - 1 && self.elevation[r + 1][c] == 'b' {
            locations.push((r + 1, c));
        }

        if c > 0 && self.elevation[r][c - 1] == 'b' {
            locations.push((r, c - 1));
        }

        if c < self.elevation[0].len() - 1 && self.elevation[r][c + 1] == 'b' {
            locations.push((r, c + 1));
        }

        if r > 0 && self.elevation[r - 1][c] == 'a' {
            locations.push((r - 1, c));
        }

        if r < self.elevation.len() - 1 && self.elevation[r + 1][c] == 'a' {
            locations.push((r + 1, c));
        }

        if c > 0 && self.elevation[r][c - 1] == 'a' {
            locations.push((r, c - 1));
        }

        if c < self.elevation[0].len() - 1 && self.elevation[r][c + 1] == 'a' {
            locations.push((r, c + 1));
        }

        locations
    }

    fn find_shortest_between_locations(
        &self,
        (r1, c1): (usize, usize),
        (r2, c2): (usize, usize),
        count: usize,
        shortest: &mut HashMap<(usize, usize), usize>,
    ) {
        if let Some(sh) = shortest.get(&(r2, c2)) {
            if count >= *sh {
                println!("Exiting due to count! {} {}", count, sh);
            }
        }

        let mut pending = HashSet::from([(r1, c1)]);

        let mut relative_positions: Vec<Vec<usize>> = (0..self.elevation.len())
            .into_iter()
            .map(|_| {
                (0..self.elevation[0].len())
                    .into_iter()
                    .map(|_| 0)
                    .collect()
            })
            .collect();

        let mut count = count + 1;

        loop {
            if pending.is_empty() {
                break;
            }

            let mut pending1 = HashSet::new();

            for (r, c) in pending.drain() {
                if (r, c) == (r2, c2) {
                    if !shortest.contains_key(&(r2, c2))
                        || count < *shortest.get(&(r2, c2)).unwrap()
                    {
                        println!(
                            "ALERT: Starting: {:?} Ending: {:?} Count:{}",
                            (r1, c1),
                            (r2, c2),
                            count
                        );
                        // if !shortest.contains_key(&(r2, c2)) {
                        //     shortest.insert((r2, c2), count);
                        // } else if let Some(x) = shortest.get_mut(&(r2, c2)) {
                        //     *x = count;
                        // }
                        // self.render_relative_positions(&relative_positions, (r1, c1), (r2, c2));
                        continue;
                    }
                    // return Some(relative_positions[r][c]);
                }

                let neighbours = &self.neighbours[r][c];
                if r > 0 {
                    match neighbours.up {
                        Some(Elevation::Next)
                        | Some(Elevation::Previous)
                        | Some(Elevation::Low)
                        | Some(Elevation::Same) => {
                            if (relative_positions[r - 1][c] == 0
                                || count < relative_positions[r - 1][c])
                                && (r - 1, c) != (r1, c1)
                            {
                                if (r - 1, c) != (r2, c2) {
                                    pending1.insert((r - 1, c));
                                    relative_positions[r - 1][c] = count;
                                } else {
                                    relative_positions[r - 1][c] = count;
                                    if let Some(shortest) = shortest.get_mut(&(r2, c2)) {
                                        if count < *shortest {
                                            println!(
                                                "Starting: {:?} Ending: {:?} Count:{}",
                                                (r1, c1),
                                                (r2, c2),
                                                count
                                            );
                                            *shortest = count;
                                            self.render_relative_positions(
                                                &relative_positions,
                                                (r1, c1),
                                                (r2, c2),
                                            );
                                        }
                                    } else {
                                        shortest.insert((r2, c2), count);
                                        println!(
                                            "Starting: {:?} Ending: {:?} Count:{}",
                                            (r1, c1),
                                            (r2, c2),
                                            count
                                        );
                                        self.render_relative_positions(
                                            &relative_positions,
                                            (r1, c1),
                                            (r2, c2),
                                        );
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                if c > 0 {
                    match neighbours.left {
                        Some(Elevation::Next)
                        | Some(Elevation::Previous)
                        | Some(Elevation::Low)
                        | Some(Elevation::Same) => {
                            if (relative_positions[r][c - 1] == 0
                                || count < relative_positions[r][c - 1])
                                && (r, c - 1) != (r1, c1)
                            {
                                if (r, c - 1) != (r2, c2) {
                                    pending1.insert((r, c - 1));
                                    relative_positions[r][c - 1] = count;
                                } else {
                                    relative_positions[r][c - 1] = count;
                                    if let Some(shortest) = shortest.get_mut(&(r2, c2)) {
                                        if count < *shortest {
                                            println!(
                                                "Starting: {:?} Ending: {:?} Count:{}",
                                                (r1, c1),
                                                (r2, c2),
                                                count
                                            );
                                            *shortest = count;
                                            self.render_relative_positions(
                                                &relative_positions,
                                                (r1, c1),
                                                (r2, c2),
                                            );
                                        }
                                    } else {
                                        shortest.insert((r2, c2), count);
                                        println!(
                                            "Starting: {:?} Ending: {:?} Count:{}",
                                            (r1, c1),
                                            (r2, c2),
                                            count
                                        );
                                        self.render_relative_positions(
                                            &relative_positions,
                                            (r1, c1),
                                            (r2, c2),
                                        );
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                if r < self.elevation.len() - 1 {
                    match neighbours.down {
                        Some(Elevation::Next)
                        | Some(Elevation::Previous)
                        | Some(Elevation::Low)
                        | Some(Elevation::Same) => {
                            if (relative_positions[r + 1][c] == 0
                                || count < relative_positions[r + 1][c])
                                && (r + 1, c) != (r1, c1)
                            {
                                if (r + 1, c) != (r2, c2) {
                                    pending1.insert((r + 1, c));
                                    relative_positions[r + 1][c] = count;
                                } else {
                                    relative_positions[r + 1][c] = count;
                                    if let Some(shortest) = shortest.get_mut(&(r2, c2)) {
                                        if count < *shortest {
                                            println!(
                                                "Starting: {:?} Ending: {:?} Count:{}",
                                                (r1, c1),
                                                (r2, c2),
                                                count
                                            );
                                            *shortest = count;
                                            self.render_relative_positions(
                                                &relative_positions,
                                                (r1, c1),
                                                (r2, c2),
                                            );
                                        }
                                    } else {
                                        shortest.insert((r2, c2), count);
                                        println!(
                                            "Starting: {:?} Ending: {:?} Count:{}",
                                            (r1, c1),
                                            (r2, c2),
                                            count
                                        );
                                        self.render_relative_positions(
                                            &relative_positions,
                                            (r1, c1),
                                            (r2, c2),
                                        );
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
                if c < self.elevation[0].len() - 1 {
                    match neighbours.right {
                        Some(Elevation::Next)
                        | Some(Elevation::Previous)
                        | Some(Elevation::Low)
                        | Some(Elevation::Same) => {
                            if (relative_positions[r][c + 1] == 0
                                || count < relative_positions[r][c + 1])
                                && (r, c + 1) != (r1, c1)
                            {
                                if (r, c + 1) != (r2, c2) {
                                    pending1.insert((r, c + 1));
                                    relative_positions[r][c + 1] = count;
                                } else {
                                    relative_positions[r][c + 1] = count;
                                    if let Some(shortest) = shortest.get_mut(&(r2, c2)) {
                                        if count < *shortest {
                                            println!(
                                                "Starting: {:?} Ending: {:?} Count:{}",
                                                (r1, c1),
                                                (r2, c2),
                                                count
                                            );
                                            *shortest = count;
                                            self.render_relative_positions(
                                                &relative_positions,
                                                (r1, c1),
                                                (r2, c2),
                                            );
                                        }
                                    } else {
                                        shortest.insert((r2, c2), count);
                                        println!(
                                            "Starting: {:?} Ending: {:?} Count:{}",
                                            (r1, c1),
                                            (r2, c2),
                                            count
                                        );
                                        self.render_relative_positions(
                                            &relative_positions,
                                            (r1, c1),
                                            (r2, c2),
                                        );
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }

            pending = pending1;
            count += 1;
        }
    }

    fn find_shortest_path_to_next_elevation(
        &self,
        shortest: Vec<((usize, usize), usize)>,
    ) -> usize {
        if let Some((_, count)) = shortest
            .iter()
            .find(|(location, _)| *location == self.end_location)
        {
            return *count;
        } else {
            let mut next_shortest: HashMap<(usize, usize), usize> = HashMap::new();

            for ((r, c), count) in shortest {
                // next_shortest.clear();

                let next_elevation = self.elevation[r][c];

                if let Some(locations) = self.next_pairs.get(&next_elevation) {
                    let mut locations: Vec<(usize, usize)> =
                        locations.iter().map(|(r, c)| (*r, *c)).collect();

                    locations.sort_by(|(r1, c1), (r2, c2)| {
                        compare((r, c), (*r1, *c1)).cmp(&compare((r, c), (*r2, *c2)))
                    });

                    println!(
                        "Attempting from ({},{}) for {} count: {}",
                        r,
                        c,
                        next_elevation,
                        locations.len()
                    );

                    for (r1, c1) in locations {
                        println!("Attempting ({},{}) for {}", r1, c1, next_elevation);

                        self.find_shortest_between_locations(
                            (r, c),
                            (r1, c1),
                            count,
                            &mut next_shortest,
                        );
                    }
                }
            }

            let mut sorted: Vec<((usize, usize), usize)> = next_shortest.into_iter().collect();

            sorted.sort_by(|x, y| x.1.cmp(&y.1));

            self.find_shortest_path_to_next_elevation(sorted)
        }
    }

    fn find_shortest_path(&self) -> usize {
        // let shortest: Vec<((usize, usize), usize)> = self
        //     .find_locations_at_start()
        //     .into_iter()
        //     .map(|rc| (rc, 2))
        //     .collect();

        // self.find_shortest_path_to_next_elevation(shortest)

        let mut min_shortest = self.elevation_count;

        for r in 0..self.elevation.len() {
            let start_location = (r, 0);

            let mut next_shortest: HashMap<(usize, usize), usize> = HashMap::new();
            self.find_shortest_between_locations(
                start_location,
                self.end_location,
                0,
                &mut next_shortest,
            );

            if let Some(shortest) = next_shortest.get(&self.end_location) {
                println!("{}", shortest);
                if *shortest < min_shortest {
                    min_shortest = *shortest;
                }
            }
        }

        min_shortest
    }
}

fn main() {
    let lines: Vec<&str> = "abccccccccccccccccccccccccccaaaaaaaaacccccccccccaaacccccccccccccccccccccccccaaaaaaaaccccccccaaaaaaccaaccccccccccccccccccccccccaaaaacaacaaaacccccccccccccccccccccccccccccccccccccaaaaa
    abccaaacccccccccccccccccccccaaaaaaaaacccccccccaaaaaacccccccccccccccccccccaaaaaaaaaaaccccccccaaaaaaccaaaaaacccaacaaccccccccccccaaaaaaaacaaaaaaccccccccccccccccccccccccccccccccccaaaaaa
    abccaaaaccccccccccccccccccccaaaaaaaaccccccccccaaaaaaccccaaaaaccccccccccccaaaaaaaaaaacccccccccaaaaaccaaaaaccccaaaacccccccccccccccaaaaacccaaaaaccccccccccccccccccccaaccccccccccccaaaaaa
    abccaaaacccccccaaaccccccccccaaaaaaacccccccccccaaaaaaccccaaaaaccccccccccccacaaaaaaaaaacccccccaaaaacaaaaaaacccaaaaaccccccccccccccaaaaacccaaaaaccccccccccccccccccccaaaaccccccccccccccaaa
    abccaaaccccccaaaaaacccccccccaccaaaccccccccccccaaaaacccccaaaaaaccccaaaacccccaaaaaaaaaaaccccccaaaaacaaaaaaacccaaaaaacccccccccccccaacaaaccaccaacccccccccccccccaaaccaaaaccccccccccccccaaa
    abcccccccccccaaaaaacccccccccccccaaacccccccccccaaaaacccccaaaaaaccccaaaaccccccaaaaacaaaaccccccccccccccaaaaaaccacaaaaccccaaaaacccccccaaccccccccccccccccccccccaaaackkkaccccccccccccccccaa
    abcccccccccccaaaaaacccccccccccccccaaacccccccccccccccccccaaaaaaccccaaaacccccaaaaaccccaaccccccccccccccaaccaaccccaaccccccaaaaaccccccccccccccccccccccccccccccccaakkkkkkkccccccccccccccccc
    abaccccccccccaaaaaccccccccccccccccaaaaccccccccccccccccccccaaaccccccaaccccccccaaaccccccccccccccccccccaacccccccccccccccaaaaaacccccccccccccccccccccaaacccccccccjkkkkkkkkccccccccaacccccc
    abaccccccccccaaaaacccccaacccccccccaaaaccccccccccccaacccccccccccccccccccccccccaaacccccccccccccccccccccaaccccccaccaccccaaaaaaaaaccaccccccccccccccaaaaccccccccjjkkoopkkkkaccaacaaacccccc
    abaccccccccccccccccaaaaaacccccccccaaacccccccccccccaaaaaacccccccccccccccccccccccccccccccccccccccccccccaaaaaaccaaaaccccaaaaaaaaacaaccccccccccccccaaaacccccccjjjkoooppkkkaccaaaaaaaacccc
    abcccccccccccccccccaaaaaaaaccccccccccccccccaccccccaaaaaaccccccccccccccccccccccccaaccaacccccccccccccccaaaaaacaaaaacccccaaacccaaaaacccccccccccccccaaacccccjjjjjoooppppkklccaaaaaaaacccc
    abcccccccccccccccccaaaaaaaacccccccccccccccaaacccaaaaaaaccccaacccacccccccccccccccaaaaaaccccccccaaaccaaaaaaaccaaaaaaccccccccaaaaaacccccccccccccccccccccjjjjjjjoooouuppplllccaccaaaacccc
    abccccccccccccccccccaaaaaaaccccaacccccaaacaaacccaaaaaaaccccaaacaacccccccccccccccaaaaaacccccccccaaaaaaaaaaaacaaaaaaccccccccaaaaaaaaccccccccccccccccciijjjjjjooouuuupppllllcccccccccccc
    abccccccccccccccccccaaaaaccccccaacccccaaaaaaaaaaccaaaaaaccccaaaaaccccccccccccccaaaaaaacccccccaaaaaaaaaaaaaacccaaccccccccccaacaaaaacccccccccccccccciiiijoooooouuuuuuppplllllcccccccccc
    abcccccccccccccccccaaaaaacccaacaaaaacccaaaaaaaaaccaaccaaccaaaaaacccccccccccccccaaaaaaaaccccccaaaaacccaaaaaacccaccccccccccccccaacccccccccccccccccciiiinnoooooouuxuuuupppplllllcccccccc
    abcccccccccccccccccccccaacccaaaaaaaaccccaaaaaaccccaaccccccaaaaaaaaccaaaccccccccaaaaaaaacccccccaaaaaccaacaaaaaaaacccaaccccccccacccccccccaaaccccccciiinnnnntttuuuxxuuuppppqqllllccccccc
    abccccccccccccaacccccccccccccaaaaaccccccaaaaaacccccaaaccccaaaaaaaaccaaaacccccccccaaaccccccccccaaccaccccccaaaaaacccaaaaaaccccccccccccccaaaaccccaaiiinnnntttttuuxxxxuuvpqqqqqllmmcccccc
    abccccccccccaaaaaaccccccccccccaaaaaccccaaaaaaacccccaaacccccccaacccccaaaaccccaaccccaacccccccccccccccccccccaaaaaaccccaaaaaccccccccccccccaaaaccccaaiiinnnttttxxxxxxxyuvvvvvqqqqmmmcccccc
    abccaaacccccaaaaaacccccccccccaaacaaccccaaacaaacccccaaaaaaacccaccccccaaaccccaaaacccccccccccccccccccccccccaaaaaaaacaaaaaaacccccccccaaacccaaaccccaaaiinnntttxxxxxxxxyyyyvvvvqqqmmmcccccc
    abcaaaacccccaaaaaccccccccccccaaaccaccccccccccacccaaaaaaaaaaccccccccccccccccaaaaccccccccccccccccccccccccaaaaaaaaaaaaaaaaaaccccccccaaaaaacccccaaaaaiiinnnttxxxxxxxyyyyyyvvvqqqmmmcccccc
    SbcaaaaccccccaaaaacccccaaaccccccaaacccccccccaaccaaaaaaaaaaaacccccccccccccccaaaaccccccccccccccccccccccccaaaaaaaaaaaaaaaaaacccccccaaaaaaacccccaaaaaiiinnntttxxxxEzzyyyyvvvqqqmmmdddcccc
    abccaaaccccccaaaaacccccaaaaccccaaaaaaccccccaaaaccaaaaaaacaaacccccaaccccccccccccccccccccccccccccccccccccacaaaaacccccaaacacccccccaaaaaaaccccccaaaaaahhhnnntttxxxyyyyyyvvvvqqmmmmdddcccc
    abcccccccccccccccccccccaaaaccccaaaaaaccccccaaaaccccaaaaaaaaaaaaaaaacacccccccccccccccccccccccccccccccccccccaaaacccccaaccccccccccaaaaaaaccccccccaaaahhhnnnnttxxxyyyyyvvvqqqqmmmdddccccc
    abcccccccccccccccaacaacaaacccccaaaaaaccccccaaaacccaaaaaaaaaaaaaaaaaaacccccccccccccccccaacaaccccccccccccccccaaccccccccccccccccccccaaaaaacccccccaaccchhhmmmttxwyyyyyyvvrqqqmmmddddccccc
    abcccccccccccccccaaaacccccccccccaaaaacccccccccccaaaaaaaaaaaaaaccaaaaccccaacccccaacccccaaaaaccccccccccccccccccccccccccccccccaaaaccaaaaaacccccccaaccahhhmmssswwywwwyyyvvrqmmmmdddcccccc
    abccccccccccccccaaaaacccccccccccaacaacccccccccccaaaaaacaaaaaacccaaaaaaacaaccccaaaccccccaaaaacccccccacccccccccccccccccccccccaaaaccaacccccccccccaaaaahhhmmsswwwwwwwwywwvrrnnmdddccccccc
    abccccccccccccccaaaaaaccaaccccccccccccccccccccccaaaaaaaaaaaaacccacaacaaaaaccccaaacaaacaaaaaacaaacaaacccccccacccaaccccaaccccaaaacccccccccaaaccccaaaahhhmmssswwwwswwwwwwrrnnndddccccccc
    abaaccccccccccccacaaaacaaaaaaaccccccccccaacccccccaaaaaaaacaaaccccccccaaaaaaaaaaaaaaaacaaaacccaaaaaaacccccccaacaaaaaaaaacccccaaccccccccccaaacccaaaaahhhmmsssswsssrrwwwrrrnneddaccccccc
    abaaccccccccaaccccaaccccaaaaacccccccccccaacccccccaaaacccccccacccccccaaaaaaaaaaaaaaaaacccaaccccaaaaaacccccccaaaaacaaaaaaacccccccccccccaaaaaaaaaaaaaahhhmmssssssssrrrrrrrrnneedaaaacccc
    abacccccccccaaaaccccccaaaaaaaccccccccaaaaaaaacccccccccccccccccccccccaaaaaaaacaaaaaacccaaacccccaaaaaaaacccccaaaaaacaaaaaaaccccccccccccaaaaaaaaaaaaaahhhmmmsssssllrrrrrrrnnneeeaaaacccc
    abaaacccccaaaaaaccccccaaaaaaaacccaaccaaaaaaaaccccaaaaaccccccccccccccaaaaaacccaaaaaaccccaaaccaaaaaaaaaaccccaaaaaaaaaaaaaaaccccccccccccccaaaaaccccaachhgmmmmmlllllllrrrrrnnneeeaaaacccc
    abaaacccccaaaaaccccaccaaaaaaaacaaaaaaccaaaaccccccaaaaacccccccccccccccccaaacccaaaaaaaccaaaaaaaaaaaaaaaaccccaaaaaaaaaaaaacccccccccccccccaaaaaaccccaaccgggmmmllllllllllnnnnneeeaaaaccccc
    abcccccccccaaaaacccaaacaaaacaacaaaaaacaaaaaccccccaaaaaaccccccccccccccccccccccaaacaaacccaaaaaaaacaaaccccccccccaaccaaaaaacccccccccccccccaaaaaacaacccccgggggmlllfffflllnnnnneeeaaaaccccc
    abcccccccccaaccacccaaaaaaacccccaaaaaacaacaaacccccaaaaaaccccccccccccccccccccccaccaaccaaaaaaaaacccaaaccccccccccaaccccccaacccccaaaaccccccaccaaaaaaccccccggggggggfffffflnnneeeeeacaaacccc
    abaaaccccccccccccccaaaaaacccccccaaaaacacccaaaacccaaaaaacccccccccccccccccccaaacccaaccaaaaaaaaacccaaccccccccccccccccccccccccccaaaaccccaaaccaaaaaacccccccgggggggfffffffffeeeeeaacccccccc
    abaaaaacccccccccccaaaaaaaaccccccaaaacccccccaaacccccaacccccccaaacccccccccccaaaacaaacccaaaaaaaacccccccccccccccccccccccccccccccaaaaccccaaacccaaaaaaacccccccccgccaaaafffffeeeeccccccccccc
    abaaaaaccccccaaccaaaaaaaaaacccccaaacaaaaaacaaaccccccccccccaaaaaccccccccccaaaaacaaacccccaaaaaaacccccccccccccccccccccccccccccccaacccccaaaaaaaaaaaaaccccccccccccaaaacaafffecccccccccccaa
    abaaaacccaaacaaccaaaaaaaaaacccccaaaaaaaaaaaaaaaaacccccccccaaaaaaccccccccccaaaaaaaaaaacaaacccacccaacccccccccaaaaccccaaacccccccccccaaaaaaaaaaaaaaacccccccccccccaaaccccaaccccccccccccaaa
    abaaaaacccaaaaacccccaaacaaacccccaaaaaaccaaaaaaaaacccccccccaaaaaaccaaccacccaaaaaaaaaaaaaaaccccccaaacccccccccaaaaccccaaaaccccccccccaaaaaaaaaaaaaaccccccccccccccaaaccccccccccccccccccaaa
    abaaaaacccaaaaaaacccaaaccccccccaaaaaaaaccaaaaaaaccccccccccaaaaacccaaaaaccccaaaaaaaaaaaaacccccaaaaaaaaccccccaaaaccccaaaacccccccccccaaaaaaacccaaaccccccccccccccaaacccccccccccccccaaaaaa
    abcccccccaaaaaaaaccccaacccccccaaaaaaaaaaaaaaaaacccccccccccaaaaaccaaaaaacccccaaaaaaaaaaaaaccccaaaaaaaacccccccaacccccaaacccccccccccccaaaaaaacccccccccccccccccccccccccccccccccccccaaaaaa".split('\n').collect();

    let hill = Hill::new(lines);
    let count = hill.find_shortest_path();

    println!("{:?}", count);
}
