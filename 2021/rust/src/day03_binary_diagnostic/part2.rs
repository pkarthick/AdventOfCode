use core::*;

struct PartTwo {}

impl PartSpec for PartTwo {
    fn get_day(&self) -> i32 {
        3
    }

    fn get_part_kind(&self) -> PartKind {
        PartKind::Two
    }
}

fn partition_by_bit_position(
    v: Vec<String>,
    pos: usize,
    retainer: fn(usize, usize) -> bool,
) -> usize {
    let (v1, v2): (Vec<String>, Vec<String>) = v
        .into_iter()
        .partition(|bits| bits.chars().nth(pos).unwrap() == '1');

    let v = if retainer(v1.len(), v2.len()) {v1} else {v2};
    
    if v.len() == 1 {
        let number_of_bits = v[0].len();
        v[0].chars()
            .enumerate()
            .filter(|(_, c)| *c == '1')
            .map(|(i, _)| 1 << number_of_bits - i - 1)
            .sum()
    } else {
        partition_by_bit_position(v, pos + 1, retainer)
    }
}

impl TestPart for PartTwo {
    fn process_input(&self, input: String) -> String {
        let lines: Vec<String> = input.lines().map(|x| x.to_string()).collect::<Vec<_>>();
        let (v1, v2): (Vec<String>, Vec<String>) = lines
            .into_iter()
            .partition(|bits| bits.chars().nth(0).unwrap() == '1');
        let (o2, co2): (Vec<String>, Vec<String>) = if v1.len() >= v2.len() {
            (v1, v2)
        } else {
            (v2, v1)
        };
        let oxygen_generator_rating = partition_by_bit_position(o2, 1, |x, y | x >= y);
        let c02_scrubber_rating = partition_by_bit_position(co2, 1, |x, y | x < y);
        (oxygen_generator_rating * c02_scrubber_rating).to_string()
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
