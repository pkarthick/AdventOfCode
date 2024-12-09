import day08.{new_map, part1, part2}
import simplifile

pub fn main() {
  let assert Ok(input) = simplifile.read("../data/08.txt")
  let map = new_map(input)

  part1(map)
  part2(map)
}
