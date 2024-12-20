import gleeunit
import gleeunit/should

import day08

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn part1_test() {
  let input =
    "............
  ........0...
  .....0......
  .......0....
  ....0.......
  ......A.....
  ............
  ............
  ........A...
  .........A..
  ............
  ............"
  let map = day08.new_map(input)

  day08.part1(map) |> should.equal(34)
}
