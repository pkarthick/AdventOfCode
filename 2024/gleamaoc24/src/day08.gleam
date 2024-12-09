import gleam/dict
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/set
import gleam/string

pub type Location =
  #(Int, Int)

pub type Antenna {
  Antenna(name: String, locations: List(Location))
}

pub type Map {
  Map(antennas: dict.Dict(String, Antenna), height: Int, width: Int)
}

fn get_antenna_locations(map: Map) -> set.Set(Location) {
  map.antennas
  |> dict.values
  |> list.flat_map(fn(antenna) { antenna.locations })
  |> set.from_list
}

fn get_antinode_locations(
  antenna: Antenna,
  antinode_count: Int,
  rowcount: Int,
  colcount: Int,
) -> set.Set(Location) {
  antenna.locations
  |> list.combinations(2)
  |> list.flat_map(fn(xs) {
    let assert [#(r1, c1), #(r2, c2)] = xs
    let rd = r1 - r2
    let cd = c1 - c2
    //change rowcount to 1 to get the result of part1
    list.range(1, antinode_count)
    |> list.flat_map(fn(times) {
      [#(r1 + rd * times, c1 + cd * times), #(r2 - rd * times, c2 - cd * times)]
    })
    |> list.filter(fn(rc) {
      rc.0 >= 0 && rc.0 < rowcount && rc.1 >= 0 && rc.1 < colcount
    })
  })
  |> set.from_list
}

pub fn new_map(input: String) -> Map {
  let lines = string.split(input, "\n")

  let map_height = list.length(lines)
  let map_width = list.first(lines) |> result.unwrap("") |> string.length

  let antennas =
    lines
    |> list.index_map(fn(line, r) {
      string.to_graphemes(line)
      |> list.index_map(fn(ch, c) { #(ch, #(r, c)) })
      |> list.filter(fn(tuple) { tuple.0 != "." })
    })
    |> list.flatten
    |> list.group(pair.first)
    |> dict.map_values(fn(a, vs) { Antenna(a, vs |> list.map(pair.second)) })

  Map(antennas, map_height, map_width)
}

pub fn part1(map: Map) {
  map.antennas
  |> dict.values
  |> list.fold(set.new(), fn(s, antenna) {
    get_antinode_locations(antenna, 1, map.height, map.width)
    |> set.union(s)
  })
  |> set.size
  |> io.debug
}

pub fn part2(map: Map) {
  map.antennas
  |> dict.values
  |> list.fold(get_antenna_locations(map), fn(s, antenna) {
    get_antinode_locations(antenna, map.height, map.height, map.width)
    |> set.union(s)
  })
  |> set.size
  |> io.debug
}
