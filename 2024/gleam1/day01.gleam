import gleam/file
import gleam/io
import gleam/result

pub fn main() {
  let res = file.read_to_bitstring("../data/08.txt")
  let contents = result.unwrap(res)
  io.debug(contents)
}
