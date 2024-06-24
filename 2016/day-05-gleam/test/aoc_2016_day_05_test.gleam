import aoc_2016_day_05
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn hello_world_test() {
  // To keep this unit test fast, we're starting with an id not far away from
  // the value for the first digit, and also stop after the first four digits.
  let start_id = 3_000_000
  let required_length = 4
  aoc_2016_day_05.crack("abc", start_id, "", required_length)
  |> should.equal("18F4")
}
