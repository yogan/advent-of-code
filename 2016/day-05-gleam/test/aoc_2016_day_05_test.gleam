import aoc_2016_day_05
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// To keep this unit test fast, we're starting with an id not far away from
// the value for the first digit, and also stop after four digits are found.
const start_id = 3_200_000
const required_length = 4

pub fn crack1_test() {
  aoc_2016_day_05.crack1("abc", start_id, "", required_length)
  |> should.equal("18F4")
}

pub fn crack2_test() {
  aoc_2016_day_05.crack2("abc", start_id, "________", required_length)
  |> should.equal("_5_CE__3")
}
