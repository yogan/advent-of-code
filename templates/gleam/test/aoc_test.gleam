import aoc
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn part1_test() {
  [#(1, 2, 3), #(1, 1, 1)]
  |> aoc.part1
  |> should.equal(6 + 1)
}

pub fn parse_line_test() {
  "2x3x4"
  |> aoc.parse_line
  |> should.equal(Ok(#(2, 3, 4)))

  "2x69x420"
  |> aoc.parse_line
  |> should.equal(Ok(#(2, 69, 420)))

  "garbage"
  |> aoc.parse_line
  |> should.equal(Error("invalid line \"garbage\""))
}
