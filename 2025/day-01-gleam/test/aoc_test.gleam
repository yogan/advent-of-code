import aoc.{parse_line, part1, part2}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = [-68, -30, 48, -5, 60, -55, -1, -99, 14, -82]

pub fn part1_test() {
  sample |> part1 |> should.equal(3)
}

pub fn part2_test() {
  sample |> part2 |> should.equal(6)
}

pub fn parse_line_test() {
  "L1" |> parse_line |> should.equal(-1)
  "R9876" |> parse_line |> should.equal(9876)
}
