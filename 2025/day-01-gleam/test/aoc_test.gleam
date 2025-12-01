import aoc.{Left, Right, parse_line, part1, part2}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = [
  Left(68),
  Left(30),
  Right(48),
  Left(5),
  Right(60),
  Left(55),
  Left(1),
  Left(99),
  Right(14),
  Left(82),
]

pub fn part1_test() {
  sample |> part1 |> should.equal(3)
}

pub fn part2_test() {
  sample |> part2 |> should.equal(6)
}

pub fn parse_line_test() {
  "L1" |> parse_line |> should.equal(Left(1))
  "R9876" |> parse_line |> should.equal(Right(9876))
}
