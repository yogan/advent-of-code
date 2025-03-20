import codyssi
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = "8
1
5
5
7
6
5
4
3
1
-++-++-++"

pub fn parse_test() {
  sample
  |> codyssi.parse
  |> should.equal(
    #([8, 1, 5, 5, 7, 6, 5, 4, 3, 1], [
      "-", "+", "+", "-", "+", "+", "-", "+", "+",
    ]),
  )
}

const start = 8

const offsets = [1, 5, 5, 7, 6, 5, 4, 3, 1]

const corrections = ["-", "+", "+", "-", "+", "+", "-", "+", "+"]

pub fn part1_test() {
  codyssi.part1(start, offsets, corrections) |> should.equal(21)
}

pub fn part2_test() {
  codyssi.part2(start, offsets, corrections) |> should.equal(23)
}

pub fn part3_test() {
  codyssi.part3(start, offsets, corrections) |> should.equal(189)
}
