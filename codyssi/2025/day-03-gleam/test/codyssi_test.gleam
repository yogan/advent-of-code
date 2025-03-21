import codyssi
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = "8-9 9-10
7-8 8-10
9-10 5-10
3-10 9-10
4-8 7-9
9-10 2-7"

pub fn parse_test() {
  sample
  |> codyssi.parse
  |> should.equal([
    8, 9, 9, 10, 7, 8, 8, 10, 9, 10, 5, 10, 3, 10, 9, 10, 4, 8, 7, 9, 9, 10, 2,
    7,
  ])
}

pub fn part1_test() {
  sample
  |> codyssi.parse
  |> codyssi.part1
  |> should.equal(43)
}

pub fn part2_test() {
  sample
  |> codyssi.parse
  |> codyssi.part2
  |> should.equal(35)
}

pub fn pile_size_test() {
  [6, 8, 8, 10]
  |> codyssi.pile_size
  |> should.equal(5)

  [5, 6, 7, 8]
  |> codyssi.pile_size
  |> should.equal(4)

  [3, 4, 7, 8]
  |> codyssi.pile_size
  |> should.equal(4)
}
