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

pub fn part1_test() {
  sample |> codyssi.part1 |> should.equal(21)
}
