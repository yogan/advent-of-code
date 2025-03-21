import codyssi
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = "Function A: ADD 495
Function B: MULTIPLY 55
Function C: RAISE TO THE POWER OF 3

5219
8933
3271
7128
9596
9407
7005
1607
4084
4525
5496"

pub fn parse_test() {
  sample
  |> codyssi.parse
  |> should.equal(
    #(495, 55, 3, [
      5219, 8933, 3271, 7128, 9596, 9407, 7005, 1607, 4084, 4525, 5496,
    ]),
  )
}

const qualities = [
  5219, 8933, 3271, 7128, 9596, 9407, 7005, 1607, 4084, 4525, 5496,
]

pub fn median_test() {
  qualities |> codyssi.median |> should.equal(5496)
}

pub fn part1_test() {
  qualities |> codyssi.part1(495, 55, 3) |> should.equal(9_130_674_516_975)
}

pub fn part2_test() {
  qualities |> codyssi.part2(495, 55, 3) |> should.equal(1_000_986_169_836_015)
}

pub fn part3_test() {
  qualities |> codyssi.part3(495, 55, 3) |> should.equal(5496)
}
