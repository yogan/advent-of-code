import codyssi
import gleam/string
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = "tv8cmj0i2951190z5w44fe205k542l5818ds05ib425h9lj260ud38-l6a06
a586m0eeuqqvt5-k-8434hb27ytha3i75-lw23-0cj856l7zn8234a05eron
"

pub fn part1_test() {
  sample |> codyssi.parse |> codyssi.part1 |> should.equal(52)
}

pub fn part2_test() {
  sample |> codyssi.parse |> codyssi.part2 |> should.equal(18)
}

pub fn part3_test() {
  sample |> codyssi.parse |> codyssi.part3 |> should.equal(26)
}

pub fn reduce_test() {
  test_string(codyssi.reduce, "321ab", "32b")
  test_string(codyssi.reduce, "32b", "3")
  test_string(codyssi.reduce, "z-4", "z")
}

pub fn max_reduce_test() {
  let reduce = codyssi.max_reduce(codyssi.reduce, _)
  test_string(reduce, "baa3", "ba")
  test_string(reduce, "321ab", "3")
  test_string(reduce, "a7b", "b")
  test_string(reduce, "z-4", "z")
}

pub fn reduce2_test() {
  test_string(codyssi.reduce2, "321ab", "32b")
  test_string(codyssi.reduce2, "32b", "3")
  test_string(codyssi.reduce2, "a7b", "b")
  test_string(codyssi.reduce2, "z-4", "z-4")
  test_string(codyssi.reduce2, "a3-b6", "-")
}

fn test_string(f, in, expected) {
  in |> string.to_graphemes |> f |> should.equal(string.to_graphemes(expected))
}
