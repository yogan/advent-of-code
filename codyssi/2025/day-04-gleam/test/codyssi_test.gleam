import codyssi
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = "NNBUSSSSSDSSZZZZMMMMMMMM
PWAAASYBRRREEEEEEE
FBBOFFFKDDDDDDDDD
VJAANCPKKLZSSSSSSSSS
NNNNNNBBVVVVVVVVV"

pub fn part1_test() {
  sample
  |> codyssi.parse
  |> codyssi.part1
  |> should.equal(1247)
}
