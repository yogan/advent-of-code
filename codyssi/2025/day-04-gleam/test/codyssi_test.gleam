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

pub fn part2_test() {
  sample
  |> codyssi.parse
  |> codyssi.part2
  |> should.equal(219)
}

pub fn lossy_compress_test() {
  "ABCDEFGHIJ"
  |> codyssi.lossy_compress
  |> should.equal("A8J")

  "OONNHHHHHANNNHHHHHHHH"
  |> codyssi.lossy_compress
  |> should.equal("OO17HH")

  "BDGGGSCLUUVLCBBBQNUUUFFFFFXXXXXXXXX"
  |> codyssi.lossy_compress
  |> should.equal("BDG29XXX")
}
