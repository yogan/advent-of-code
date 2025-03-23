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

pub fn part3_test() {
  sample
  |> codyssi.parse
  |> codyssi.part3
  |> should.equal(539)
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

pub fn rle_compress_test() {
  "OONNHHHHHANNNHHHHHHHH"
  |> codyssi.rle_compress
  |> should.equal("2O2N5H1A3N8H")

  "BDGGGSCLUUVLCBBBQNUUUFFFFFXXXXXXXXX"
  |> codyssi.rle_compress
  |> should.equal("1B1D3G1S1C1L2U1V1L1C3B1Q1N3U5F9X")
}
