import codyssi
import gleam/dict
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn part1_test() {
  sample |> codyssi.parse |> codyssi.part1 |> should.equal(45)
}

pub fn part2_test() {
  sample |> codyssi.parse |> codyssi.part2 |> should.equal(796)
}

pub fn part3_test() {
  sample |> codyssi.parse |> codyssi.part3 |> should.equal(827)
}

pub fn tripples_test() {
  [#(4, 8), #(5, 8), #(10, 1)]
  |> codyssi.to_tripples
  |> should.equal([#(4, 8, 5), #(5, 8, 10), #(10, 1, 4)])
}

pub fn block_length_test() {
  let len = 10

  len |> codyssi.block_length(#(1, 5)) |> should.equal(4)
  len |> codyssi.block_length(#(5, 1)) |> should.equal(4)

  len |> codyssi.block_length(#(1, 6)) |> should.equal(5)
  len |> codyssi.block_length(#(6, 1)) |> should.equal(5)

  len |> codyssi.block_length(#(1, 9)) |> should.equal(2)
  len |> codyssi.block_length(#(9, 1)) |> should.equal(2)

  len |> codyssi.block_length(#(1, 10)) |> should.equal(1)
  len |> codyssi.block_length(#(10, 1)) |> should.equal(1)

  len |> codyssi.block_length(#(4, 8)) |> should.equal(3)
  len |> codyssi.block_length(#(8, 4)) |> should.equal(3)
}

pub fn parse_test() {
  "159\n527\n\n4-8\n5-8\n10-1\n\n10\n"
  |> codyssi.parse
  |> should.equal(#(
    dict.from_list([#(1, 159), #(2, 527)]),
    [#(4, 8), #(5, 8), #(10, 1)],
    10,
  ))
}

const sample = "159
527
827
596
296
413
45
796
853
778

4-8
5-8
10-1
6-5
2-1
6-5
8-7
3-6
7-8
2-10
6-4
8-10
1-9
3-6
7-10

10
"
