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

pub fn tripples_test() {
  [#(4, 8), #(5, 8), #(10, 1)]
  |> codyssi.to_tripples
  |> should.equal([#(4, 8, 5), #(5, 8, 10), #(10, 1, 4)])
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
