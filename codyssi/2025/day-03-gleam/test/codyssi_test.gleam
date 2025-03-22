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

pub fn part3_test() {
  sample
  |> codyssi.parse
  |> codyssi.part3
  |> should.equal(9)
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

pub fn overlap_test() {
  codyssi.overlap([8, 10, 7, 10]) |> should.equal([8, 10])
}

pub fn combine_test() {
  codyssi.combine([5, 6, 7, 8]) |> should.equal([[5, 6], [7, 8]])
  codyssi.combine([3, 4, 7, 8]) |> should.equal([[3, 4], [7, 8]])
  codyssi.combine([6, 8, 8, 9]) |> should.equal([[6, 9]])
  codyssi.combine([7, 9, 6, 8]) |> should.equal([[6, 9]])
  codyssi.combine([2, 9, 6, 8]) |> should.equal([[2, 9]])
  codyssi.combine([6, 8, 2, 9]) |> should.equal([[2, 9]])

  codyssi.combine([8, 9, 9, 10]) |> should.equal([[8, 10]])
  codyssi.combine([7, 8, 8, 10]) |> should.equal([[7, 10]])
  codyssi.combine([9, 10, 5, 10]) |> should.equal([[5, 10]])
  codyssi.combine([3, 10, 9, 10]) |> should.equal([[3, 10]])
  codyssi.combine([4, 8, 7, 9]) |> should.equal([[4, 9]])
  codyssi.combine([9, 10, 2, 7]) |> should.equal([[9, 10], [2, 7]])

  codyssi.combine([1, 2, 4, 7]) |> should.equal([[1, 2], [4, 7]])
}

pub fn combine_pairs_test() {
  codyssi.combine_pairs([[8, 9, 9, 10], [7, 8, 8, 10]])
  |> should.equal([[7, 10]])

  codyssi.combine_pairs([[7, 8, 8, 10], [9, 10, 5, 10]])
  |> should.equal([[5, 10]])

  codyssi.combine_pairs([[9, 10, 5, 10], [3, 10, 9, 10]])
  |> should.equal([[3, 10]])

  codyssi.combine_pairs([[3, 10, 9, 10], [4, 8, 7, 9]])
  |> should.equal([[3, 10]])

  codyssi.combine_pairs([[4, 8, 7, 9], [9, 10, 2, 7]])
  |> should.equal([[4, 10], [2, 9]])

  codyssi.combine_pairs([[9, 10, 2, 7], [4, 8, 7, 9]])
  |> should.equal([[4, 10], [2, 9]])

  codyssi.combine_pairs([[9, 10, 2, 7], [1, 2, 4, 7]])
  // combines [[9, 10], [2, 7]] with [[1, 2], [4, 7]]
  |> should.equal([[9, 10], [1, 2], [9, 10], [4, 7], [1, 7], [2, 7]])
}

pub fn merge_ranges_test() {
  [[2, 3], [2, 4], [5, 10], [2, 6]]
  |> codyssi.merge_ranges
  |> should.equal([[2, 10]])

  [[2, 3], [5, 10]] |> codyssi.merge_ranges |> should.equal([[2, 3], [5, 10]])
  [[2, 9], [4, 10]] |> codyssi.merge_ranges |> should.equal([[2, 10]])
  [[4, 10], [2, 9]] |> codyssi.merge_ranges |> should.equal([[2, 10]])

  [[9, 10], [1, 2], [9, 10], [4, 7], [1, 7], [2, 7]]
  |> codyssi.merge_ranges
  |> should.equal([[1, 7], [9, 10]])

  [[9, 10], [1, 2], [10, 10], [12, 12], [4, 7], [1, 7], [2, 7]]
  |> codyssi.merge_ranges
  |> should.equal([[1, 7], [9, 10], [12, 12]])
}
