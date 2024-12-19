import aoc
import gleeunit
import gleeunit/should
import gleam/set

pub fn main() {
  gleeunit.main()
}

pub fn parse_line_test() {
  "190: 10 19"
  |> aoc.parse_line
  |> should.equal(aoc.Eq(190, [10, 19]))

  "21037: 9 7 18 13"
  |> aoc.parse_line
  |> should.equal(aoc.Eq(21_037, [9, 7, 18, 13]))
}

pub fn cartesian_product_test() {
  ["+", "*"]
  |> aoc.cartesian_product(1)
  |> set.from_list
  |> should.equal(set.from_list([["+"], ["*"]]))

  ["+", "*"]
  |> aoc.cartesian_product(2)
  |> set.from_list
  |> should.equal(
    set.from_list([["+", "+"], ["+", "*"], ["*", "+"], ["*", "*"]]),
  )

  ["+", "*"]
  |> aoc.cartesian_product(3)
  |> set.from_list
  |> should.equal(
    set.from_list([
      ["+", "+", "+"],
      ["+", "+", "*"],
      ["+", "*", "+"],
      ["+", "*", "*"],
      ["*", "+", "+"],
      ["*", "+", "*"],
      ["*", "*", "+"],
      ["*", "*", "*"],
    ]),
  )

  ["+", "*", "|"]
  |> aoc.cartesian_product(1)
  |> set.from_list
  |> should.equal(set.from_list([["+"], ["*"], ["|"]]))

  ["+", "*", "|"]
  |> aoc.cartesian_product(2)
  |> set.from_list
  |> should.equal(
    set.from_list([
      ["+", "+"],
      ["+", "*"],
      ["+", "|"],
      ["*", "+"],
      ["*", "*"],
      ["*", "|"],
      ["|", "+"],
      ["|", "*"],
      ["|", "|"],
    ]),
  )
}

pub fn possible_part1_test() {
  let ops = ["+", "*"]

  ops
  |> aoc.possible(aoc.Eq(190, [10, 19]))
  |> should.be_true

  ops
  |> aoc.possible(aoc.Eq(3267, [81, 40, 27]))
  |> should.be_true

  ops
  |> aoc.possible(aoc.Eq(83, [17, 5]))
  |> should.be_false

  ops
  |> aoc.possible(aoc.Eq(156, [15, 6]))
  |> should.be_false

  ops
  |> aoc.possible(aoc.Eq(7290, [6, 8, 6, 15]))
  |> should.be_false

  ops
  |> aoc.possible(aoc.Eq(161_011, [16, 10, 13]))
  |> should.be_false

  ops
  |> aoc.possible(aoc.Eq(192, [17, 8, 14]))
  |> should.be_false

  ops
  |> aoc.possible(aoc.Eq(21_037, [9, 7, 18, 13]))
  |> should.be_false

  ops
  |> aoc.possible(aoc.Eq(292, [11, 6, 16, 20]))
  |> should.be_true
}

pub fn possible_part2_test() {
  let extended_ops = ["+", "*", "|"]

  extended_ops
  |> aoc.possible(aoc.Eq(190, [10, 19]))
  |> should.be_true

  extended_ops
  |> aoc.possible(aoc.Eq(3267, [81, 40, 27]))
  |> should.be_true

  extended_ops
  |> aoc.possible(aoc.Eq(83, [17, 5]))
  |> should.be_false

  extended_ops
  |> aoc.possible(aoc.Eq(156, [15, 6]))
  |> should.be_true

  extended_ops
  |> aoc.possible(aoc.Eq(7290, [6, 8, 6, 15]))
  |> should.be_true

  extended_ops
  |> aoc.possible(aoc.Eq(161_011, [16, 10, 13]))
  |> should.be_false

  extended_ops
  |> aoc.possible(aoc.Eq(192, [17, 8, 14]))
  |> should.be_true

  extended_ops
  |> aoc.possible(aoc.Eq(21_037, [9, 7, 18, 13]))
  |> should.be_false

  extended_ops
  |> aoc.possible(aoc.Eq(292, [11, 6, 16, 20]))
  |> should.be_true
}

pub fn eval_test() {
  aoc.eval([1, 2, 3], ["+", "*"])
  |> should.equal({ 1 + 2 } * 3)

  aoc.eval([1, 2, 3, 4, 5], ["*", "*", "*", "+"])
  |> should.equal(1 * 2 * 3 * 4 + 5)

  aoc.eval([1, 2], ["|"])
  |> should.equal(12)
}
