import aoc
import gleam/list
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = [
  aoc.Data(value: 123, for: "x"),
  aoc.Data(value: 456, for: "y"),
  aoc.Gate(in: aoc.And(aoc.Wire("x"), aoc.Wire("y")), out: "d"),
  aoc.Gate(in: aoc.Or(aoc.Wire("x"), aoc.Wire("y")), out: "e"),
  aoc.Gate(in: aoc.LShift(aoc.Wire("x"), 2), out: "f"),
  aoc.Gate(in: aoc.RShift(aoc.Wire("y"), 2), out: "g"),
  aoc.Gate(in: aoc.Not("x"), out: "h"),
  aoc.Gate(in: aoc.Not("y"), out: "i"),
]

pub fn emulate_works_for_wires() {
  [
    aoc.Data(value: 123, for: "a"),
    aoc.Connection(from: "a", to: "b"),
    aoc.Connection(from: "c", to: "d"),
  ]
  |> aoc.emulate("d")
  |> should.equal(Ok(123))
}

pub fn emulate_works_for_sample_test() {
  ["d", "e", "f", "g", "h", "i", "x", "y", "nonexistent"]
  |> list.map(fn(w) { aoc.emulate(sample, w) })
  |> should.equal([
    Ok(72),
    Ok(507),
    Ok(492),
    Ok(114),
    Ok(65_412),
    Ok(65_079),
    Ok(123),
    Ok(456),
    Error("wire nonexistent not found"),
  ])
}

pub fn parse_input_works_for_sample_test() {
  "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"
  |> aoc.parse_input
  |> should.equal(Ok(sample))
}

pub fn parse_input_supports_connections_test() {
  "x -> y
q -> r
abc -> cde"
  |> aoc.parse_input
  |> should.equal(
    Ok([
      aoc.Connection(from: "x", to: "y"),
      aoc.Connection(from: "q", to: "r"),
      aoc.Connection(from: "abc", to: "cde"),
    ]),
  )
}

pub fn parse_input_supports_immidiate_values_in_gates_test() {
  "1 AND r -> s
2 OR 3 -> t
4 LSHIFT 5 -> u"
  |> aoc.parse_input
  |> should.equal(
    Ok([
      aoc.Gate(in: aoc.And(aoc.Value(1), aoc.Wire("r")), out: "s"),
      aoc.Gate(in: aoc.Or(aoc.Value(2), aoc.Value(3)), out: "t"),
      aoc.Gate(in: aoc.LShift(aoc.Value(4), 5), out: "u"),
    ]),
  )
}

pub fn parse_input_detects_invalid_lines_test() {
  "123 -> x
x AND y -> d
lOlGaRbAgE
x LSHIFT 2 -> f"
  |> aoc.parse_input
  |> should.equal(Error("invalid line \"lOlGaRbAgE\""))
}
