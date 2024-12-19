import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub type Equation {
  Eq(target: Int, values: List(Int))
}

fn run(equations: List(Equation)) {
  [["+", "*"], ["+", "*", "|"]]
  |> list.map(fn(ops) { calibrate(ops, equations) })
  |> list.map(int.to_string)
  |> string.join("\n")
  |> io.println
}

pub fn calibrate(ops: List(String), equations: List(Equation)) -> Int {
  equations
  |> list.map(fn(eq) {
    case possible(ops, eq) {
      True -> eq.target
      False -> 0
    }
  })
  |> list.fold(0, int.add)
}

pub fn possible(ops: List(String), equation: Equation) -> Bool {
  ops
  |> cartesian_product(list.length(equation.values) - 1)
  |> try_combinations(equation)
}

fn try_combinations(
  combinations: List(List(String)),
  equation: Equation,
) -> Bool {
  let Eq(target, values) = equation

  case combinations {
    [] -> False
    [ops, ..rest] -> {
      case eval(values, ops) == target {
        True -> True
        False -> try_combinations(rest, equation)
      }
    }
  }
}

pub fn eval(values: List(Int), ops: List(String)) -> Int {
  let assert [first, ..rest] = values

  rest
  |> list.zip(ops)
  |> list.fold(first, fn(acc, val_op) {
    let #(value, op) = val_op
    case op {
      "+" -> int.add(acc, value)
      "*" -> int.multiply(acc, value)
      "|" -> {
        let assert Ok(n) =
          [acc, value]
          |> list.map(int.to_string)
          |> string.concat
          |> int.parse
        n
      }
      _ -> -1
    }
  })
}

pub fn cartesian_product(lst: List(String), n: Int) -> List(List(String)) {
  case n {
    0 -> [[]]
    _ ->
      lst
      |> list.flat_map(fn(x) {
        cartesian_product(lst, n - 1)
        |> list.map(fn(xs) { [x, ..xs] })
      })
  }
}

pub fn parse_line(line: String) -> Equation {
  let assert Ok(#(l, r)) = string.split_once(line, ": ")
  let assert Ok(target) = int.parse(l)
  let assert Ok(values) =
    string.split(r, " ")
    |> list.map(int.parse)
    |> result.all
  Eq(target, values)
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) ->
          content
          |> string.trim
          |> string.split("\n")
          |> list.map(parse_line)
          |> run
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
