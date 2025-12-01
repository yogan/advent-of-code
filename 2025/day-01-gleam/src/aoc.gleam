import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

const start = #(50, 0)

pub fn part1(moves: List(Int)) -> Int {
  moves |> open_door(turn_count_zeros)
}

pub fn part2(moves: List(Int)) -> Int {
  moves |> open_door(turn_count_over_zeros)
}

fn open_door(
  moves: List(Int),
  count_fn: fn(#(Int, Int), Int) -> #(Int, Int),
) -> Int {
  moves |> list.fold(from: start, with: count_fn) |> second
}

fn turn_count_zeros(pair: #(Int, Int), move: Int) -> #(Int, Int) {
  let #(dial, zeros) = pair
  case turn(dial, move) {
    0 -> #(0, zeros + 1)
    d -> #(d, zeros)
  }
}

fn turn_count_over_zeros(pair: #(Int, Int), move: Int) -> #(Int, Int) {
  let #(dial, zeros) = pair
  #(turn(dial, move), zeros + count_over_zeros(dial, move))
}

fn count_over_zeros(dial: Int, move: Int) -> Int {
  case move < 0 {
    True -> {
      let #(div, mod) = divmod(move, -100)
      div + bool_to_int(dial != 0 && dial + mod <= 0)
    }
    False -> {
      let #(div, mod) = divmod(move, 100)
      div + bool_to_int(dial + mod >= 100)
    }
  }
}

fn turn(dial: Int, move: Int) -> Int {
  dial + move |> mod(100)
}

fn mod(a: Int, b: Int) -> Int {
  divmod(a, b) |> second
}

fn divmod(a: Int, b: Int) -> #(Int, Int) {
  let q = a / b
  let r = a % b
  case r != 0 && { r < 0 } != { b < 0 } {
    True -> #(q - 1, r + b)
    False -> #(q, r)
  }
}

fn second(pair: #(Int, Int)) -> Int {
  pair.1
}

fn bool_to_int(bool: Bool) -> Int {
  case bool {
    True -> 1
    False -> 0
  }
}

fn parse(content: String) -> List(Int) {
  content
  |> string.trim
  |> string.split("\n")
  |> list.map(parse_line)
}

pub fn parse_line(line: String) -> Int {
  case line {
    "L" <> dist -> Ok("-" <> dist)
    "R" <> dist -> Ok(dist)
    _ -> Error(Nil)
  }
  |> result.try(int.parse)
  |> result.unwrap(0)
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let input = parse(content)
          let do = fn(f) { input |> f |> int.to_string |> io.println }
          do(part1)
          do(part2)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
