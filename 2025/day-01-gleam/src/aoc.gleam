import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub type Move {
  Right(Int)
  Left(Int)
}

pub fn part1(moves: List(Move)) -> Int {
  moves
  |> list.map_fold(from: 50, with: fn(dial, move) { #(turn(dial, move), dial) })
  |> fn(pair) { pair.1 |> list.count(fn(d) { d == 0 }) }
}

fn turn(dial, move) {
  case move {
    Right(d) -> dial + d
    Left(d) -> dial - d
  }
  |> mod(100)
}

fn mod(a: Int, b: Int) {
  case a % b {
    r if r != 0 && r < 0 != b < 0 -> r + b
    r -> r
  }
}

fn parse(content) {
  content
  |> string.trim
  |> string.split("\n")
  |> list.map(parse_line)
}

pub fn parse_line(line: String) -> Move {
  case line {
    "L" <> dist -> dist |> int.parse |> result.unwrap(0) |> Left
    "R" <> dist -> dist |> int.parse |> result.unwrap(0) |> Right
    _ -> panic
  }
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let input = parse(content)
          let do = fn(f) { input |> f |> int.to_string |> io.println }
          do(part1)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
