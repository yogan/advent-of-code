import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub type Move {
  Right(dist: Int)
  Left(dist: Int)
}

pub fn part1(moves: List(Move)) -> Int {
  moves
  |> list.map_fold(from: 50, with: fn(dial, move) { #(turn(dial, move), dial) })
  |> fn(pair) { [pair.0, ..pair.1] }
  |> list.count(fn(d) { d == 0 })
}

pub fn part2(moves: List(Move)) -> Int {
  moves
  |> list.map_fold(from: #(50, 0), with: fn(dial, move) {
    #(turn2(dial.0, move), dial)
  })
  |> fn(pair) { [pair.0, ..pair.1] }
  |> list.fold(0, fn(acc, d) { acc + d.1 })
}

fn turn(dial: Int, move: Move) -> Int {
  case move {
    Right(d) -> dial + d
    Left(d) -> dial - d
  }
  |> mod(100)
}

fn turn2(dial: Int, move: Move) -> #(Int, Int) {
  let dist = mod(move.dist, 100)
  let extra = case move {
    Right(_) if dial + dist >= 100 -> 1
    Left(_) if dial != 0 && dial - dist <= 0 -> 1
    Left(_) if move.dist < 0 && dist != 0 -> -1
    _ -> 0
  }
  #(turn(dial, move), move.dist / 100 + extra)
}

fn mod(a: Int, b: Int) -> Int {
  case a % b {
    r if r != 0 && r < 0 != b < 0 -> r + b
    r -> r
  }
}

fn parse(content: String) -> List(Move) {
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
          do(part2)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
