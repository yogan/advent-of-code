import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/regex
import gleam/string
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let lines = parse(content)
          let do = fn(f) { lines |> f |> int.to_string |> io.println }
          do(part1)
          do(part2)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(lines) {
  lines
  |> list.map(fn(line) { line |> list.filter(is_alphabetical) |> list.length })
  |> sum
}

pub fn part2(lines) {
  lines |> list.map(max_reduce) |> list.map(list.length) |> sum
}

fn is_alphabetical(c) {
  let assert Ok(re) = regex.from_string("[a-zA-Z]")
  regex.check(with: re, content: c)
}

fn is_numerical(c) {
  let assert Ok(re) = regex.from_string("[0-9]")
  regex.check(with: re, content: c)
}

pub fn max_reduce(line) {
  let reduced = reduce(line)
  case reduced == line {
    True -> reduced
    False -> max_reduce(reduced)
  }
}

pub fn reduce(line) {
  case line {
    [a, b, ..rest] -> {
      case is_numerical(a), is_numerical(b) {
        True, False -> reduce(rest)
        False, True -> reduce(rest)
        _, _ -> [a, ..reduce([b, ..rest])]
      }
    }
    _ -> line
  }
}

fn sum(lst) {
  lst |> list.fold(0, int.add)
}

pub fn parse(input) {
  input |> string.trim() |> string.split("\n") |> list.map(string.to_graphemes)
}
