import argv
import gleam/bool
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
          do(part3)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(lines) {
  map_and_count(lines, list.filter(_, is_letter))
}

pub fn part2(lines) {
  reduce_and_count(lines, part2_cond)
}

pub fn part3(lines) {
  reduce_and_count(lines, part3_cond)
}

fn reduce_and_count(lines, cond) {
  map_and_count(lines, max_reduce(reduce(cond, _), _))
}

fn map_and_count(lines, mapper) {
  lines |> list.map(mapper) |> list.map(list.length) |> sum
}

fn matches(line, re_str) {
  let assert Ok(re) = regex.from_string(re_str)
  regex.check(with: re, content: line)
}

pub fn max_reduce(reduce_fn, line) {
  let reduced = reduce_fn(line)
  case reduced == line {
    True -> reduced
    False -> max_reduce(reduce_fn, reduced)
  }
}

pub fn reduce(cond, line) {
  case line {
    [a, b, ..rest] -> {
      case cond(a, b) {
        True -> reduce(cond, rest)
        False -> [a, ..reduce(cond, [b, ..rest])]
      }
    }
    _ -> line
  }
}

pub fn part2_cond(a, b) {
  bool.exclusive_or(is_digit(a), is_digit(b))
}

pub fn part3_cond(a, b) {
  { is_digit(a) && is_letter(b) } || { is_letter(a) && is_digit(b) }
}

fn is_letter(c) {
  c |> matches("[a-zA-Z]")
}

fn is_digit(c) {
  c |> matches("[0-9]")
}

fn sum(lst) {
  lst |> list.fold(0, int.add)
}

pub fn parse(input) {
  input |> string.trim() |> string.split("\n") |> list.map(string.to_graphemes)
}
