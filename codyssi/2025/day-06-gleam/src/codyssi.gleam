import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/string
import simplifile

type Char {
  Corrupted(String)
  Uncorrupted(Int)
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let chars = parse(content)
          let do = fn(f) { chars |> f |> int.to_string |> io.println }
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

pub fn part1(chars) {
  chars |> list.map(decode) |> list.filter(is_uncorrupted) |> list.length
}

pub fn part2(chars) {
  chars |> list.map(decode) |> list.map(value) |> sum
}

pub fn part3(chars) {
  let assert [Uncorrupted(value), ..rest] = chars |> list.map(decode)
  amend(value, rest, value)
}

fn amend(prev, chars, total) {
  case chars {
    [] -> total
    [head, ..tail] -> {
      case head {
        Uncorrupted(v) -> amend(v, tail, total + v)
        Corrupted(_) -> {
          let corrected = prev * 2 - 5 |> shift
          amend(corrected, tail, total + corrected)
        }
      }
    }
  }
}

fn shift(x) {
  case x < 1, x > 52 {
    True, _ -> x + 52
    _, True -> x - 52
    _, _ -> x
  }
}

fn decode(c) {
  let assert Ok(lowercase) = regexp.from_string("[a-z]")
  let assert Ok(uppercase) = regexp.from_string("[A-Z]")

  case regexp.check(content: c, with: lowercase) {
    True -> Uncorrupted(to_ascii(c) - to_ascii("a") + 1)
    False -> {
      case regexp.check(content: c, with: uppercase) {
        True -> Uncorrupted(to_ascii(c) - to_ascii("A") + 27)
        False -> Corrupted(c)
      }
    }
  }
}

fn to_ascii(c) {
  let assert Ok(utf) = c |> string.to_utf_codepoints |> list.first
  utf |> string.utf_codepoint_to_int
}

fn is_uncorrupted(c) {
  case c {
    Uncorrupted(_) -> True
    Corrupted(_) -> False
  }
}

fn value(c) {
  case c {
    Corrupted(_) -> 0
    Uncorrupted(v) -> v
  }
}

fn sum(lst) {
  lst |> list.fold(0, int.add)
}

pub fn parse(input) {
  input |> string.replace("\n", "") |> string.to_graphemes
}
