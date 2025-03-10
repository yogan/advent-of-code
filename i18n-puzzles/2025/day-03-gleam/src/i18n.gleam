import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/string
import simplifile

pub fn length_ok(pw: String) {
  string.length(pw) >= 4 && string.length(pw) <= 12
}

pub fn contains_digit(pw: String) {
  let assert Ok(re) = regexp.from_string("\\d")
  regexp.check(with: re, content: pw)
}

pub fn contains_uppercase(pw: String) {
  let assert Ok(re) = regexp.from_string("\\p{Lu}")
  regexp.check(with: re, content: pw)
}

pub fn contains_lowercase(pw: String) {
  let assert Ok(re) = regexp.from_string("\\p{Ll}")
  regexp.check(with: re, content: pw)
}

pub fn contains_non_ascii(pw: String) {
  let assert Ok(re) = regexp.from_string("[^\\x00-\\x7F]")
  regexp.check(with: re, content: pw)
}

pub fn is_valid(pw: String) {
  length_ok(pw)
  && contains_digit(pw)
  && contains_uppercase(pw)
  && contains_lowercase(pw)
  && contains_non_ascii(pw)
}

fn bool_to_int(b: Bool) -> Int {
  case b {
    True -> 1
    False -> 0
  }
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          content
          |> string.trim
          |> string.split("\n")
          |> list.map(is_valid)
          |> list.map(bool_to_int)
          |> list.fold(0, int.add)
          |> int.to_string
          |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
