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
          let chars = parse(content)
          let do = fn(f) { chars |> f |> int.to_string |> io.println }
          do(part1)
          do(part2)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(chars) {
  let assert Ok(re) = regex.from_string("[a-zA-Z]")
  chars
  |> string.to_graphemes
  |> list.filter(fn(c) { regex.check(content: c, with: re) })
  |> list.length
}

pub fn part2(chars) {
  chars |> string.to_graphemes |> list.map(to_value) |> sum
}

fn to_value(c) {
  let assert Ok(lowercase) = regex.from_string("[a-z]")
  let assert Ok(uppercase) = regex.from_string("[A-Z]")

  case regex.check(content: c, with: lowercase) {
    True -> to_ascii(c) - to_ascii("a") + 1
    False -> {
      case regex.check(content: c, with: uppercase) {
        True -> to_ascii(c) - to_ascii("A") + 27
        False -> 0
      }
    }
  }
}

fn to_ascii(c) {
  let assert Ok(utf) = c |> string.to_utf_codepoints |> list.first
  utf |> string.utf_codepoint_to_int
}

fn sum(lst) {
  lst |> list.fold(0, int.add)
}

pub fn parse(input) {
  input |> string.replace("\n", "")
}
