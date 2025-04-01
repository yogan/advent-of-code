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

pub fn parse(input) {
  input |> string.replace("\n", "")
}
