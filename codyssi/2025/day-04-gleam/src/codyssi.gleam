import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let nums = parse(content)
          let do = fn(f) { nums |> f |> int.to_string |> io.println }
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
  |> list.map(string.to_graphemes)
  |> list.map(fn(chars) { chars |> list.map(memory) |> sum })
  |> sum
}

pub fn part2(lines) {
  lines
  |> list.map(lossy_compress)
  |> list.map(string.to_graphemes)
  |> list.map(fn(chars) { chars |> list.map(memory) |> sum })
  |> sum
}

fn memory(char) {
  case int.parse(char) {
    Ok(n) -> n
    _ -> to_ascii(char) - to_ascii("A") + 1
  }
}

fn to_ascii(char) {
  let assert Ok(utf_char) = char |> string.to_utf_codepoints |> list.first
  utf_char |> string.utf_codepoint_to_int
}

pub fn lossy_compress(line) {
  let len = line |> string.length
  let assert Ok(keep) = len |> int.divide(10)
  let removed = len - keep * 2 |> int.to_string

  let l = line |> string.slice(length: keep, at_index: 0)
  let r = line |> string.slice(length: keep, at_index: -keep)

  l <> removed <> r
}

pub fn parse(input) {
  input |> string.trim |> string.split("\n")
}

fn sum(nums) {
  nums |> list.fold(0, int.add)
}
