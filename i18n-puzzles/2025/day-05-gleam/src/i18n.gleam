import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

pub fn walk(input: String) -> String {
  let lines = string.split(input, "\n")
  let assert Ok(first_line) = list.first(lines)
  let width = string.length(first_line)

  walk_rec(lines, 0, width, "")
}

fn walk_rec(lines: List(String), idx: Int, width: Int, res: String) -> String {
  case lines {
    [] -> res
    [line, ..rest] -> {
      string.slice(from: line, at_index: idx, length: 1)
      <> walk_rec(rest, { idx + 2 } % width, width, res)
    }
  }
}

pub fn count_poo(input: String) -> Int {
  walk(input)
  |> string.split("")
  |> list.filter(fn(c) { c == "💩" })
  |> list.length
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          content |> count_poo |> int.to_string |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
