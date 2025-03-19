import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          content |> part1 |> int.to_string |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(input) {
  let assert #([start, ..corrections], offsets) = parse(input)

  corrections
  |> list.zip(offsets)
  |> list.fold(start, fn(acc, pair) {
    acc
    + case pair {
      #(n, "-") -> -n
      #(n, "+") -> n
      _ -> panic
    }
  })
}

pub fn parse(input: String) -> #(List(Int), List(String)) {
  let assert [offsets, ..rst] =
    input |> string.trim |> string.split("\n") |> list.reverse

  let assert Ok(nums) = rst |> list.map(int.parse) |> list.reverse |> result.all

  #(nums, offsets |> string.to_graphemes)
}
