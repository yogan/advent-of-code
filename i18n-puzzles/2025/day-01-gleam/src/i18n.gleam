import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import simplifile

pub fn bytes_and_chars(line: String) -> #(Int, Int) {
  #(string.byte_size(line), string.length(line))
}

pub fn costs(bytes_and_chars: #(Int, Int)) -> Int {
  let #(bytes, chars) = bytes_and_chars

  case bytes > 160, chars > 140 {
    True, True -> 0
    True, False -> 7
    False, True -> 11
    False, False -> 13
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
          |> list.map(bytes_and_chars)
          |> list.map(costs)
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
