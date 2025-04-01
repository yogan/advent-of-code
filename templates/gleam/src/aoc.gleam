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
          let input = parse(content)
          let do = fn(f) { input |> f |> int.to_string |> io.println }
          do(part1)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

type Box =
  #(Int, Int, Int)

pub fn part1(boxes: List(Box)) -> Int {
  let volume = fn(box) {
    let #(l, w, h) = box
    l * w * h
  }

  boxes |> list.map(volume) |> sum
}

fn sum(lst) {
  lst |> list.fold(0, int.add)
}

fn parse(content) {
  let assert Ok(res) =
    content
    |> string.trim
    |> string.split("\n")
    |> list.map(parse_line)
    |> result.all

  res
}

pub fn parse_line(line: String) -> Result(Box, String) {
  let nums = line |> string.split("x") |> list.map(int.parse)

  case nums {
    [Ok(a), Ok(b), Ok(c)] -> Ok(#(a, b, c))
    _ -> Error("invalid line \"" <> line <> "\"")
  }
}
