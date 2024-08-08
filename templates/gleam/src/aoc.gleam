import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

type Box =
  #(Int, Int, Int)

fn run(boxes: List(Box)) {
  boxes
  |> part1
  |> int.to_string
  |> io.println
}

pub fn part1(boxes: List(Box)) -> Int {
  let volume = fn(box) {
    let #(l, w, h) = box
    l * w * h
  }

  boxes
  |> list.map(volume)
  |> list.fold(0, int.add)
}

pub fn parse_line(line: String) -> Result(Box, String) {
  let nums =
    line
    |> string.split("x")
    |> list.map(int.parse)

  case nums {
    [Ok(a), Ok(b), Ok(c)] -> Ok(#(a, b, c))
    _ -> Error("invalid line \"" <> line <> "\"")
  }
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let boxes =
            content
            |> string.trim
            |> string.split("\n")
            |> list.map(parse_line)
            |> result.all

          case boxes {
            Ok(boxes) -> run(boxes)
            Error(msg) ->
              io.println("Error parsing " <> filename <> ": " <> msg)
          }
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
