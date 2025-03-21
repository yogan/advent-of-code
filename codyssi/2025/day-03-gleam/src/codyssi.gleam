import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/result
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let boxes = parse(content)
          boxes |> part1 |> int.to_string |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(boxes) {
  boxes
  |> list.map(fn(pair) {
    let assert [from, to] = pair
    to - from + 1
  })
  |> list.fold(0, int.add)
}

pub fn parse(input: String) -> List(List(Int)) {
  let assert Ok(re) = regexp.from_string("\\d+")
  input
  |> regexp.scan(with: re)
  |> list.map(fn(match) { match.content |> int.parse })
  |> result.values
  |> list.sized_chunk(into: 2)
}
