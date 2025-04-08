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
          let grid = parse(content)
          let do = fn(f) { grid |> f |> int.to_string |> io.println }
          do(part1)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(grid) {
  list.append(grid, list.transpose(grid)) |> list.map(sum) |> min
}

fn sum(lst) {
  list.fold(lst, 0, int.add)
}

fn min(lst) {
  let assert Ok(minimum) = list.sort(lst, int.compare) |> list.first
  minimum
}

pub fn parse(input) {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(fn(line) {
    line
    |> string.split(" ")
    |> list.map(fn(x) {
      let assert Ok(n) = int.parse(x)
      n
    })
  })
}
