import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/order
import gleam/string
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let coordinates = parse(content)
          let do = fn(f) { coordinates |> f |> int.to_string |> io.println }
          do(part1)
          do(part2)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(coordinates) {
  let distances =
    coordinates
    |> list.map(fn(coord) { manhatten_distance(#(0, 0), coord) })
    |> list.sort(by: int.compare)

  let assert Ok(closest) = list.first(distances)
  let assert Ok(furthest) = list.last(distances)

  furthest - closest
}

pub fn part2(coordinates) {
  let comp = fn(a, b) {
    let #(a_dist, #(ax, ay)) = a
    let #(b_dist, #(bx, by)) = b

    case int.compare(a_dist, b_dist) {
      order.Eq ->
        case int.compare(ax, bx) {
          order.Eq -> int.compare(ay, by)
          ord -> ord
        }
      ord -> ord
    }
  }

  let assert Ok(#(_, closest)) =
    coordinates
    |> list.map(fn(coord) { #(manhatten_distance(#(0, 0), coord), coord) })
    |> list.sort(by: comp)
    |> list.first

  let assert Ok(#(res, _)) =
    coordinates
    |> list.filter(fn(coord) { coord != closest })
    |> list.map(fn(coord) { #(manhatten_distance(closest, coord), coord) })
    |> list.sort(by: comp)
    |> list.first

  res
}

pub fn manhatten_distance(a, b) {
  let #(ax, ay) = a
  let #(bx, by) = b
  let dx = ax - bx |> int.absolute_value
  let dy = ay - by |> int.absolute_value
  dx + dy
}

pub fn parse(input) {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(fn(line) {
    let assert [Ok(x), Ok(y)] =
      line
      |> string.drop_left(1)
      |> string.drop_right(1)
      |> string.split(", ")
      |> list.map(int.parse)
    #(x, y)
  })
}
