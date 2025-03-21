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

pub fn part1(nums) {
  nums
  |> list.sized_chunk(into: 2)
  |> list.map(fn(pair) {
    let assert [from, to] = pair
    1 + to - from
  })
  |> list.fold(0, int.add)
}

pub fn part2(nums) {
  nums
  |> list.sized_chunk(into: 4)
  |> list.map(pile_size)
  |> list.fold(0, int.add)
}

pub fn pile_size(piles) {
  let assert [l1, r1, l2, r2] = piles
  let l = 1 + r1 - l1
  let r = 1 + r2 - l2
  l + r - overlap(l1, r1, l2, r2)
}

fn overlap(l1, r1, l2, r2) {
  case r1 < l2 || r2 < l1 {
    True -> 0
    False -> {
      let l = int.max(l1, l2)
      let r = int.min(r1, r2)
      1 + r - l
    }
  }
}

pub fn parse(input: String) -> List(Int) {
  let assert Ok(re) = regexp.from_string("\\d+")
  input
  |> regexp.scan(with: re)
  |> list.map(fn(match) { match.content |> int.parse })
  |> result.values
}
