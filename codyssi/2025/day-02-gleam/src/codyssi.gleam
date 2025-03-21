import argv
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let #(add, mult, pow, qualities) = parse(content)
          let do = fn(f) {
            f(qualities, add, mult, pow) |> int.to_string |> io.println
          }
          do(part1)
          do(part2)
          do(part3)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(qualities, add, mult, pow) {
  qualities |> median |> price(add, mult, pow)
}

pub fn part2(qualities, add, mult, pow) {
  qualities
  |> list.filter(fn(q) { q % 2 == 0 })
  |> list.fold(0, int.add)
  |> price(add, mult, pow)
}

pub fn part3(qualities, add, mult, pow) {
  let assert Ok(best) =
    qualities
    |> list.map(fn(q) { #(q, price(q, add, mult, pow)) })
    |> list.filter(fn(qp) { qp.1 <= 15_000_000_000_000 })
    |> list.map(fn(qp) { qp.0 })
    |> list.sort(int.compare)
    |> list.last

  best
}

fn price(m, add, mult, pow) {
  let assert Ok(m) = int.power(m, pow |> int.to_float)
  float.truncate(m) * mult + add
}

pub fn median(nums) {
  let assert Ok(idx) = nums |> list.length |> int.divide(2)
  let assert Ok(elem) =
    nums
    |> list.sort(int.compare)
    |> list.index_map(fn(x, i) { #(i, x) })
    |> list.find(fn(ix) { ix.0 == idx })
  elem.1
}

pub fn parse(input: String) -> #(Int, Int, Int, List(Int)) {
  let assert Ok(re) = regexp.from_string("\\d+")
  let assert Ok(numbers) =
    input
    |> regexp.scan(with: re)
    |> list.map(fn(match) { match.content |> int.parse })
    |> result.all
  let assert [add, mult, pow, ..qualities] = numbers

  #(add, mult, pow, qualities)
}
