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
          [part1(qualities, add, mult, pow), part2(qualities, add, mult, pow)]
          |> list.map(int.to_string)
          |> string.join("\n")
          |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(qualities, add, mult, pow) {
  median(qualities) |> price(add, mult, pow)
}

pub fn part2(qualities, add, mult, pow) {
  qualities
  |> list.filter(fn(q) { q % 2 == 0 })
  |> list.fold(0, int.add)
  |> price(add, mult, pow)
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
