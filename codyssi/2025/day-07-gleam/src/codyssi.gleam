import argv
import gleam/dict
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

pub fn part1(input) {
  let #(freqs, swaps, test_index) = input

  let freqs =
    freqs
    |> list.index_map(fn(f, i) { #(i + 1, f) })
    |> dict.from_list

  let assert Ok(res) =
    swaps
    |> list.fold(freqs, fn(freqs, swap) {
      let #(a, b) = swap
      let assert Ok(a_val) = freqs |> dict.get(a)
      let assert Ok(b_val) = freqs |> dict.get(b)
      freqs |> dict.insert(a, b_val) |> dict.insert(b, a_val)
    })
    |> dict.get(test_index)

  res
}

pub fn parse(input) {
  let assert [freqs, swaps, index] = string.split(input, "\n\n")

  let assert Ok(freqs) =
    freqs |> string.split("\n") |> list.map(int.parse) |> result.all

  let swaps =
    swaps
    |> string.split("\n")
    |> list.map(fn(line) {
      let assert [a, b] = string.split(line, "-")
      let assert Ok(a) = int.parse(a)
      let assert Ok(b) = int.parse(b)
      #(a, b)
    })

  let assert Ok(index) = index |> string.trim |> int.parse

  #(freqs, swaps, index)
}
