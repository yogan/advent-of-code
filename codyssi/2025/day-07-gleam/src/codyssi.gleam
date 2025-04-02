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
          do(part2)
          do(part3)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(input) {
  let #(freqs, swaps, test_index) = input

  let assert Ok(res) =
    swaps
    |> list.fold(freqs, fn(freqs, swap) {
      let #(x, y) = swap
      let assert Ok(x_freq) = freqs |> dict.get(x)
      let assert Ok(y_freq) = freqs |> dict.get(y)
      freqs |> dict.insert(x, y_freq) |> dict.insert(y, x_freq)
    })
    |> dict.get(test_index)

  res
}

pub fn part2(input) {
  let #(freqs, swaps, test_index) = input

  let assert Ok(res) =
    swaps
    |> to_tripples
    |> list.fold(freqs, fn(freqs, swap) {
      let #(x, y, z) = swap
      let assert Ok(x_freq) = freqs |> dict.get(x)
      let assert Ok(y_freq) = freqs |> dict.get(y)
      let assert Ok(z_freq) = freqs |> dict.get(z)
      freqs
      |> dict.insert(x, z_freq)
      |> dict.insert(y, x_freq)
      |> dict.insert(z, y_freq)
    })
    |> dict.get(test_index)

  res
}

pub fn part3(input) {
  let #(freqs, swaps, test_index) = input

  let assert Ok(res) =
    swaps
    |> list.fold(freqs, fn(freqs, swap) {
      let #(x, y) = swap
      let len = block_length(freqs |> dict.size, swap)
      swap_with_offset(freqs, x, y, 0, len)
    })
    |> dict.get(test_index)

  res
}

fn swap_with_offset(freqs, x, y, offset, max_offset) {
  case offset >= max_offset {
    True -> freqs
    False -> {
      let xx = x + offset
      let yy = y + offset
      let assert Ok(x_freq) = freqs |> dict.get(xx)
      let assert Ok(y_freq) = freqs |> dict.get(yy)
      let freqs = freqs |> dict.insert(xx, y_freq) |> dict.insert(yy, x_freq)
      swap_with_offset(freqs, x, y, offset + 1, max_offset)
    }
  }
}

pub fn to_tripples(pairs) {
  let assert Ok(head) = pairs |> list.first

  pairs
  |> list.append([head])
  |> list.window_by_2
  |> list.map(fn(pair) {
    let #(#(x, y), #(z, _)) = pair
    #(x, y, z)
  })
}

pub fn block_length(len, swap) {
  let #(x, y) = swap
  let l = int.min(x, y)
  let r = int.max(x, y)

  int.min(r - l, len - r + 1)
}

pub fn parse(input) {
  let assert [freqs, swaps, index] = string.split(input, "\n\n")

  let assert Ok(freqs) =
    freqs |> string.split("\n") |> list.map(int.parse) |> result.all

  let freqs =
    freqs
    |> list.index_map(fn(f, i) { #(i + 1, f) })
    |> dict.from_list

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
