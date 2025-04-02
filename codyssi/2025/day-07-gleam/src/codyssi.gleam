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
  let #(freqs, swaps, index) = input
  swaps |> list.fold(freqs, swap) |> get(index)
}

pub fn part2(input) {
  let #(freqs, swaps, index) = input
  swaps |> to_tripples |> list.fold(freqs, triple_swap) |> get(index)
}

pub fn part3(input) {
  let #(freqs, swaps, index) = input
  swaps |> list.fold(freqs, block_swap) |> get(index)
}

fn get(freqs, index) {
  let assert Ok(res) = freqs |> dict.get(index)
  res
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

fn block_swap(freqs, indices) {
  let #(x, y) = indices
  let len = block_length(freqs |> dict.size, indices)
  swap_with_offset(freqs, x, y, 0, len)
}

fn swap_with_offset(freqs, x, y, offset, max_offset) {
  case offset >= max_offset {
    True -> freqs
    False -> {
      swap(freqs, #(x + offset, y + offset))
      |> swap_with_offset(x, y, offset + 1, max_offset)
    }
  }
}

fn swap(freqs, indices) {
  let #(x, y) = indices
  let assert Ok(x_freq) = freqs |> dict.get(x)
  let assert Ok(y_freq) = freqs |> dict.get(y)
  freqs |> dict.insert(x, y_freq) |> dict.insert(y, x_freq)
}

fn triple_swap(freqs, indices) {
  let #(x, y, z) = indices
  let assert Ok(x_freq) = freqs |> dict.get(x)
  let assert Ok(y_freq) = freqs |> dict.get(y)
  let assert Ok(z_freq) = freqs |> dict.get(z)
  freqs
  |> dict.insert(x, z_freq)
  |> dict.insert(y, x_freq)
  |> dict.insert(z, y_freq)
}

pub fn block_length(len, indices) {
  let #(x, y) = indices
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
