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
          let nums = parse(content)
          let do = fn(f) { nums |> f |> int.to_string |> io.println }
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

pub fn part1(lines) {
  solve(lines, fn(x) { x })
}

pub fn part2(lines) {
  solve(lines, lossy_compress)
}

pub fn part3(lines) {
  solve(lines, rle_compress)
}

fn solve(lines, f) {
  lines
  |> list.map(f)
  |> list.map(string.to_graphemes)
  |> list.map(fn(chars) { chars |> list.map(memory) |> sum })
  |> sum
}

fn memory(char) {
  case int.parse(char) {
    Ok(n) -> n
    _ -> to_ascii(char) - to_ascii("A") + 1
  }
}

fn to_ascii(char) {
  let assert Ok(utf_char) = char |> string.to_utf_codepoints |> list.first
  utf_char |> string.utf_codepoint_to_int
}

pub fn lossy_compress(line) {
  let len = line |> string.length
  let assert Ok(keep) = len |> int.divide(10)
  let removed = len - keep * 2 |> int.to_string

  let l = line |> string.slice(length: keep, at_index: 0)
  let r = line |> string.slice(length: keep, at_index: -keep)

  l <> removed <> r
}

pub fn rle_compress(line) {
  line
  |> string.to_graphemes
  |> rle_rec([])
  |> list.reverse
  |> string.join("")
}

fn rle_rec(chars, acc) {
  case chars {
    [] -> acc
    [char, ..rest] -> {
      let #(l, r) = rest |> list.split_while(fn(c) { c == char })
      let len = l |> list.length |> int.add(1) |> int.to_string
      rle_rec(r, [len <> char, ..acc])
    }
  }
}

pub fn parse(input) {
  input |> string.trim |> string.split("\n")
}

fn sum(nums) {
  nums |> list.fold(0, int.add)
}
