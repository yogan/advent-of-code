import argv
import gleam/float
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
          content
          |> string.trim
          |> string.split("\n")
          |> list.map(fn(line) { string.split(line, " × ") })
          |> list.map(fn(pair) { pair |> list.map(to_length) |> calc_area })
          |> list.fold(0, fn(acc, x) { acc + x })
          |> int.to_string
          |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

fn calc_area(pair) {
  let assert [w, h] = pair
  shaku_to_meter(w) *. shaku_to_meter(h) |> float.round
}

fn shaku_to_meter(pair) {
  let #(n, shaku) = pair
  int.to_float(n) *. shaku *. 10.0 /. 33.0
}

pub fn to_length(str) {
  let assert [u, ..value] = str |> string.to_graphemes |> list.reverse
  #(value |> list.reverse |> to_number, u |> unit)
}

pub fn to_number(chars) {
  case chars {
    [] -> 1
    _ -> {
      let #(out, remaining) =
        ["億", "万", "千", "百", "十"]
        |> list.fold(#(0, chars), fn(acc, symbol) {
          let #(res, chars) = acc
          case chars |> list.contains(symbol) {
            True -> {
              let #(l, r) = chars |> list.split_while(fn(x) { x != symbol })
              #(res + to_number(l) * power(symbol), r |> list.drop(1))
            }
            False -> #(res, chars)
          }
        })
      case remaining {
        [] -> out
        [d] -> out + digit(d)
        _ -> panic
      }
    }
  }
}

fn digit(d) {
  case d {
    "一" -> 1
    "二" -> 2
    "三" -> 3
    "四" -> 4
    "五" -> 5
    "六" -> 6
    "七" -> 7
    "八" -> 8
    "九" -> 9
    _ -> panic as { "unknown digit " <> d }
  }
}

fn power(p) {
  case p {
    "十" -> 10
    "百" -> 100
    "千" -> 1000
    "万" -> 10_000
    "億" -> 100_000_000
    _ -> panic as { "unknown power " <> p }
  }
}

fn unit(u) {
  case u {
    "毛" -> 1.0 /. 10_000.0
    "厘" -> 1.0 /. 1000.0
    "分" -> 1.0 /. 100.0
    "寸" -> 1.0 /. 10.0
    "尺" -> 1.0
    "間" -> 6.0
    "丈" -> 10.0
    "町" -> 360.0
    "里" -> 12_960.0
    _ -> panic as { "unknown unit " <> u }
  }
}
