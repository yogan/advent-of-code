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
          |> list.map(fn(pair) { pair |> list.map(eval_length) |> calc_area })
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

pub fn eval_length(str) {
  let assert [unit, ..value] = str |> string.to_graphemes |> list.reverse
  #(value |> list.reverse |> eval_number, unit |> eval_unit)
}

pub fn eval_number(chars) {
  case chars {
    [] -> 0
    ["一"] -> 1
    ["二"] -> 2
    ["三"] -> 3
    ["四"] -> 4
    ["五"] -> 5
    ["六"] -> 6
    ["七"] -> 7
    ["八"] -> 8
    ["九"] -> 9
    ["十"] -> 10
    ["百"] -> 100
    ["千"] -> 1000
    _ ->
      case chars |> list.contains("億") {
        True -> {
          let #(l, r) = chars |> list.split_while(fn(x) { x != "億" })
          100_000_000 * eval_number(l) + eval_number(r |> list.drop(1))
        }
        False -> {
          case chars |> list.contains("万") {
            True -> {
              let #(l, r) = chars |> list.split_while(fn(x) { x != "万" })
              10_000 * eval_number(l) + eval_number(r |> list.drop(1))
            }
            False -> {
              let assert [a, b, ..rest] = chars
              case a {
                "十" -> 10 + eval_number([b, ..rest])
                "百" -> 100 + eval_number([b, ..rest])
                "千" -> 1000 + eval_number([b, ..rest])
                _ -> eval_number([a]) * eval_number([b]) + eval_number(rest)
              }
            }
          }
        }
      }
  }
}

fn eval_unit(unit) {
  case unit {
    "尺" -> 1.0
    "間" -> 6.0
    "丈" -> 10.0
    "町" -> 360.0
    "里" -> 12_960.0
    "毛" -> 1.0 /. 10_000.0
    "厘" -> 1.0 /. 1000.0
    "分" -> 1.0 /. 100.0
    "寸" -> 1.0 /. 10.0
    _ -> panic as { "unknown unit " <> unit }
  }
}
