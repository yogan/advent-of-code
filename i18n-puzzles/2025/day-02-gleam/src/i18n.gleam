import argv
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import simplifile
import tempo
import tempo/datetime
import tempo/naive_datetime

pub fn normalize(timestamp: String) {
  datetime.literal(timestamp)
  |> datetime.apply_offset
  |> naive_datetime.as_utc
  |> datetime.format(tempo.Custom("YYYY-MM-DDTHH:mm:ssZ"))
}

pub fn find_four_identical(timestamps: List(String)) {
  let inc = fn(x) { option.unwrap(x, 0) |> int.add(1) }
  let update = fn(dict, datetime) { dict.upsert(dict, datetime, inc) }

  timestamps
  |> list.fold(dict.new(), update)
  |> dict.filter(fn(_, count) { count == 4 })
  |> dict.keys
  |> list.first
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          content
          |> string.trim
          |> string.split("\n")
          |> list.map(normalize)
          |> find_four_identical
          |> result.unwrap("No four identical timestamps found")
          |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
