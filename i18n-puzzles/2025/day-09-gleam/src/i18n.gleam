import argv
import gleam/dict
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile
import tempo
import tempo/date

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          content
          |> string.trim
          |> string.split("\n")
          |> list.map(parse)
          |> fn(diary) { solve(diary, find_prefs(diary), []) }
          |> list.sort(by: string.compare)
          |> string.join(" ")
          |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn parse(line) {
  let assert [date, names] = string.split(line, on: ": ")
  #(date, string.split(names, on: ", "))
}

pub fn find_prefs(lines: List(#(String, List(String)))) {
  list.fold(
    over: lines,
    from: lines
      |> list.flat_map(fn(pair) { pair.1 })
      |> list.map(fn(name) {
        #(name, ["DD-MM-YY", "MM-DD-YY", "YY-DD-MM", "YY-MM-DD"])
      })
      |> dict.from_list,
    with: fn(prefs, pair) {
      let #(date, names) = pair
      list.fold(over: names, from: prefs, with: fn(prefs, name) {
        update_prefs(date, name, prefs)
      })
    },
  )
}

fn update_prefs(date, name, prefs) {
  let assert Ok(cur_prefs) = dict.get(prefs, name)
  dict.insert(prefs, name, list.filter(cur_prefs, can_be(date, _)))
}

pub fn can_be(date_str, format) {
  date.parse(date_str, tempo.CustomDate(format)) |> result.is_ok
}

fn solve(diary, prefs, res) {
  case diary {
    [] -> res
    [pair, ..rest] -> {
      solve(rest, prefs, list.append(res, find_writers(pair, prefs)))
    }
  }
}

fn find_writers(pair, prefs) {
  let #(date, names) = pair
  case date {
    "09-11-01" -> filter_names(names, prefs, "MM-DD-YY")
    "11-09-01" -> filter_names(names, prefs, "DD-MM-YY")
    "01-11-09" -> filter_names(names, prefs, "YY-DD-MM")
    "01-09-11" -> filter_names(names, prefs, "YY-MM-DD")
    _ -> []
  }
}

fn filter_names(names, prefs, format) {
  list.filter(names, fn(name) {
    case dict.get(prefs, name) {
      Ok(prefs) -> list.contains(prefs, format)
      Error(_) -> False
    }
  })
}
