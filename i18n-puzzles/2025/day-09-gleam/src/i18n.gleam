import argv
import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/set.{type Set}
import gleam/string
import simplifile
import tempo
import tempo/date

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let diary =
            content
            |> string.trim
            |> string.split("\n")
            |> list.map(parse)

          solve(diary, find_prefs(diary), set.new())
          |> set.to_list
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

pub fn parse(line: String) -> #(String, List(String)) {
  let assert [date, names] = string.split(line, on: ": ")
  #(date, string.split(names, on: ", "))
}

pub fn find_prefs(
  lines: List(#(String, List(String))),
) -> Dict(String, List(String)) {
  let prefs =
    lines
    |> list.flat_map(fn(pair) { pair.1 })
    |> list.map(fn(name) {
      #(name, ["DD-MM-YY", "MM-DD-YY", "YY-DD-MM", "YY-MM-DD"])
    })
    |> dict.from_list

  list.fold(over: lines, from: prefs, with: fn(prefs, pair) {
    let #(date, names) = pair
    list.fold(over: names, from: prefs, with: fn(prefs, name) {
      update_prefs(date, name, prefs)
    })
  })
}

fn update_prefs(
  date: String,
  name: String,
  prefs: Dict(String, List(String)),
) -> Dict(String, List(String)) {
  let assert Ok(cur_prefs) = dict.get(prefs, name)
  dict.insert(
    prefs,
    name,
    list.filter(cur_prefs, fn(pref) { can_be(date, pref) }),
  )
}

pub fn can_be(date_str: String, format: String) -> Bool {
  case date.parse(date_str, tempo.CustomDate(format)) {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn solve(
  diary: List(#(String, List(String))),
  prefs: Dict(String, List(String)),
  res: Set(String),
) -> Set(String) {
  case diary {
    [] -> res
    [pair, ..rest] -> {
      let #(date, names) = pair
      let writers = find_writers(date, names, prefs) |> set.from_list
      solve(rest, prefs, set.union(res, writers))
    }
  }
}

fn find_writers(date, names, prefs) {
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
