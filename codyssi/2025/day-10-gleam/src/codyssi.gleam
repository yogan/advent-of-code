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
          let grid = parse(content)
          let do = fn(f) { grid |> f |> int.to_string |> io.println }
          do(part1)
          do(part2)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(grid) {
  list.append(grid, list.transpose(grid)) |> list.map(sum) |> min
}

/// This is infinite enough for our needs.
const inf = 1_000_000_000

pub fn part2(grid) {
  let start = #(1, 1)
  let goal = #(15, 15)

  let grid =
    to_coords(grid)
    |> dict.filter(fn(key, _) { key.0 <= goal.0 && key.1 <= goal.1 })

  let assert Ok(start_cost) = dict.get(grid, start)

  let dist =
    grid
    |> dict.map_values(fn(_, _) { inf })
    |> dict.insert(start, start_cost)

  let queue = grid |> dict.keys

  let dist = shortest_path(grid, dist, goal, queue)

  let assert Ok(path_cost) = dict.get(dist, goal)
  // 95 is wrong :-/
  path_cost
}

fn shortest_path(grid, dist, goal, queue: List(#(Int, Int))) {
  case queue {
    [] -> panic as "no path found"

    _ -> {
      let assert Ok(#(u, u_cost)) =
        queue
        |> list.map(fn(v) { #(v, dict.get(dist, v) |> result.unwrap(inf)) })
        |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
        |> list.first

      let queue = queue |> list.filter(fn(v) { v != u })

      let dist =
        [#(u.0 + 1, u.1), #(u.0, u.1 + 1)]
        |> list.filter(list.contains(queue, _))
        |> list.map(fn(v) {
          let assert Ok(v_cost) = dict.get(grid, v)
          #(v, u_cost + v_cost)
        })
        |> list.fold(dist, fn(acc, pair) {
          let #(v, alt) = pair
          case dict.get(dist, v) {
            Ok(dist_v) if dist_v <= alt -> acc
            _ -> dict.insert(acc, v, alt)
          }
        })

      todo
    }
    // [pair, ..] if pair.0 == goal -> dist
    // [pair, ..rest] -> {
    // // let assert Ok(u_cost) = dict.get(dist, u)
    // let #(u, u_cost) = pair
    // let #(side_len, _) = goal
    //
    // let #(dist, prev, next) =
    //   [#(u.0 + 1, u.1), #(u.0, u.1 + 1)]
    //   |> list.filter(fn(coord) { coord.0 <= side_len && coord.1 <= side_len })
    //   |> list.map(fn(v) {
    //     let assert Ok(v_cost) = dict.get(grid, v)
    //     #(v, u_cost + v_cost)
    //   })
    //   |> list.fold(#(dist, prev, []), fn(acc, pair) {
    //     let #(dist, prev, next) = acc
    //     let #(v, alt) = pair
    //     case dict.get(dist, v) {
    //       Ok(dist_v) if dist_v <= alt -> acc
    //       _ -> #(dict.insert(dist, v, alt), dict.insert(prev, v, u), [
    //         #(v, alt),
    //         ..next
    //       ])
    //     }
    //   })
    //
    // // make queue a priority queue
    // let queue =
    //   list.append(next, rest)
    //   |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
    // // |> echo
    //
    // shortest_path(grid, dist, prev, goal, queue)
    // }
  }
}

pub fn to_coords(grid) {
  grid
  |> list.index_map(fn(row, i) {
    row |> list.index_map(fn(x, j) { #(#(i + 1, j + 1), x) })
  })
  |> list.flatten
  |> dict.from_list
}

fn sum(lst) {
  list.fold(lst, 0, int.add)
}

fn min(lst) {
  let assert Ok(minimum) = list.sort(lst, int.compare) |> list.first
  minimum
}

pub fn parse(input) {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(fn(line) {
    line
    |> string.split(" ")
    |> list.map(fn(x) {
      let assert Ok(n) = int.parse(x)
      n
    })
  })
}
