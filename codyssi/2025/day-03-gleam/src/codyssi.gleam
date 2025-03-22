import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/result
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

pub fn part1(nums) {
  nums |> list.sized_chunk(into: 2) |> list.map(size) |> sum
}

pub fn part2(nums) {
  nums |> list.sized_chunk(into: 4) |> list.map(pile_size) |> sum
}

pub fn part3(nums) {
  nums
  |> list.sized_chunk(into: 4)
  |> list.window(2)
  |> list.map(combine_pairs)
  |> list.map(merge_ranges)
  |> list.flatten
  |> list.map(size)
  |> max
}

fn size(pair) {
  let assert [from, to] = pair
  1 + to - from
}

pub fn pile_size(piles) {
  let assert [l1, r1, l2, r2] = piles
  let l = 1 + r1 - l1
  let r = 1 + r2 - l2
  l + r - overlap_size(piles)
}

pub fn overlap(piles) {
  let assert [l1, r1, l2, r2] = piles
  case r1 < l2 || r2 < l1 {
    True -> []
    False -> [int.max(l1, l2), int.min(r1, r2)]
  }
}

fn overlap_size(piles) {
  case piles |> overlap {
    [l, r] -> 1 + r - l
    _ -> 0
  }
}

pub fn combine(piles) {
  let assert [l1, r1, l2, r2] = piles
  case r1 < l2 || r2 < l1 {
    True -> [[l1, r1], [l2, r2]]
    False -> [[int.min(l1, l2), int.max(r1, r2)]]
  }
}

pub fn combine_pairs(pair) {
  let assert [a, b] = pair
  case combine(a), combine(b) {
    [a1], [b1] -> [list.append(a1, b1)]
    [a1], [b1, b2] -> [list.append(a1, b1), list.append(a1, b2)]
    [a1, a2], [b1] -> [list.append(a1, b1), list.append(a2, b1)]
    [a1, a2], [b1, b2] -> [
      list.append(a1, b1),
      list.append(a1, b2),
      list.append(a2, b1),
      list.append(a2, b2),
    ]
    _, _ -> panic as "invalid piles"
  }
  |> list.map(combine)
  |> list.flatten
}

pub fn merge_ranges(pairs) {
  pairs
  |> list.sort(compare_left)
  |> merge_rec([])
  |> list.reverse
}

fn compare_left(a, b) {
  let assert [l1, _] = a
  let assert [l2, _] = b
  int.compare(l1, l2)
}

fn merge_rec(remaining, res) {
  case remaining {
    [] -> res
    pairs -> {
      let assert [[l, _], ..] = pairs
      let #(ls, rest) =
        pairs |> list.split_while(fn(p) { p |> list.first == Ok(l) })
      let r = max_r(ls)
      let #(mergeable, rest) = find_mergeable(r, rest, [])
      let r = max_r([[l, r], ..mergeable])
      merge_rec(rest, [[l, r], ..res])
    }
  }
}

fn find_mergeable(r, pairs, res) {
  case pairs {
    [] -> #(res, pairs)
    _ -> {
      let #(mergeable, rest) =
        pairs
        |> list.split_while(fn(p) {
          let assert [pl, _] = p
          pl <= r + 1
        })

      case mergeable {
        [] -> #(mergeable, rest)
        _ -> {
          find_mergeable(max_r(mergeable), rest, res |> list.append(mergeable))
        }
      }
    }
  }
}

fn sum(nums) {
  nums |> list.fold(0, int.add)
}

fn max(nums) {
  let assert Ok(max) = nums |> list.sort(int.compare) |> list.last
  max
}

fn max_r(pairs) {
  let assert Ok(rs) = pairs |> list.map(list.last) |> result.all
  rs |> max
}

pub fn parse(input: String) -> List(Int) {
  let assert Ok(re) = regexp.from_string("\\d+")
  input
  |> regexp.scan(with: re)
  |> list.map(fn(match) { match.content |> int.parse })
  |> result.values
}
