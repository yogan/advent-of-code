import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
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
          |> list.map(rotations)
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

pub fn rotations(s: String) -> Int {
  case
    list.range(from: 1, to: 23)
    |> list.map(fn(n) { #(rotate(s, n) |> has_odysseus, n) })
    |> list.find(fn(pair) { pair.0 == True })
  {
    Ok(#(_, n)) -> n
    _ -> 0
  }
}

fn has_odysseus(s) {
  let assert Ok(re) = regexp.from_string("Οδυσσε(υς|ως|ι|α|υ)")
  s |> regexp.check(with: re)
}

pub fn rotate(s, n) {
  s
  |> normalize_sigmas
  |> string.to_graphemes
  |> list.map(fn(c) { rot_char(c, n) })
  |> string.join(with: "")
  |> replace_trailing_sigmas
}

fn normalize_sigmas(s) {
  s |> string.replace(each: "ς", with: "σ")
}

fn replace_trailing_sigmas(s) {
  let assert Ok(re) = regexp.from_string("σ\\b")
  regexp.replace(each: re, in: s, with: "ς")
}

fn rot_char(c, n) {
  let upper = [
    "Α", "Β", "Γ", "Δ", "Ε", "Ζ", "Η", "Θ", "Ι", "Κ", "Λ", "Μ", "Ν", "Ξ", "Ο",
    "Π", "Ρ", "Σ", "Τ", "Υ", "Φ", "Χ", "Ψ", "Ω",
  ]

  let lower = [
    "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ν", "ξ", "ο",
    "π", "ρ", "σ", "τ", "υ", "φ", "χ", "ψ", "ω",
  ]

  let rot = fn(chars, c, n) {
    let chars_idx = chars |> list.index_map(fn(c, i) { #(i, c) })
    let assert Ok(#(idx, _)) = chars_idx |> list.find(fn(ic) { ic.1 == c })
    let ii = { idx + n } % { upper |> list.length }
    let assert Ok(#(_, cc)) = chars_idx |> list.find(fn(ic) { ic.0 == ii })
    cc
  }

  case list.contains(upper, c), list.contains(lower, c) {
    True, _ -> rot(upper, c, n)
    _, True -> rot(lower, c, n)
    _, _ -> c
  }
}
