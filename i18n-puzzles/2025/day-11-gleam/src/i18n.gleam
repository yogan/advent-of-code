import argv
import gleam/bool
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
          |> list.map(find_matching_rotation)
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

pub fn find_matching_rotation(str: String) -> Int {
  case
    list.range(from: 1, to: 23)
    |> list.drop_while(fn(n) { rotate(str, n) |> has_odysseus |> bool.negate })
  {
    [] -> 0
    [n, ..] -> n
  }
}

fn has_odysseus(str) {
  let assert Ok(odysseus_variants) = regexp.from_string("Οδυσσε(υς|ως|ι|α|υ)")
  str |> regexp.check(with: odysseus_variants)
}

pub fn rotate(str, n) {
  str
  |> normalize_sigmas
  |> string.to_graphemes
  |> list.map(fn(c) { rot_char(c, n) })
  |> string.join(with: "")
  |> replace_trailing_sigmas
}

fn normalize_sigmas(str) {
  str |> string.replace(each: "ς", with: "σ")
}

fn replace_trailing_sigmas(str) {
  let assert Ok(trailing_small_sigma) = regexp.from_string("σ\\b")
  regexp.replace(each: trailing_small_sigma, in: str, with: "ς")
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
