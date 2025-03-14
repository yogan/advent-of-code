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
          |> list.filter(is_valid)
          |> list.fold(0, fn(acc, _) { acc + 1 })
          |> int.to_string
          |> io.println
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn is_valid(pw: String) {
  length_ok(pw) && required_characters(pw) && no_recurring_letters(pw)
}

pub fn length_ok(pw: String) {
  string.length(pw) >= 4 && string.length(pw) <= 12
}

pub fn required_characters(pw: String) {
  let options = regexp.Options(case_insensitive: True, multi_line: False)
  let assert Ok(digit) = regexp.from_string("\\d")
  let assert Ok(vowel) = regexp.compile("[aeiou]", with: options)
  let assert Ok(consonant) =
    regexp.compile("[bcdfghjklmnpqrstvwxyz]", with: options)

  pw
  |> strip_combining_chars
  |> fn(s) {
    regexp.check(s, with: digit)
    && regexp.check(s, with: vowel)
    && regexp.check(s, with: consonant)
  }
}

pub fn no_recurring_letters(pw: String) {
  let normalized_chars =
    pw
    |> strip_combining_chars
    |> string.lowercase
    |> string.to_graphemes

  list.length(normalized_chars) == list.unique(normalized_chars) |> list.length
}

@external(erlang, "unicode", "characters_to_nfd_binary")
fn characters_to_nfd_binary(s: String) -> String

fn strip_combining_chars(s: String) -> String {
  let assert Ok(combined_char) = regexp.from_string("\\p{Mn}")
  s
  |> characters_to_nfd_binary
  |> regexp.replace(each: combined_char, with: "")
}
