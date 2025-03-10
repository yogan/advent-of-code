import gleam/list
import gleeunit
import gleeunit/should
import i18n

pub fn main() {
  gleeunit.main()
}

const passwords = [
  "d9Ō", "uwI.E9GvrnWļbzO", "ž-2á", "Ģ952W*F4", "?O6JQf", "xi~Rťfsa",
  "r_j4XcHŔB", "71äĜ3",
]

pub fn length_ok_test() {
  passwords
  |> list.map(i18n.length_ok)
  |> should.equal([False, False, True, True, True, True, True, True])
}

pub fn contains_digit_test() {
  passwords
  |> list.map(i18n.contains_digit)
  |> should.equal([True, True, True, True, True, False, True, True])
}

pub fn contains_uppercase_test() {
  passwords
  |> list.map(i18n.contains_uppercase)
  |> should.equal([True, True, False, True, True, True, True, True])
}

pub fn contains_lowercase_test() {
  passwords
  |> list.map(i18n.contains_lowercase)
  |> should.equal([True, True, True, False, True, True, True, True])
}

pub fn contains_non_ascii_test() {
  passwords
  |> list.map(i18n.contains_non_ascii)
  |> should.equal([True, True, True, True, False, True, True, True])
}

pub fn is_valid_test() {
  passwords
  |> list.map(i18n.is_valid)
  |> should.equal([False, False, False, False, False, False, True, True])
}
