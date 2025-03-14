import gleam/list
import gleeunit
import gleeunit/should
import i18n

pub fn main() {
  gleeunit.main()
}

pub fn length_ok_test() {
  "iS0" |> i18n.length_ok |> should.be_false
  "V8AeC1S7KhP4Ļu" |> i18n.length_ok |> should.be_false
  "pD9Ĉ*jXh" |> i18n.length_ok |> should.be_true
}

pub fn required_characters() {
  "pD9Ĉ*jXh" |> i18n.required_characters |> should.be_false
  "E1-0" |> i18n.required_characters |> should.be_false
  "tqd~üō" |> i18n.required_characters |> should.be_false
  "pD9Ĉ*jXh" |> i18n.required_characters |> should.be_true
}

pub fn no_recurring_letters_test() {
  "ĕnz2cymE" |> i18n.no_recurring_letters |> should.be_false
  "tqd~üō" |> i18n.no_recurring_letters |> should.be_true
}

pub fn is_valid_test() {
  let passwords = [
    "iS0", "V8AeC1S7KhP4Ļu", "pD9Ĉ*jXh", "E1-0", "ĕnz2cymE", "tqd~üō",
    "IgwQúPtd9", "k2lp79ąqV",
  ]
  passwords
  |> list.map(i18n.is_valid)
  |> should.equal([False, False, False, False, False, False, True, True])
}
