import gleam/list
import gleam/string
import gleeunit
import gleeunit/should
import i18n

pub fn main() {
  gleeunit.main()
}

pub fn to_length_test() {
  i18n.to_length("二百四十二町") |> should.equal(#(242, 360.0))
  i18n.to_length("六百十二分") |> should.equal(#(612, 1.0 /. 100.0))
}

pub fn to_number_test() {
  ["三百", "三百二十一", "四千", "五万", "九万九千九百九十九", "四十二万四十二", "九億八千七百六十五万四千三百二十一"]
  |> list.map(string.to_graphemes)
  |> list.map(i18n.to_number)
  |> should.equal([300, 321, 4000, 50_000, 99_999, 420_042, 987_654_321])
}
