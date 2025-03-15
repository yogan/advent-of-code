import gleam/dict
import gleeunit
import gleeunit/should
import i18n

pub fn main() {
  gleeunit.main()
}

pub fn parse_test() {
  i18n.parse("16-05-18: Margot, Frank")
  |> should.equal(#("16-05-18", ["Margot", "Frank"]))
}

pub fn find_prefs_test() {
  [
    #("16-05-18", ["Margot", "Frank"]),
    #("02-17-04", ["Peter", "Elise"]),
    #("06-02-29", ["Peter", "Margot"]),
    #("31-09-11", ["Elise", "Frank"]),
    #("09-11-01", ["Peter", "Frank", "Elise"]),
    #("11-09-01", ["Margot", "Frank"]),
  ]
  |> i18n.find_prefs
  |> should.equal(
    dict.from_list([
      #("Margot", ["DD-MM-YY"]),
      #("Peter", ["MM-DD-YY"]),
      #("Frank", ["YY-MM-DD"]),
      #("Elise", ["YY-DD-MM"]),
    ]),
  )
}

pub fn can_be_dmy_test() {
  let dmy = "DD-MM-YY"
  i18n.can_be("16-05-18", dmy) |> should.be_true
  // 17 > 12 months
  i18n.can_be("02-17-18", dmy) |> should.be_false
  i18n.can_be("06-02-29", dmy) |> should.be_true
  // only 30 days in September
  i18n.can_be("31-09-11", dmy) |> should.be_false
  i18n.can_be("09-11-01", dmy) |> should.be_true
  i18n.can_be("11-09-01", dmy) |> should.be_true
}

pub fn can_be_mdy_test() {
  let mdy = "MM-DD-YY"
  // 16 > 12 months
  i18n.can_be("16-05-18", mdy) |> should.be_false
  i18n.can_be("02-17-18", mdy) |> should.be_true
  i18n.can_be("06-02-29", mdy) |> should.be_true
  // 31 > 12 months
  i18n.can_be("31-09-11", mdy) |> should.be_false
  i18n.can_be("09-11-01", mdy) |> should.be_true
  i18n.can_be("11-09-01", mdy) |> should.be_true
}

pub fn can_be_ydm_test() {
  let ydm = "YY-DD-MM"
  // 18 > 12 months
  i18n.can_be("16-05-18", ydm) |> should.be_false
  // 18 > 12 months
  i18n.can_be("02-17-18", ydm) |> should.be_false
  // 29 > 12 months
  i18n.can_be("06-02-29", ydm) |> should.be_false
  i18n.can_be("31-09-11", ydm) |> should.be_true
  i18n.can_be("09-11-01", ydm) |> should.be_true
  i18n.can_be("11-09-01", ydm) |> should.be_true
}

pub fn can_be_ymd_test() {
  let ymd = "YY-MM-DD"
  i18n.can_be("16-05-18", ymd) |> should.be_true
  // 17 > 12 months
  i18n.can_be("02-17-18", ymd) |> should.be_false
  // 2006 had only 28 days in February
  i18n.can_be("06-02-29", ymd) |> should.be_false
  i18n.can_be("31-09-11", ymd) |> should.be_true
  i18n.can_be("09-11-01", ymd) |> should.be_true
  i18n.can_be("11-09-01", ymd) |> should.be_true
}
