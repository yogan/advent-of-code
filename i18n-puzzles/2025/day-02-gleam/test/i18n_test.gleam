import gleeunit
import gleeunit/should
import i18n

pub fn main() {
  gleeunit.main()
}

pub fn normalize_test() {
  let first_four_expected = "2019-06-05T12:15:00+00:00"

  "2019-06-05T08:15:00-04:00"
  |> i18n.normalize
  |> should.equal(first_four_expected)

  "2019-06-05T14:15:00+02:00"
  |> i18n.normalize
  |> should.equal(first_four_expected)

  "2019-06-05T17:45:00+05:30"
  |> i18n.normalize
  |> should.equal(first_four_expected)

  "2019-06-05T05:15:00-07:00"
  |> i18n.normalize
  |> should.equal(first_four_expected)

  "2011-02-01T09:15:00-03:00"
  |> i18n.normalize
  |> should.equal("2011-02-01T12:15:00+00:00")

  "2011-02-01T09:15:00-05:00"
  |> i18n.normalize
  |> should.equal("2011-02-01T14:15:00+00:00")
}

pub fn find_four_identical_test() {
  let expected = "2019-06-05T12:15:00+00:00"

  [expected, expected, expected, expected, "a", "b"]
  |> i18n.find_four_identical
  |> should.equal(Ok(expected))

  ["a", expected, expected, expected, expected, "b"]
  |> i18n.find_four_identical
  |> should.equal(Ok(expected))

  [expected, "a", expected, "b", expected, "x", "", expected, "   "]
  |> i18n.find_four_identical
  |> should.equal(Ok(expected))

  [expected, "a", expected, "b", expected, "x", ""]
  |> i18n.find_four_identical
  |> should.equal(Error(Nil))
}
