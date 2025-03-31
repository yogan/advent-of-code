import codyssi
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = "(-16, -191)
(92, 186)
(157, -75)
(39, -132)
(-42, 139)
(-74, -150)
(200, 197)
(-106, 105)"

pub fn part1_test() {
  sample |> codyssi.parse |> codyssi.part1 |> should.equal(226)
}

pub fn manhatten_distance_test() {
  codyssi.manhatten_distance(#(5, 2), #(3, 7)) |> should.equal(7)
}

pub fn parse_test() {
  sample
  |> codyssi.parse
  |> should.equal([
    #(-16, -191),
    #(92, 186),
    #(157, -75),
    #(39, -132),
    #(-42, 139),
    #(-74, -150),
    #(200, 197),
    #(-106, 105),
  ])
}
