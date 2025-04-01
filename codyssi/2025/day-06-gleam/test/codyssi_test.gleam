import codyssi
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const sample = "t#UD$%%DVd*L?^p?S$^@#@@$pF$?xYJ$LLv$@%EXO&$*iSFZuT!^VMHy#zKISHa
Bj?e*#&yRVdemc#?
&#Q%j&ev*#YWRi@?mNQ@eK"

pub fn part1_test() {
  sample |> codyssi.parse |> codyssi.part1 |> should.equal(59)
}

pub fn part2_test() {
  sample |> codyssi.parse |> codyssi.part2 |> should.equal(1742)
}
