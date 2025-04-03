import codyssi
import gleam/dict
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn part1_test() {
  sample |> codyssi.parse |> codyssi.part1 |> should.equal(2870)
}

pub fn process_test() {
  sample
  |> codyssi.parse
  |> codyssi.process
  |> should.equal(
    dict.from_list([
      #("Alpha", 199),
      #("Bravo", 499),
      #("Charlie", -359),
      #("Delta", 745),
      #("Echo", 148),
      #("Foxtrot", 1626),
    ]),
  )
}

pub fn parse_test() {
  sample
  |> codyssi.parse
  |> should.equal(
    #(
      dict.from_list([
        #("Alpha", 131),
        #("Bravo", 804),
        #("Charlie", 348),
        #("Delta", 187),
        #("Echo", 649),
        #("Foxtrot", 739),
      ]),
      [
        #("Echo", "Foxtrot", 328),
        #("Charlie", "Bravo", 150),
        #("Charlie", "Delta", 255),
        #("Alpha", "Delta", 431),
        #("Foxtrot", "Alpha", 230),
        #("Echo", "Foxtrot", 359),
        #("Echo", "Alpha", 269),
        #("Delta", "Foxtrot", 430),
        #("Bravo", "Echo", 455),
        #("Charlie", "Delta", 302),
      ],
    ),
  )
}

const sample = "Alpha HAS 131
Bravo HAS 804
Charlie HAS 348
Delta HAS 187
Echo HAS 649
Foxtrot HAS 739

FROM Echo TO Foxtrot AMT 328
FROM Charlie TO Bravo AMT 150
FROM Charlie TO Delta AMT 255
FROM Alpha TO Delta AMT 431
FROM Foxtrot TO Alpha AMT 230
FROM Echo TO Foxtrot AMT 359
FROM Echo TO Alpha AMT 269
FROM Delta TO Foxtrot AMT 430
FROM Bravo TO Echo AMT 455
FROM Charlie TO Delta AMT 302
"
