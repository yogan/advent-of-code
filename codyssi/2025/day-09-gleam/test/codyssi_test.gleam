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

pub fn part2_test() {
  sample |> codyssi.parse |> codyssi.part2 |> should.equal(2542)
}

pub fn part3_test() {
  sample |> codyssi.parse |> codyssi.part3 |> should.equal(2511)
}

pub fn process_uncapped_test() {
  sample
  |> codyssi.parse
  |> codyssi.process(capped: False)
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

pub fn process_capped_test() {
  sample
  |> codyssi.parse
  |> codyssi.process(capped: True)
  |> should.equal(
    dict.from_list([
      #("Alpha", 230),
      #("Bravo", 499),
      #("Charlie", 0),
      #("Delta", 86),
      #("Echo", 455),
      #("Foxtrot", 1588),
    ]),
  )
}

pub fn process_transaction_with_debts_test() {
  let accounts_with_debts =
    dict.from_list([
      #("Alpha", #(131, [])),
      #("Bravo", #(804, [])),
      #("Charlie", #(348, [])),
      #("Delta", #(187, [])),
      #("Echo", #(649, [])),
      #("Foxtrot", #(739, [])),
    ])

  accounts_with_debts
  |> codyssi.process_transaction_with_debts(#("Echo", "Foxtrot", 328))
  |> should.equal(
    dict.from_list([
      #("Alpha", #(131, [])),
      #("Bravo", #(804, [])),
      #("Charlie", #(348, [])),
      #("Delta", #(187, [])),
      #("Echo", #(649 - 328, [])),
      #("Foxtrot", #(739 + 328, [])),
    ]),
  )

  accounts_with_debts
  |> codyssi.process_transaction_with_debts(#("Echo", "Foxtrot", 650))
  |> should.equal(
    dict.from_list([
      #("Alpha", #(131, [])),
      #("Bravo", #(804, [])),
      #("Charlie", #(348, [])),
      #("Delta", #(187, [])),
      #("Echo", #(0, [#("Foxtrot", 1)])),
      #("Foxtrot", #(739 + 649, [])),
    ]),
  )

  accounts_with_debts
  |> codyssi.process_transaction_with_debts(#("Echo", "Foxtrot", 650))
  |> codyssi.process_transaction_with_debts(#("Alpha", "Echo", 31))
  |> should.equal(
    dict.from_list([
      #("Alpha", #(131 - 31, [])),
      #("Bravo", #(804, [])),
      #("Charlie", #(348, [])),
      #("Delta", #(187, [])),
      #("Echo", #(31 - 1, [])),
      #("Foxtrot", #(739 + 649 + 1, [])),
    ]),
  )

  dict.from_list([
    #("Alpha", #(131, [])),
    #("Bravo", #(804, [])),
    #("Charlie", #(348, [])),
    #("Delta", #(187, [])),
    #("Echo", #(0, [#("Foxtrot", 1), #("Bravo", 500)])),
    #("Foxtrot", #(739 + 649, [])),
  ])
  |> codyssi.process_transaction_with_debts(#("Alpha", "Echo", 31))
  |> should.equal(
    dict.from_list([
      #("Alpha", #(131 - 31, [])),
      #("Bravo", #(804 + 30, [])),
      #("Charlie", #(348, [])),
      #("Delta", #(187, [])),
      #("Echo", #(0, [#("Bravo", 470)])),
      #("Foxtrot", #(739 + 649 + 1, [])),
    ]),
  )
}

pub fn process_transaction_with_debts_mutual_test() {
  dict.from_list([
    #("Alpha", #(100, [#("Bravo", 20)])),
    #("Bravo", #(200, [#("Alpha", 10)])),
    #("Charlie", #(300, [])),
  ])
  |> codyssi.process_transaction_with_debts(#("Charlie", "Alpha", 50))
  |> should.equal(
    dict.from_list([
      #("Alpha", #(140, [])),
      #("Bravo", #(210, [])),
      #("Charlie", #(250, [])),
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
