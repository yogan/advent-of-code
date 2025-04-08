import argv
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/string
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          let input = parse(content)
          let do = fn(f) { input |> f |> int.to_string |> io.println }
          do(part1)
          do(part2)
          do(part3)
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(input) {
  input |> sum_top3(process(_, capped: False))
}

pub fn part2(input) {
  input |> sum_top3(process(_, capped: True))
}

pub fn part3(input) {
  input |> sum_top3(process_with_debts)
}

fn sum_top3(input, proc_fn) {
  input
  |> proc_fn
  |> dict.values
  |> list.sort(int.compare)
  |> list.reverse
  |> list.take(3)
  |> list.fold(0, int.add)
}

pub fn process(input, capped capped) {
  let #(accounts, transactions) = input

  transactions
  |> list.fold(accounts, fn(acc, transaction) {
    let #(from, to, amount) = transaction

    let amount = case capped {
      True -> {
        let assert Ok(from_balance) = dict.get(acc, from)
        int.min(amount, from_balance)
      }
      False -> amount
    }

    acc
    |> dict.upsert(from, fn(balance) {
      let assert Some(balance) = balance
      balance - amount
    })
    |> dict.upsert(to, fn(balance) {
      let assert Some(balance) = balance
      balance + amount
    })
  })
}

fn process_with_debts(input) {
  let #(accounts, transactions) = input

  let accounts_with_debts =
    accounts |> dict.map_values(fn(_, balance) { #(balance, []) })

  transactions
  |> list.fold(accounts_with_debts, process_transaction_with_debts)
  |> dict.map_values(fn(_, pair) { pair.0 })
}

pub fn process_transaction_with_debts(accounts, transaction) {
  let #(from, to, amount) = transaction
  let assert Ok(#(from_balance, _)) = dict.get(accounts, from)
  let payment = int.min(from_balance, amount)
  let debt = amount - payment

  accounts
  |> dict.upsert(from, fn(pair) {
    let assert Some(#(balance, debts)) = pair
    let debts = case debt {
      0 -> debts
      _ -> list.append(debts, [#(to, debt)])
    }
    #(balance - payment, debts)
  })
  |> repay_debts(to, payment)
}

fn repay_debts(accounts, debitor, income) {
  case income <= 0 {
    True -> accounts
    False -> {
      let assert Ok(#(balance, debts)) = dict.get(accounts, debitor)
      case debts {
        // no debts, just book the income
        [] -> accounts |> dict.insert(debitor, #(balance + income, []))

        [#(creditor, amount), ..remaining_debts] -> {
          case income >= amount {
            // repay the full amount, remove debt
            True -> {
              accounts
              |> dict.insert(debitor, #(balance, remaining_debts))
              |> repay_debts(creditor, amount)
              |> repay_debts(debitor, income - amount)
            }

            // repay part of the debt, keep it (reduced)
            False -> {
              let partial_amount = int.min(income, amount)
              let reduced_debt = #(creditor, amount - partial_amount)
              accounts
              |> dict.insert(
                debitor,
                #(balance, [reduced_debt, ..remaining_debts]),
              )
              |> repay_debts(creditor, partial_amount)
              |> repay_debts(debitor, income - partial_amount)
            }
          }
        }
      }
    }
  }
}

pub fn parse(input) {
  let assert [top, bottom] = input |> string.trim |> string.split("\n\n")

  let accounts =
    top
    |> string.split("\n")
    |> list.map(fn(line) {
      let assert [name, balance] = line |> string.split(" HAS ")
      let assert Ok(balance) = balance |> int.parse
      #(name, balance)
    })
    |> dict.from_list

  let transactions =
    bottom
    |> string.split("\n")
    |> list.map(fn(line) {
      let assert [_, from, _, to, _, amount] = line |> string.split(" ")
      let assert Ok(amount) = amount |> int.parse
      #(from, to, amount)
    })

  #(accounts, transactions)
}
