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
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

pub fn part1(input) {
  input
  |> process
  |> dict.values
  |> list.sort(int.compare)
  |> list.reverse
  |> list.take(3)
  |> list.fold(0, int.add)
}

pub fn process(input) {
  let #(accounts, transactions) = input

  transactions
  |> list.fold(accounts, fn(acc, transaction) {
    let #(from, to, amount) = transaction
    acc
    |> dict.update(from, fn(balance) {
      let assert Some(balance) = balance
      balance - amount
    })
    |> dict.update(to, fn(balance) {
      let assert Some(balance) = balance
      balance + amount
    })
  })
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
