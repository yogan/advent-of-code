import argv
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/regex
import gleam/result
import gleam/string
import simplifile

pub type Circuit =
  List(Connection)

pub type Connection {
  Gate(in: Gate, out: String)
  Connection(from: String, to: String)
  Data(value: Int, for: String)
}

pub type Operand {
  Wire(String)
  Value(Int)
}

pub type Gate {
  And(Operand, Operand)
  Or(Operand, Operand)
  LShift(Operand, Int)
  RShift(Operand, Int)
  Not(String)
}

type Cache =
  dict.Dict(String, Int)

fn run(circuit: Circuit) {
  let part1 =
    circuit
    |> emulate("a")

  let part2 =
    circuit
    |> replace_wire("b", part1)
    |> emulate("a")

  [part1, part2]
  |> list.map(int.to_string)
  |> string.join("\n")
  |> io.println
}

pub fn emulate(circuit: Circuit, wire: String) -> Int {
  circuit
  |> backtrace(wire, dict.new())
  |> fn(x: #(_, _)) { x.0 }
}

fn backtrace(circuit: Circuit, wire: String, cache: Cache) -> #(Int, Cache) {
  case dict.get(cache, wire) {
    Ok(val) -> #(val, cache)

    _ -> {
      case get_wire(circuit, wire) {
        Connection(from, _) -> backtrace(circuit, from, cache)

        Data(value, _) -> #(value, dict.insert(cache, wire, value))

        Gate(in: gate, out: _) ->
          case gate {
            And(Value(a), Value(b)) -> {
              let res = int.bitwise_and(a, b)
              #(res, dict.insert(cache, wire, res))
            }
            And(Value(a), Wire(b)) -> {
              let #(b, cache) = backtrace(circuit, b, cache)
              let res = int.bitwise_and(a, b)
              #(res, dict.insert(cache, wire, res))
            }
            And(Wire(a), Value(b)) -> {
              let #(a, cache) = backtrace(circuit, a, cache)
              let res = int.bitwise_and(a, b)
              #(res, dict.insert(cache, wire, res))
            }
            And(Wire(a), Wire(b)) -> {
              let #(a, cache) = backtrace(circuit, a, cache)
              let #(b, cache) = backtrace(circuit, b, cache)
              let res = int.bitwise_and(a, b)
              #(res, dict.insert(cache, wire, res))
            }

            Or(Value(a), Value(b)) -> {
              let res = int.bitwise_or(a, b)
              #(res, dict.insert(cache, wire, res))
            }
            Or(Value(a), Wire(b)) -> {
              let #(b, cache) = backtrace(circuit, b, cache)
              let res = int.bitwise_or(a, b)
              #(res, dict.insert(cache, wire, res))
            }
            Or(Wire(a), Value(b)) -> {
              let #(a, cache) = backtrace(circuit, a, cache)
              let res = int.bitwise_or(a, b)
              #(res, dict.insert(cache, wire, res))
            }
            Or(Wire(a), Wire(b)) -> {
              let #(a, cache) = backtrace(circuit, a, cache)
              let #(b, cache) = backtrace(circuit, b, cache)
              let res = int.bitwise_or(a, b)
              #(res, dict.insert(cache, wire, res))
            }

            LShift(Value(a), b) -> {
              let res = int.bitwise_shift_left(a, b)
              #(res, dict.insert(cache, wire, res))
            }
            LShift(Wire(a), b) -> {
              let #(a, cache) = backtrace(circuit, a, cache)
              let res = int.bitwise_shift_left(a, b)
              #(res, dict.insert(cache, wire, res))
            }

            RShift(Value(a), b) -> {
              let res = int.bitwise_shift_right(a, b)
              #(res, dict.insert(cache, wire, res))
            }
            RShift(Wire(a), b) -> {
              let #(a, cache) = backtrace(circuit, a, cache)
              let res = int.bitwise_shift_right(a, b)
              #(res, dict.insert(cache, wire, res))
            }

            Not(a) -> {
              // `int.bitwise_not` cannot be used as ints in Gleam are signed and
              // of variable bit length depending on the target platform.
              let bitwise_not_unsigned_16_bit = fn(x) { 65_535 - x }
              let #(a, cache) = backtrace(circuit, a, cache)
              let res = bitwise_not_unsigned_16_bit(a)
              #(res, dict.insert(cache, wire, res))
            }
          }
      }
    }
  }
}

fn get_wire(circuit: Circuit, wire: String) -> Connection {
  let assert Ok(connection) = list.find(circuit, fn(c) { goes_to(c, wire) })
  connection
}

fn replace_wire(circuit: Circuit, wire: String, value: Int) -> Circuit {
  [
    Data(value: value, for: wire),
    ..list.filter(circuit, fn(c) { !goes_to(c, wire) })
  ]
}

fn goes_to(connection: Connection, wire: String) -> Bool {
  case connection {
    Gate(_, out: w) -> w == wire
    Connection(_, to: w) -> w == wire
    Data(_, for: w) -> w == wire
  }
}

pub fn parse_input(input: String) -> Result(Circuit, String) {
  input
  |> string.trim
  |> string.split("\n")
  |> list.map(parse_line)
  |> result.all
}

fn parse_line(line: String) -> Result(Connection, String) {
  case string.split(line, " -> ") {
    [left, right] -> {
      let assert Ok(re) = regex.from_string("(AND|OR|LSHIFT|RSHIFT|NOT)")
      case regex.scan(with: re, content: left) {
        [regex.Match("AND", _)] -> {
          let assert [a, b] = string.split(left, " AND ")
          case int.parse(a), int.parse(b) {
            Ok(a), Ok(b) -> Ok(Gate(in: And(Value(a), Value(b)), out: right))
            Ok(a), _ -> Ok(Gate(in: And(Value(a), Wire(b)), out: right))
            _, Ok(b) -> Ok(Gate(in: And(Wire(a), Value(b)), out: right))
            _, _ -> Ok(Gate(in: And(Wire(a), Wire(b)), out: right))
          }
        }
        [regex.Match("OR", _)] -> {
          let assert [a, b] = string.split(left, " OR ")
          case int.parse(a), int.parse(b) {
            Ok(a), Ok(b) -> Ok(Gate(in: Or(Value(a), Value(b)), out: right))
            Ok(a), _ -> Ok(Gate(in: Or(Value(a), Wire(b)), out: right))
            _, Ok(b) -> Ok(Gate(in: Or(Wire(a), Value(b)), out: right))
            _, _ -> Ok(Gate(in: Or(Wire(a), Wire(b)), out: right))
          }
        }
        [regex.Match("LSHIFT", _)] -> {
          let assert [a, b] = string.split(left, " LSHIFT ")
          case int.parse(a), int.parse(b) {
            Ok(a), Ok(b) -> Ok(Gate(in: LShift(Value(a), b), out: right))
            _, Ok(b) -> Ok(Gate(in: LShift(Wire(a), b), out: right))
            _, _ -> Error("invalid value \"" <> a <> "\"")
          }
        }
        [regex.Match("RSHIFT", _)] -> {
          let assert [a, b] = string.split(left, " RSHIFT ")
          case int.parse(a), int.parse(b) {
            Ok(a), Ok(b) -> Ok(Gate(in: RShift(Value(a), b), out: right))
            _, Ok(b) -> Ok(Gate(in: RShift(Wire(a), b), out: right))
            _, _ -> Error("invalid value \"" <> a <> "\"")
          }
        }
        [regex.Match("NOT", _)] -> {
          let assert [_, a] = string.split(left, "NOT ")
          Ok(Gate(in: Not(a), out: right))
        }
        _ ->
          case int.parse(left) {
            Ok(val) -> Ok(Data(value: val, for: right))
            Error(_) -> Ok(Connection(from: left, to: right))
          }
      }
    }
    _ -> Error("invalid line \"" <> line <> "\"")
  }
}

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      case simplifile.read(from: filename) {
        Ok(content) -> {
          case parse_input(content) {
            Ok(circuit) -> run(circuit)
            Error(msg) ->
              io.println("Error parsing " <> filename <> ": " <> msg)
          }
        }
        Error(_) -> io.println("Error reading " <> filename)
      }
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}
