import argv
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/regex
import gleam/result
import gleam/string
import simplifile

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

fn run(connections: List(Connection)) {
  connections
  |> part1("a")
  |> result.try(fn(x) { Ok(int.to_string(x)) })
  |> result.unwrap_both
  |> io.println
}

pub fn part1(connections: List(Connection), wire: String) -> Result(Int, String) {
  connections
  |> backtrace(wire, dict.new())
  |> result.map(fn(x) { x.0 })
}

fn backtrace(
  connections: List(Connection),
  wire: String,
  cache: Cache,
) -> Result(#(Int, Cache), String) {
  case dict.get(cache, wire) {
    Ok(val) -> {
      Ok(#(val, cache))
    }

    Error(_) -> {
      let con =
        list.find(connections, fn(c) {
          case c {
            Gate(_, out: w) -> w == wire
            Connection(_, to: w) -> w == wire
            Data(_, for: w) -> w == wire
          }
        })

      case con {
        Ok(Connection(from: from, to: _)) -> backtrace(connections, from, cache)
        Ok(Data(value, _)) -> Ok(#(value, dict.insert(cache, wire, value)))
        Ok(Gate(in: gate, out: _)) ->
          case gate {
            And(Value(a), Value(b)) -> {
              let res = int.bitwise_and(a, b)
              Ok(#(res, dict.insert(cache, wire, res)))
            }
            And(Value(a), Wire(b)) -> {
              case backtrace(connections, b, cache) {
                Ok(#(b_val, b_cache)) -> {
                  let res = int.bitwise_and(a, b_val)
                  Ok(#(res, dict.insert(b_cache, wire, res)))
                }
                _ ->
                  Error("failed to eval " <> int.to_string(a) <> " AND " <> b)
              }
            }
            And(Wire(a), Value(b)) -> {
              case backtrace(connections, a, cache) {
                Ok(#(a_val, a_cache)) -> {
                  let res = int.bitwise_and(a_val, b)
                  Ok(#(res, dict.insert(a_cache, wire, res)))
                }
                _ ->
                  Error("failed to eval " <> a <> " AND " <> int.to_string(b))
              }
            }
            And(Wire(a), Wire(b)) -> {
              case backtrace(connections, a, cache) {
                Ok(#(a_val, a_cache)) -> {
                  case backtrace(connections, b, a_cache) {
                    Ok(#(b_val, b_cache)) -> {
                      let res = int.bitwise_and(a_val, b_val)
                      Ok(#(res, dict.insert(b_cache, wire, res)))
                    }
                    _ -> Error("failed to eval " <> a <> " AND " <> b)
                  }
                }
                _ -> Error("failed to eval " <> a <> " AND " <> b)
              }
            }
            Or(Value(a), Value(b)) -> {
              let res = int.bitwise_or(a, b)
              Ok(#(res, dict.insert(cache, wire, res)))
            }
            Or(Value(a), Wire(b)) -> {
              case backtrace(connections, b, cache) {
                Ok(#(b_val, b_cache)) -> {
                  let res = int.bitwise_or(a, b_val)
                  Ok(#(res, dict.insert(b_cache, wire, res)))
                }
                _ -> Error("failed to eval " <> int.to_string(a) <> " OR " <> b)
              }
            }
            Or(Wire(a), Value(b)) -> {
              case backtrace(connections, a, cache) {
                Ok(#(a_val, a_cache)) -> {
                  let res = int.bitwise_or(a_val, b)
                  Ok(#(res, dict.insert(a_cache, wire, res)))
                }
                _ -> Error("failed to eval " <> a <> " OR " <> int.to_string(b))
              }
            }
            Or(Wire(a), Wire(b)) -> {
              case backtrace(connections, a, cache) {
                Ok(#(a_val, a_cache)) -> {
                  case backtrace(connections, b, a_cache) {
                    Ok(#(b_val, b_cache)) -> {
                      let res = int.bitwise_or(a_val, b_val)
                      Ok(#(res, dict.insert(b_cache, wire, res)))
                    }
                    _ -> Error("failed to eval " <> a <> " OR " <> b)
                  }
                }
                _ -> Error("failed to eval " <> a <> " OR " <> b)
              }
            }
            LShift(Value(a), b) -> {
              let res = int.bitwise_shift_left(a, b)
              Ok(#(res, dict.insert(cache, wire, res)))
            }
            LShift(Wire(a), b) -> {
              case backtrace(connections, a, cache) {
                Ok(#(val, c)) -> {
                  let res = int.bitwise_shift_left(val, b)
                  Ok(#(res, dict.insert(c, wire, res)))
                }
                _ -> Error("failed to eval " <> a <> " << " <> int.to_string(b))
              }
            }
            RShift(Value(a), b) -> {
              let res = int.bitwise_shift_right(a, b)
              Ok(#(res, dict.insert(cache, wire, res)))
            }
            RShift(Wire(a), b) -> {
              case backtrace(connections, a, cache) {
                Ok(#(val, c)) -> {
                  let res = int.bitwise_shift_right(val, b)
                  Ok(#(res, dict.insert(c, wire, res)))
                }
                _ -> Error("failed to eval " <> a <> " >> " <> int.to_string(b))
              }
            }
            Not(a) -> {
              // `int.bitwise_not` cannot be used as ints in Gleam are signed and
              // of variable bit length depending on the target platform.
              let bitwise_not_unsigned_16_bit = fn(x) { 65_535 - x }
              case backtrace(connections, a, cache) {
                Ok(#(val, c)) -> {
                  let res = bitwise_not_unsigned_16_bit(val)
                  Ok(#(res, dict.insert(c, wire, res)))
                }
                _ -> Error("failed to eval NOT " <> a)
              }
            }
          }
        _ -> Error("wire " <> wire <> " not found")
      }
    }
  }
}

pub fn parse_input(input: String) -> Result(List(Connection), String) {
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
            Ok(connections) -> run(connections)
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
