import argv
import gleam/crypto
import gleam/int
import gleam/io
import gleam/string
import gleam/result
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> {
      simplifile.read(from: filename)
      |> result.map(string.trim)
      |> result.unwrap("")
      |> run
    }
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

fn run(word: String) {
  crack1(word, 0, "", 8)
  |> string.lowercase
  |> io.println

  crack2(word, 0, "________", 0)
  |> string.lowercase
  |> io.println
}

pub fn crack1(door: String, id: Int, pw: String, len: Int) -> String {
  case string.length(pw) {
    l if l == len -> pw
    _ -> {
      let #(header, val, _) = md5(door, id)
      case header {
        0 -> crack1(door, id + 1, pw <> int.to_base16(val), len)
        _ -> crack1(door, id + 1, pw, len)
      }
    }
  }
}

pub fn crack2(door: String, id: Int, pw: String, found: Int) -> String {
  case found {
    8 -> pw
    _ -> {
      let #(header, pos, val) = md5(door, id)
      case #(header, pos) {
        #(0, p) if p >= 0 && p < 8 ->
          case string.slice(pw, p, 1) {
            "_" -> crack2(door, id + 1, update_password(pw, p, val), found + 1)
            _ -> crack2(door, id + 1, pw, found)
          }
        _ -> crack2(door, id + 1, pw, found)
      }
    }
  }
}

fn md5(door: String, id: Int) -> #(Int, Int, Int) {
  let bytes = <<string.append(door, int.to_string(id)):utf8>>
  let hash = crypto.hash(crypto.Md5, bytes)

  // check for 5 leaded HEX zeroes - a hex digit is 4 bits, so 20 bits
  let assert <<header:20, pos:4, val:4, _:bits>> = hash

  #(header, pos, val)
}

fn update_password(pw: String, pos: Int, val: Int) -> String {
  let l = string.slice(pw, 0, pos)
  let r = string.slice(pw, pos + 1, string.length(pw))
  let v = int.to_base16(val)
  l <> v <> r
}
