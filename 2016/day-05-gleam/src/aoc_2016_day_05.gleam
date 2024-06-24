import argv
import gleam/crypto
import gleam/int
import gleam/io
import gleam/string
import gleam/result
import simplifile

pub fn main() {
  case argv.load().arguments {
    [filename] -> run(filename)
    _ -> io.println("Usage: gleam run FILENAME")
  }
}

fn run(filename: String) {
  let word =
    simplifile.read(from: filename)
    |> result.map(string.trim)
    |> result.unwrap("")

  crack(word, 0, "", 8)
  |> string.lowercase
  |> io.println
}

pub fn crack(door: String, id: Int, pw: String, len: Int) -> String {
  case string.length(pw) {
    l if l== len -> pw
    _ -> {
      let bytes = <<string.append(door, int.to_string(id)):utf8>>
      let hash = crypto.hash(crypto.Md5, bytes)

      // check for 5 leaded HEX zeroes - a hex digit is 4 bits, so 20 bits
      let assert <<header:20, sixth:4, _:bits>> = hash

      case header {
        0 -> crack(door, id + 1, pw <> int.to_base16(sixth), len)
        _ -> crack(door, id + 1, pw, len)
      }
    }
  }
}
