import std/strutils

proc nextRow*(row: string): string =
  var next = ""
  var extended = "." & row & "."

  for i in 0 ..< row.len:
    let above = extended.substr(i, i + 2)
    if above == "^^." or above == ".^^" or above == "^.." or above == "..^":
      next &= "^"
    else:
      next &= "."

  result = next


proc safeTiles*(firstRow: string, rows: int): int =
  var sum = 0
  var row = firstRow

  for i in 0 ..< rows:
    sum += row.count('.')
    row = nextRow(row)

  result = sum
