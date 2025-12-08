import std/[strutils, sequtils]

type
  Box* = object
    l*: int
    w*: int
    h*: int

proc volume(box: Box): int =
  result = box.l * box.w * box.h

proc part1*(boxes: seq[Box]): int =
  result = boxes.mapIt(volume(it)).foldl(a + b)

proc parseInput*(data: string): seq[Box] =
  result = data
    .splitLines()
    .filterIt(it != "")
    .mapIt(it.split("x"))
    .mapIt(it.mapIt(it.parseInt))
    .mapIt(Box(l: it[0], w: it[1], h: it[2]))

when isMainModule:
  import std/os
  if paramCount() < 1:
    quit("Usage: ./aoc <filename>")

  let data = readFile(paramStr(1))
  let boxes = parseInput(data)
  echo part1(boxes)

