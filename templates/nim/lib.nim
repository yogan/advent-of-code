import std/[strutils, sequtils]

type
  Box = object
    l: int
    w: int
    h: int

proc parseInput*(data: string): seq[Box] =
  result = data
    .splitLines()
    .filterIt(it != "")
    .mapIt(it.split("x"))
    .mapIt(it.mapIt(it.parseInt))
    .mapIt(Box(l: it[0], w: it[1], h: it[2]))

proc volume(box: Box): int =
  result = box.l * box.w * box.h

proc surfaceArea(box: Box): int =
  let (l, w, h) = (box.l, box.w, box.h)
  result = 2 * (l * w + w * h + h * l)

proc part1*(boxes: seq[Box]): int =
  result = boxes.mapIt(volume(it)).foldl(a + b)

proc part2*(boxes: seq[Box]): int =
  result = boxes.mapIt(surfaceArea(it)).foldl(a + b)
