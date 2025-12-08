import unittest
import aoc

suite "Advent of Code in Nim":

  test "parseInput works for sample data":
    check(parseInput("""
1x2x3
987x10x1
""") == @[
      Box(l: 1, w: 2, h: 3),
      Box(l: 987, w: 10, h: 1),
    ])

  test "part1 returns the sum of the volumes of the boxes":
    const boxes = @[Box(l: 1, w: 2, h: 3), Box(l: 1, w: 1, h: 1)]
    check part1(boxes) == 7

