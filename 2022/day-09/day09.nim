import std/strutils, std/strformat, std/sequtils

type Direction = enum
  left, right, up, down

type
  Command = object
    direction: Direction
    steps: int

proc parseInput(filename: string): seq[Command] =
  result = readFile(filename)
    .splitLines()
    .filterIt(it != "")
    .mapIt(it.split(" "))
    .mapIt(Command(
      direction: case it[0]
      of "L": left
      of "R": right
      of "U": up
      of "D": down
      else: raise newException(ValueError,
                               "Invalid direction: " & it[0]),
      steps: parseInt(it[1])))

proc part1: int =
  # let input = parseInput("day09.sample")
  let input = parseInput("day09.in")
  # echo input

  result = input.len

#region Tests

import unittest

suite "Advent of Code 2022 Day 09":

  test "parseInput":
    # day09.sample:
    #  "R 4", "U 4", "L 3", "D 1",
    #  "R 4", "D 1", "L 5", "R 2"
    check(parseInput("day09.sample") == @[
      Command(direction: right, steps: 4),
      Command(direction: up, steps: 4),
      Command(direction: left, steps: 3),
      Command(direction: down, steps: 1),
      Command(direction: right, steps: 4),
      Command(direction: down, steps: 1),
      Command(direction: left, steps: 5),
      Command(direction: right, steps: 2)
    ])

#endregion Tests

echo &"Part 1: {part1()}"
