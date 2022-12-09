import std/strutils, std/strformat, std/sequtils

type Direction = enum
  left, right, up, down

type
  Command = object
    direction: Direction
    steps: int

type
  Positions = tuple
    head_x: int
    head_y: int
    tail_x: int
    tail_y: int

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

proc step(positions: Positions, direction: Direction): Positions =
  result = positions

  case direction
  of left: dec(result.head_x)
  of right: inc(result.head_x)
  of up: inc(result.head_y)
  of down: dec(result.head_y)

  var delta_x = result.head_x - result.tail_x
  var delta_y = result.head_y - result.tail_y
  assert abs(delta_x) + abs(delta_y) < 4

  if delta_x == 2:
    inc(result.tail_x)
  elif delta_x == -2:
    dec(result.tail_x)
  elif delta_y == 2:
    inc(result.tail_y)
  elif delta_y == -2:
    dec(result.tail_y)

  var diagonal = (
    (abs(delta_x) == 2 and abs(delta_y) == 1) or
    (abs(delta_x) == 1 and abs(delta_y) == 2)
  )
  if not diagonal:
    return

  if (delta_x == 1):
    inc(result.tail_x)
  if (delta_x == -1):
    dec(result.tail_x)
  if (delta_y == 1):
    inc(result.tail_y)
  if (delta_y == -1):
    dec(result.tail_y)

proc simulateRope(commands: seq[Command]) =
  var positions = (head_x: 0, head_y: 0,
                   tail_x: 0, tail_y: 0)

  for command in commands:
    for _ in 0 ..< command.steps:
      positions = step(positions, command.direction)

proc part1: int =
  let commands = parseInput("day09.sample")
  # let commands = parseInput("day09.in")
  simulateRope(commands)

  result = 999999999

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

  test "step (tail stays)":
    check(step(
      (head_x: 0, head_y: 0,
       tail_x: 0, tail_y: 0), right) ==
      (head_x: 1, head_y: 0,
       tail_x: 0, tail_y: 0))

    check(step(
      (head_x: 2, head_y: 1,
       tail_x: 1, tail_y: 1), left) ==
      (head_x: 1, head_y: 1,
       tail_x: 1, tail_y: 1))

  test "step (tail follows horizontally)":
    check(step(
      (head_x: 2, head_y: 1,
       tail_x: 1, tail_y: 1), right) ==
      (head_x: 3, head_y: 1,
       tail_x: 2, tail_y: 1))

    check(step(
      (head_x: 2, head_y: 1,
       tail_x: 3, tail_y: 1), left) ==
      (head_x: 1, head_y: 1,
       tail_x: 2, tail_y: 1))

  test "step (tail follows vertically)":
    check(step(
      (head_x: 1, head_y: 2,
       tail_x: 1, tail_y: 3), down) ==
      (head_x: 1, head_y: 1,
       tail_x: 1, tail_y: 2))

    check(step(
      (head_x: 1, head_y: 2,
       tail_x: 1, tail_y: 1), up) ==
      (head_x: 1, head_y: 3,
       tail_x: 1, tail_y: 2))

  test "step (tail follows diagonally)":
    check(step(
      (head_x: 2, head_y: 2,
       tail_x: 1, tail_y: 1), up) ==
      (head_x: 2, head_y: 3,
       tail_x: 2, tail_y: 2))

    check(step(
      (head_x: 2, head_y: 2,
       tail_x: 1, tail_y: 1), right) ==
      (head_x: 3, head_y: 2,
       tail_x: 2, tail_y: 2))

#endregion Tests

echo &"Part 1: {part1()}"
