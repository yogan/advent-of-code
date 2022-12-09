import std/sets, std/strutils, std/strformat, std/sequtils

type
  Direction = enum
    left, right, up, down

  Command = object
    direction: Direction
    steps: int

  Position = tuple[x: int, y: int]

  Positions = tuple
    head: Position
    tail: Position

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
  of left: dec(result.head.x)
  of right: inc(result.head.x)
  of up: inc(result.head.y)
  of down: dec(result.head.y)

  var delta_x = result.head.x - result.tail.x
  var delta_y = result.head.y - result.tail.y
  assert abs(delta_x) + abs(delta_y) < 4

  if delta_x == 2:
    inc(result.tail.x)
  elif delta_x == -2:
    dec(result.tail.x)
  elif delta_y == 2:
    inc(result.tail.y)
  elif delta_y == -2:
    dec(result.tail.y)

  var diagonal = (
    (abs(delta_x) == 2 and abs(delta_y) == 1) or
    (abs(delta_x) == 1 and abs(delta_y) == 2)
  )
  if not diagonal:
    return

  if (delta_x == 1):
    inc(result.tail.x)
  if (delta_x == -1):
    dec(result.tail.x)
  if (delta_y == 1):
    inc(result.tail.y)
  if (delta_y == -1):
    dec(result.tail.y)

proc simulateRope(commands: seq[Command]): HashSet[Position] =
  result = initHashSet[Position]()
  result.incl((0, 0))

  var positions = (head: (0, 0), tail: (0, 0))

  for command in commands:
    for _ in 0 ..< command.steps:
      positions = step(positions, command.direction)
      result.incl(positions.tail)

proc part1(filename: string): int =
  let commands = parseInput(filename)
  let tailPositions = simulateRope(commands)
  result = tailPositions.len

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
      (head: (0, 0), tail: (0, 0)), right) ==
      (head: (1, 0), tail: (0, 0)))

    check(step(
      (head: (2, 1), tail: (1, 1)), left) ==
      (head: (1, 1), tail: (1, 1)))

  test "step (tail follows horizontally)":
    check(step(
      (head: (2, 1), tail: (1, 1)), right) ==
      (head: (3, 1), tail: (2, 1)))

    check(step(
      (head: (2, 1), tail: (3, 1)), left) ==
      (head: (1, 1), tail: (2, 1)))

  test "step (tail follows vertically)":
    check(step(
      (head: (1, 2), tail: (1, 3)), down) ==
      (head: (1, 1), tail: (1, 2)))

    check(step(
      (head: (1, 2), tail: (1, 1)), up) ==
      (head: (1, 3), tail: (1, 2)))

  test "step (tail follows diagonally)":
    check(step(
      (head: (2, 2), tail: (1, 1)), up) ==
      (head: (2, 3), tail: (2, 2)))

    check(step(
      (head: (2, 2), tail: (1, 1)), right) ==
      (head: (3, 2), tail: (2, 2)))

  test "part1":
    check part1("day09.sample") == 13

#endregion Tests

let tailPositions = part1("day09.in")
echo &"Part 1: {tailPositions}"
