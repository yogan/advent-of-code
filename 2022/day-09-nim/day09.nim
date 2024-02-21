import std/sets, std/strutils, std/strformat, std/sequtils

type
  Direction = enum
    left, right, up, down, none

  Command = object
    direction: Direction
    steps: int

  Position = tuple[x: int, y: int]

  Positions = tuple
    head: Position
    tail: Position

proc printField(knots: seq[Position], height: int, width: int) =
  let emptyLine = toSeq(0..width).mapIt(".")
  var lines = toSeq(0..height).mapIt(emptyLine)

  for idx, knot in knots:
    let (col, row) = knot
    try:
      lines[height - row][col] = if idx == 0: "H" else: $idx
    except IndexDefect:
      discard

  for line in lines:
    echo line.join("")
  echo ""

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
  else: discard

  var delta_x = result.head.x - result.tail.x
  var delta_y = result.head.y - result.tail.y
  assert abs(delta_x) + abs(delta_y) <= 4

  # horizontal movement
  if (delta_x == 0):
    if (delta_y == 2):
      inc(result.tail.y)
      return
    elif (delta_y == -2):
      dec(result.tail.y)
      return
  elif (delta_y == 0):
    if (delta_x == 2):
      inc(result.tail.x)
      return
    elif (delta_x == -2):
      dec(result.tail.x)
      return

  # diagonal movement
  if (delta_x == -2 and delta_y == -1) or
     (delta_x == -1 and delta_y == -2) or
     (delta_x == -2 and delta_y == -2):
    dec(result.tail.x)
    dec(result.tail.y)
  elif (delta_x == -2 and delta_y == 1) or
       (delta_x == -1 and delta_y == 2) or
       (delta_x == -2 and delta_y == 2):
    dec(result.tail.x)
    inc(result.tail.y)
  elif (delta_x == 1 and delta_y == -2) or
       (delta_x == 2 and delta_y == -1) or
       (delta_x == 2 and delta_y == -2):
    inc(result.tail.x)
    dec(result.tail.y)
  elif (delta_x == 1 and delta_y == 2) or
       (delta_x == 2 and delta_y == 1) or
       (delta_x == 2 and delta_y == 2):
    inc(result.tail.x)
    inc(result.tail.y)

proc simulateRope(commands: seq[Command], numKnots: int):
    (HashSet[Position], seq[Position]) =
  var knots: seq[Position] = @[]
  for i in 0..<numKnots:
    knots.add((0, 0))

  var tailPositions = initHashSet[Position]()
  tailPositions.incl((0, 0))

  for command in commands:
    for s in 0 ..< command.steps:
      for i in 0..<numKnots - 1:
        var positions = (head: knots[i], tail: knots[i + 1])
        if i == 0:
          positions = step(positions, command.direction)
        else:
          positions = step(positions, none)
        knots[i] = positions.head
        knots[i + 1] = positions.tail
        # printField(knots, 6, 8)
      tailPositions.incl(knots[numKnots - 1])

  result = (tailPositions, knots)

proc part1(filename: string): int =
  let commands = parseInput(filename)
  let (tailPositions, _) = simulateRope(commands, 2)
  result = tailPositions.len

proc part2(filename: string): int =
  let commands = parseInput(filename)
  let (tailPositions, _) = simulateRope(commands, 10)
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

    check(step(
      (head: (5, 1), tail: (4, 0)), up) ==
      (head: (5, 2), tail: (5, 1)))

  test "step (no movement, tail follows diagonally - easy)":
    let head = (0, 0)

    check(step(
      (head: head, tail: (-2, -2)), none) ==
      (head: head, tail: (-1, -1)))

    check(step(
      (head: head, tail: (-2, 2)), none) ==
      (head: head, tail: (-1, 1)))

    check(step(
      (head: head, tail: (2, 2)), none) ==
      (head: head, tail: (1, 1)))

    check(step(
      (head: head, tail: (2, -2)), none) ==
      (head: head, tail: (1, -1)))

  test "step (no movement, tail follows diagonally - hard)":
    let head = (0, 0)

    check(step(
      (head: head, tail: (2, 1)), none) ==
      (head: head, tail: (1, 0)))

    check(step(
      (head: head, tail: (2, -1)), none) ==
      (head: head, tail: (1, 0)))

    check(step(
      (head: head, tail: (1, 2)), none) ==
      (head: head, tail: (0, 1)))

    check(step(
      (head: head, tail: (1, -2)), none) ==
      (head: head, tail: (0, -1)))

    check(step(
      (head: head, tail: (-1, 2)), none) ==
      (head: head, tail: (0, 1)))

    check(step(
      (head: head, tail: (-1, -2)), none) ==
      (head: head, tail: (0, -1)))

    check(step(
      (head: head, tail: (-2, 1)), none) ==
      (head: head, tail: (-1, 0)))

    check(step(
      (head: head, tail: (-2, -1)), none) ==
      (head: head, tail: (-1, 0)))

  test "simulateRope (first two commands)":
    let commands = @[
      Command(direction: right, steps: 5),
      Command(direction: up, steps: 8),
    ]

    let (tailPositions, knots) = simulateRope(commands, 10)

    check(tailPositions == toHashSet([(0, 0)]))
    check(knots == @[
      #H       1       2       3       4
      (5, 8), (5, 7), (5, 6), (5, 5), (5, 4),
      #5       6      7       8       9
      (4, 4), (3, 3), (2, 2), (1, 1), (0, 0)
    ])

  test "simulateRope (full larger sample)":
    let commands = @[
      Command(direction: right, steps: 5),
      Command(direction: up, steps: 8),
      Command(direction: left, steps: 8),
      Command(direction: down, steps: 3),
      Command(direction: right, steps: 17),
      Command(direction: down, steps: 10),
      Command(direction: left, steps: 25),
      Command(direction: up, steps: 20),
    ]

    let (tailPositions, knots) = simulateRope(commands, 10)

    check(tailPositions.len == 36)
    check(knots == @[
      #H          1          2          3          4
      (-11, 15), (-11, 14), (-11, 13), (-11, 12), (-11, 11),
      #6          7         8         9         10
      (-11, 10), (-11, 9), (-11, 8), (-11, 7), (-11, 6),
    ])

  test "part1":
    check part1("day09.sample") == 13

  test "part2":
    check part2("day09.sample") == 1

#endregion Tests

let filename = "day09.in"
echo &"Part 1: {part1(filename)}"
echo &"Part 2: {part2(filename)}"
