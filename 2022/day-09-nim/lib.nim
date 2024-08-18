import std/sets, std/strutils, std/sequtils

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

proc part1*(filename: string): int =
  let commands = parseInput(filename)
  let (tailPositions, _) = simulateRope(commands, 2)
  result = tailPositions.len

proc part2*(filename: string): int =
  let commands = parseInput(filename)
  let (tailPositions, _) = simulateRope(commands, 10)
  result = tailPositions.len
