import std/[sequtils, sets, strutils]

type Position = tuple[x, y: int]

type
  Robot = object
    p: Position
    v: Position

proc parseInput*(data: string): seq[Robot] =
  result = data
    .splitLines()
    .filterIt(it != "")
    .mapIt(it.split(" ").mapIt(it[2..^1].split(",")))
    .mapIt(Robot(p: (it[0][0].parseInt, it[0][1].parseInt),
                 v: (it[1][0].parseInt, it[1][1].parseInt)))

proc `%` (a, n: int): int =
  result = a mod n
  if result < 0: result += n

proc simulate(robots: seq[Robot], seconds, max_x, max_y: int): seq[Position] =
  for robot in robots:
    let x = robot.p.x + robot.v.x * seconds
    let y = robot.p.y + robot.v.y * seconds
    result.add((x % max_x, y % max_y))

proc count_quadrants(positions: seq[Position], max_x, max_y: int): seq[int] =
  result = newSeq[int](4)
  var (mid_x, mid_y) = (max_x div 2, max_y div 2)

  for (x, y) in positions:
    if 0+x < mid_x and y < mid_y: inc result[0]
    elif x > mid_x and y < mid_y: inc result[1]
    elif x < mid_x and y > mid_y: inc result[2]
    elif x > mid_x and y > mid_y: inc result[3]

const (max_x, max_y) = (101, 103)

proc part1*(robots: seq[Robot]): int =
  var positions = simulate(robots, 100, max_x, max_y)
  result = count_quadrants(positions, max_x, max_y).foldl(a * b)

proc part2*(robots: seq[Robot]): int =
  var max_unique_positions = 0
  var easter_egg_second: int

  for second in 1..max_x * max_y:
    let unique_positions = toHashSet(simulate(robots, second, max_x, max_y))
    if unique_positions.len > max_unique_positions:
      max_unique_positions = unique_positions.len
      easter_egg_second = second

  result = easter_egg_second
