import unittest
include lib

suite "Advent of Code in Nim":
  const (max_x, max_y) = (11, 7)

  test "parseInput":
    check(parseInput("""
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
""") == @[
      Robot(p: (0, 4), v: (3, -3)),
      Robot(p: (6, 3), v: (-1, -3)),
      Robot(p: (10, 3), v: (-1, 2)),
    ])

  test "simulate":
    const robots = @[Robot(p: (2, 4), v: (2, -3))]
    check(simulate(robots, 1, max_x, max_y) == @[(4, 1)])
    check(simulate(robots, 2, max_x, max_y) == @[(6, 5)])
    check(simulate(robots, 3, max_x, max_y) == @[(8, 2)])
    check(simulate(robots, 4, max_x, max_y) == @[(10, 6)])
    check(simulate(robots, 5, max_x, max_y) == @[(1, 3)])

  test "count_quadrants":
    const positions = @[
      (3, 5), (5, 4), (9, 0), (4, 5), (1, 6), (1, 3),
      (6, 0), (2, 3), (0, 2), (6, 0), (4, 5), (6, 6),
    ]
    check(count_quadrants(positions, max_x, max_y) == @[1, 3, 4, 1])
