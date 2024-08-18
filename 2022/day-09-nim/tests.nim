import unittest
import std/sets, std/strutils, std/sequtils
include lib

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
