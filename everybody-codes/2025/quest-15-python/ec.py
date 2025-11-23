import sys
import unittest
from collections import deque

NORTH, EAST, SOUTH, WEST = (1, 0), (0, 1), (-1, 0), (0, -1)


def parse(filename):
    return [(i[0], int(i[1:])) for i in open(filename).read().strip().split(",")]


def part1(instructions):
    walls, end = build_maze(instructions)
    steps = 0
    seen = set()
    border = set([(0, 0)])

    while True:
        if end in border:
            return steps

        seen |= border
        steps += 1

        border = set(
            (nr, nc)
            for r, c in border
            for nr, nc in neighbors(r, c, walls)
            if (nr, nc) not in seen
        )

        # print_maze(walls, next_border, seen, steps, end)


def build_maze(instructions):
    r, c = 0, 0
    dr, dc = NORTH
    walls = set()

    for turn, dist in instructions:
        dr, dc = direction((dr, dc), turn)
        for _ in range(dist):
            r += dr
            c += dc
            walls.add((r, c))

    end = (r, c)
    walls.remove(end)
    return walls, end


def neighbors(r, c, walls):
    return {(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)} - walls


def direction(cur, turn):
    if cur == NORTH:
        return EAST if turn == "R" else WEST
    if cur == EAST:
        return SOUTH if turn == "R" else NORTH
    if cur == SOUTH:
        return WEST if turn == "R" else EAST
    if cur == WEST:
        return NORTH if turn == "R" else SOUTH
    raise (ValueError(f"direction({cur=}, {turn=})"))


def print_maze(walls, border, seen, steps, end):
    min_r = min(r for r, _ in walls)
    min_c = min(c for _, c in walls)
    max_r = max(r for r, _ in walls)
    max_c = max(c for _, c in walls)

    print(f"\n{steps=}")
    for r in range(max_r, min_r - 1, -1):
        for c in range(min_c, max_c + 1):
            if (r, c) == (0, 0):
                print("S", end="")
            elif (r, c) == end:
                print("E", end="")
            elif (r, c) in walls:
                print("#", end="")
            elif (r, c) in border:
                print("x", end="")
            elif (r, c) in seen:
                print(".", end="")
            else:
                print(" ", end="")
        print()


class Tests(unittest.TestCase):
    def test_build_maze(self):
        instr = [("R", 3), ("R", 4), ("L", 3), ("L", 4), ("R", 3), ("R", 6), ("R", 9)]

        w1 = [(0, 1), (0, 2), (0, 3)]
        w2 = w1 + [(-1, 3), (-2, 3), (-3, 3), (-4, 3)]
        w3 = w2 + [(-4, 4), (-4, 5), (-4, 6)]
        w4 = w3 + [(-3, 6), (-2, 6), (-1, 6), (0, 6)]
        w5 = w4 + [(0, 7), (0, 8), (0, 9)]
        w6 = w5 + [(-1, 9), (-2, 9), (-3, 9), (-4, 9), (-5, 9), (-6, 9)]
        w7 = w6 + [(-6, c) for c in range(8, -1, -1)]

        self.assertEqual(build_maze(instr[:1]), (set(w1[:-1]), w1[-1]))
        self.assertEqual(build_maze(instr[:2]), (set(w2[:-1]), w2[-1]))
        self.assertEqual(build_maze(instr[:3]), (set(w3[:-1]), w3[-1]))
        self.assertEqual(build_maze(instr[:4]), (set(w4[:-1]), w4[-1]))
        self.assertEqual(build_maze(instr[:5]), (set(w5[:-1]), w5[-1]))
        self.assertEqual(build_maze(instr[:6]), (set(w6[:-1]), w6[-1]))
        self.assertEqual(build_maze(instr[:7]), (set(w7[:-1]), w7[-1]))


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 16)
    else:
        failures += check(1, part1(parse("input1.txt")), 101)

    exit(failures)


def check(part, actual, expected=None):
    failure = 0

    if expected is None:
        symbol = "🤔"
        result = f"{actual}"
    elif actual == expected:
        symbol = "✅"
        result = f"{actual}"
    else:
        symbol = "❌"
        result = f"{actual} ≠ {expected}"
        failure = 1

    print(f"{symbol} Part {part}{' (sample)' if is_sample else ''}: {result}")
    return failure


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    main()
