import sys
import unittest


def parse(filename):
    lines = open(filename).read().splitlines()
    positions = parse_positions(lines)
    return positions, len(lines[0])


def parse_positions(lines):
    positions = {}
    for row, line in enumerate(lines):
        for col, c in enumerate(line):
            if c != ".":
                pos = (row, col)
                if c in positions:
                    positions[c].add(pos)
                else:
                    positions[c] = set([pos])
    return positions


def part1(positions, size, moves):
    curr = set(positions["D"])
    seen = set(curr)

    for _ in range(moves):
        next = move_dragon(curr, size)
        seen |= next
        curr = next

    return len(positions["S"] & seen)


def move_dragon(positions, size):
    next = set()
    for x, y in positions:
        # fmt:off
        for dx, dy in [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]:  # fmt:on
            xx, yy = x + dx, y + dy
            if 0 <= xx < size and 0 <= yy < size:
                next.add((xx, yy))
    return next


class Tests(unittest.TestCase):
    def test_parse_positions(self):
        pos = parse_positions(
            [
                ".......",
                "..X.X..",
                ".X...X.",
                "...D...",
                ".X...X.",
                "..X.X..",
                ".......",
            ]
        )
        self.assertEqual(pos["D"], set([(3, 3)]))
        # fmt:off
        self.assertEqual(pos["X"], set([
                  (1,2),(1,4),
            (2,1),            (2,5),
            (4,1),            (4,5),
                  (5,2),(5,4),
        ]))  # fmt:on

    def test_move_dragon_center(self):
        pos = parse_positions(
            [
                ".......",
                "..X.X..",
                ".X...X.",
                "...D...",
                ".X...X.",
                "..X.X..",
                ".......",
            ]
        )
        self.assertEqual(move_dragon(pos["D"], 7), pos["X"])

    def test_move_dragon_top_left(self):
        pos = parse_positions(
            [
                "...X...",
                ".D.....",
                "...X...",
                "X.X....",
                ".......",
                ".......",
                ".......",
            ]
        )
        self.assertEqual(move_dragon(pos["D"], 7), pos["X"])

    def test_move_dragon_top_right(self):
        pos = parse_positions(
            [
                "......D",
                "....X..",
                ".....X.",
                ".......",
                ".......",
                ".......",
                ".......",
            ]
        )
        self.assertEqual(move_dragon(pos["D"], 7), pos["X"])

    def test_move_dragon_bottom_center(self):
        pos = parse_positions(
            [
                ".......",
                ".......",
                ".......",
                ".......",
                "..X.X..",
                ".X...X.",
                "...D...",
            ]
        )
        self.assertEqual(move_dragon(pos["D"], 7), pos["X"])

    def test_move_dragon_right_center(self):
        pos = parse_positions(
            [
                ".......",
                "....X.X",
                "...X...",
                ".....D.",
                "...X...",
                "....X.X",
                ".......",
            ]
        )
        self.assertEqual(move_dragon(pos["D"], 7), pos["X"])


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt"), 3), 27)
    else:
        failures += check(1, part1(*parse("input1.txt"), 4), 151)

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
