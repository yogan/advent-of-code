import sys
import unittest


def parse(filename):
    return [[int(c) for c in list(line.strip())] for line in open(filename).readlines()]


def part1(grid):
    return sum((score(grid, head) for head in find_trailheads(grid)))


def find_trailheads(grid):
    trailheads = []
    for r, row in enumerate(grid):
        for c, v in enumerate(row):
            if v == 0:
                trailheads.append((r, c))
    return trailheads


def score(grid, head):
    rows, cols = len(grid), len(grid[0])
    positions = {head}
    height = 0

    while positions and height < 9:
        next_positions = set()
        for pos in positions:
            for n in neighbors(pos, rows, cols):
                if grid[n[0]][n[1]] == height + 1:
                    next_positions.add(n)
        positions = next_positions
        height = height + 1

    return len(positions)


def neighbors(pos, rows, cols):
    r, c = pos
    n = set()
    deltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for d in deltas:
        rr, cc = r + d[0], c + d[1]
        if 0 <= rr < rows and 0 <= cc < cols:
            n.add((rr, cc))
    return n


class Tests(unittest.TestCase):
    sample = [
        [8, 9, 0, 1, 0, 1, 2, 3],
        [7, 8, 1, 2, 1, 8, 7, 4],
        [8, 7, 4, 3, 0, 9, 6, 5],
        [9, 6, 5, 4, 9, 8, 7, 4],
        [4, 5, 6, 7, 8, 9, 0, 3],
        [3, 2, 0, 1, 9, 0, 1, 2],
        [0, 1, 3, 2, 9, 8, 0, 1],
        [1, 0, 4, 5, 6, 7, 3, 2],
    ]

    def test_find_trailheads(self):
        self.assertEqual(
            find_trailheads(self.sample),
            [
                (0, 2),
                (0, 4),
                (2, 4),
                (4, 6),
                (5, 2),
                (5, 5),
                (6, 0),
                (6, 6),
                (7, 1),
            ],
        )

    def test_score(self):
        self.assertEqual(score(self.sample, (0, 2)), 5)
        self.assertEqual(score(self.sample, (0, 4)), 6)
        self.assertEqual(score(self.sample, (2, 4)), 5)
        self.assertEqual(score(self.sample, (4, 6)), 3)
        self.assertEqual(score(self.sample, (5, 2)), 1)
        self.assertEqual(score(self.sample, (5, 5)), 3)
        self.assertEqual(score(self.sample, (6, 0)), 5)
        self.assertEqual(score(self.sample, (6, 6)), 3)
        self.assertEqual(score(self.sample, (7, 1)), 5)

    def test_neighbors(self):
        R, C = 8, 8
        self.assertEqual(neighbors((0, 0), R, C), {(0, 1), (1, 0)})
        self.assertEqual(neighbors((0, 1), R, C), {(0, 0), (0, 2), (1, 1)})
        self.assertEqual(neighbors((0, 7), R, C), {(0, 6), (1, 7)})
        self.assertEqual(neighbors((1, 0), R, C), {(0, 0), (2, 0), (1, 1)})
        self.assertEqual(neighbors((1, 1), R, C), {(0, 1), (2, 1), (1, 0), (1, 2)})
        self.assertEqual(neighbors((7, 6), R, C), {(7, 5), (7, 7), (6, 6)})
        self.assertEqual(neighbors((7, 7), R, C), {(6, 7), (7, 6)})


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(exit=True)

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    grid = parse(filename)
    p1 = part1(grid)
    p2 = None

    check(1, p1, 36 if is_sample else 698)
    check(2, p2)
