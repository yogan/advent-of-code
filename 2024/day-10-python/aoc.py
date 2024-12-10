import sys
import unittest


def parse(filename):
    return [[int(c) for c in list(line.strip())] for line in open(filename).readlines()]


def trailheads(grid):
    heads = []
    for r, row in enumerate(grid):
        for c, v in enumerate(row):
            if v == 0:
                heads.append((r, c))
    return heads


def score_and_rating(grid, head):
    rows, cols = len(grid), len(grid[0])
    paths = [[head]]

    for height in range(9):
        next_paths = []
        for path in paths:
            for r, c in neighbors(path[-1], rows, cols):
                if grid[r][c] == height + 1:
                    next_paths.append(path + [(r, c)])
        paths = next_paths

    peaks = set(p[-1] for p in paths)

    return len(peaks), len(paths)


def neighbors(pos, rows, cols):
    r, c = pos
    n = set()
    for dr, dc in ((-1, 0), (1, 0), (0, -1), (0, 1)):
        rr, cc = r + dr, c + dc
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

    def test_trailheads(self):
        self.assertEqual(
            trailheads(self.sample),
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

    def test_score_and_rating(self):
        self.assertEqual(score_and_rating(self.sample, (0, 2)), (5, 20))
        self.assertEqual(score_and_rating(self.sample, (0, 4)), (6, 24))
        self.assertEqual(score_and_rating(self.sample, (2, 4)), (5, 10))
        self.assertEqual(score_and_rating(self.sample, (4, 6)), (3, 4))
        self.assertEqual(score_and_rating(self.sample, (5, 2)), (1, 1))
        self.assertEqual(score_and_rating(self.sample, (5, 5)), (3, 4))
        self.assertEqual(score_and_rating(self.sample, (6, 0)), (5, 5))
        self.assertEqual(score_and_rating(self.sample, (6, 6)), (3, 8))
        self.assertEqual(score_and_rating(self.sample, (7, 1)), (5, 5))

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
    scores_and_ratings = [score_and_rating(grid, head) for head in trailheads(grid)]

    p1 = sum(score for score, _ in scores_and_ratings)
    p2 = sum(rating for _, rating in scores_and_ratings)

    check(1, p1, 36 if is_sample else 698)
    check(2, p2, 81 if is_sample else 1436)
