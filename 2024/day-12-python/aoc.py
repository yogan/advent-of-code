import sys
import unittest
from collections import deque


def parse(filename):
    return [line.strip() for line in open(filename).readlines()]


def part1(lines):
    return sum(a * p for a, p in regions(lines))


def regions(lines):
    rows, cols = len(lines), len(lines[0])
    seen = set()
    regions = []
    for r in range(rows):
        for c in range(cols):
            if (r, c) in seen:
                continue
            region = flood_fill(lines, r, c, rows, cols, seen)
            a = len(region)
            p = perimeter(region, lines, r, c, rows, cols)
            regions.append((a, p))
    return regions


def flood_fill(lines, r, c, rows, cols, seen):
    char = lines[r][c]
    region = set()
    border = deque([(r, c)])
    while border:
        r, c = border.popleft()
        if (
            r < 0
            or r >= rows
            or c < 0
            or c >= cols
            or (r, c) in seen
            or lines[r][c] != char
        ):
            continue
        seen.add((r, c))
        region.add((r, c))
        border.extend([(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)])
    return region


def perimeter(region, lines, r, c, rows, cols):
    char = lines[r][c]
    seen = set()
    p = 0
    for r, c in region:
        seen.add((r, c))
        for rr, cc in [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]:
            if rr < 0 or rr >= rows or cc < 0 or cc >= cols or lines[rr][cc] != char:
                p += 1
    return p


class Tests(unittest.TestCase):
    sample1 = [
        "AAAA",
        "BBCD",
        "BBCC",
        "EEEC",
    ]

    def test_regions(self):
        self.assertEqual(
            regions(self.sample1),
            [
                (4, 10),  # A
                (4, +8),  # B
                (4, 10),  # C
                (1, +4),  # D
                (3, +8),  # E
            ],
        )

    def test_flood_fill(self):
        rows, cols = len(self.sample1), len(self.sample1[0])
        seen = set()

        As = flood_fill(self.sample1, 0, 0, rows, cols, seen)
        self.assertEqual(As, {(0, 0), (0, 1), (0, 2), (0, 3)})
        self.assertEqual(seen, As)

        Bs = flood_fill(self.sample1, 1, 0, rows, cols, seen)
        self.assertEqual(Bs, {(1, 0), (1, 1), (2, 0), (2, 1)})
        self.assertEqual(seen, As | Bs)

        Cs = flood_fill(self.sample1, 1, 2, rows, cols, seen)
        self.assertEqual(Cs, {(1, 2), (2, 2), (2, 3), (3, 3)})
        self.assertEqual(seen, As | Bs | Cs)

        Ds = flood_fill(self.sample1, 1, 3, rows, cols, seen)
        self.assertEqual(Ds, {(1, 3)})
        self.assertEqual(seen, As | Bs | Cs | Ds)

        Es = flood_fill(self.sample1, 3, 0, rows, cols, seen)
        self.assertEqual(Es, {(3, 0), (3, 1), (3, 2)})
        self.assertEqual(seen, As | Bs | Cs | Ds | Es)

    def test_perimeter(self):
        rows, cols = len(self.sample1), len(self.sample1[0])
        self.assertEqual(
            perimeter(
                {(0, 0), (0, 1), (0, 2), (0, 3)},
                self.sample1,
                0,
                0,
                rows,
                cols,
            ),
            10,
        )


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

    lines = parse(filename)
    p1 = part1(lines)
    p2 = None

    check(1, p1, 1930 if is_sample else 1361494)
    check(2, p2)
