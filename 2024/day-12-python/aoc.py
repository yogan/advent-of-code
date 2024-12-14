import sys
import unittest
from collections import deque


def parse(filename):
    return [line.strip() for line in open(filename).readlines()]


def part1(regions):
    return sum(a * p for a, p, _ in regions)


def part2(regions):
    return sum(a * s for a, _, s in regions)


def find_regions(lines):
    rows, cols = len(lines), len(lines[0])
    seen = set()
    regions = []
    for r in range(rows):
        for c in range(cols):
            if (r, c) in seen:
                continue
            region = flood_fill(lines, r, c, rows, cols, seen)
            sides = perimeter(region, lines, r, c, rows, cols)
            regions.append((len(region), len(sides), count_sides(sides)))
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
    sides = set()
    for r, c in region:
        seen.add((r, c))
        for rr, cc, side in [
            (r - 1, c, ("T", r, c)),
            (r + 1, c, ("B", r + 1, c)),
            (r, c - 1, ("L", r, c)),
            (r, c + 1, ("R", r, c + 1)),
        ]:
            if rr < 0 or rr >= rows or cc < 0 or cc >= cols or lines[rr][cc] != char:
                sides.add(side)
    return sides


def count_sides(sides):
    res = 0
    seen = set()

    for dir, r, c in sides:
        if (dir, r, c) in seen:
            continue

        res += 1
        seen.add((dir, r, c))

        if dir == "T" or dir == "B":
            # add all directly left
            cc = c - 1
            while (dir, r, cc) in sides and (dir, r, cc) not in seen:
                seen.add((dir, r, cc))
                cc -= 1
            # add all directly right
            cc = c + 1
            while (dir, r, cc) in sides and (dir, r, cc) not in seen:
                seen.add((dir, r, cc))
                cc += 1
        else:
            # add all directly above
            rr = r - 1
            while (dir, rr, c) in sides and (dir, rr, c) not in seen:
                seen.add((dir, rr, c))
                rr -= 1
            # add all directly below
            rr = r + 1
            while (dir, rr, c) in sides and (dir, rr, c) not in seen:
                seen.add((dir, rr, c))
                rr += 1

    return res


class Tests(unittest.TestCase):
    sample1 = [
        "AAAA",
        "BBCD",
        "BBCC",
        "EEEC",
    ]

    sample2 = [
        "RRRRIICCFF",
        "RRRRIICCCF",
        "VVRRRCCFFF",
        "VVRCCCJFFF",
        "VVVVCJJCFE",
        "VVIVCCJJEE",
        "VVIIICJJEE",
        "MIIIIIJJEE",
        "MIIISIJEEE",
        "MMMISSJEEE",
    ]

    def test_find_regions_sample1(self):
        self.assertEqual(
            find_regions(self.sample1),
            [
                (4, 10, 4),  # A
                (4, +8, 4),  # B
                (4, 10, 8),  # C
                (1, +4, 4),  # D
                (3, +8, 4),  # E
            ],
        )

    def test_find_regions_sample2(self):
        self.assertEqual(
            find_regions(self.sample2),
            [
                (12, 18, 10),  # R
                (+4, +8, +4),  # I
                (14, 28, 22),  # C
                (10, 18, 12),  # F
                (13, 20, 10),  # V
                (11, 20, 12),  # J
                (+1, +4, +4),  # C
                (13, 18, +8),  # E
                (14, 22, 16),  # I
                (+5, 12, +6),  # M
                (+3, +8, +6),  # S
            ],
        )

    def test_find_regions_sample_e(self):
        self.assertEqual(
            find_regions(
                [
                    "EEEEE",
                    "EXXXX",
                    "EEEEE",
                    "EXXXX",
                    "EEEEE",
                ]
            ),
            [
                (17, 36, 12),  # E
                (+4, 10, +4),  # X top
                (+4, 10, +4),  # X bottom
            ],
        )

    def test_find_regions_sample_ab(self):
        self.assertEqual(
            find_regions(
                [
                    "AAAAAA",
                    "AAABBA",
                    "AAABBA",
                    "ABBAAA",
                    "ABBAAA",
                    "AAAAAA",
                ]
            ),
            [
                (28, 40, 12),  # A
                (+4, +8, +4),  # B top
                (+4, +8, +4),  # B bottom
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
            {
                # above
                ("T", 0, 0),
                ("T", 0, 1),
                ("T", 0, 2),
                ("T", 0, 3),
                # below
                ("B", 1, 0),
                ("B", 1, 1),
                ("B", 1, 2),
                ("B", 1, 3),
                # left
                ("L", 0, 0),
                # right
                ("R", 0, 4),
            },
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

    regions = find_regions(parse(filename))
    p1 = part1(regions)
    p2 = part2(regions)

    check(1, p1, 1930 if is_sample else 1361494)
    check(2, p2, 1206 if is_sample else 830516)
