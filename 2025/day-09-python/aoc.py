import sys
import unittest


def solve(coords):
    p1, p2 = 0, 0
    E = edges(coords)

    for i, (c1, r1) in enumerate(coords):
        for c2, r2 in coords[i:]:
            area = (abs(c2 - c1) + 1) * (abs(r2 - r1) + 1)
            p1 = max(p1, area)

            if area < p2:
                continue

            rectangle = (min(c1, c2), max(c1, c2), min(r1, r2), max(r1, r2))
            if any(inside(e1, e2, rectangle) for e1, e2 in E):
                continue

            p2 = area

    return p1, p2


def edges(coords):
    return [sorted([l, r]) for l, r in zip(coords, coords[1:] + [coords[0]])]


def inside(p1, p2, rectangle):
    (c1, r1), (c2, r2) = p1, p2
    c_min, c_max, r_min, r_max = rectangle

    if r1 == r2:
        return r_min < r1 < r_max and c1 < c_max and c_min < c2
    elif c1 == c2:
        return c_min < c1 < c_max and r1 < r_max and r_min < r2


def parse():
    return [
        tuple(map(int, line.strip().split(","))) for line in open(filename).readlines()
    ]


class Tests(unittest.TestCase):
    def test_edges(self):
        self.assertEqual(
            edges([(7, 1), (11, 1), (11, 7), (9, 7), (9, 5), (2, 5), (2, 3), (7, 3)]),
            [
                [(7, 1), (11, 1)],
                [(11, 1), (11, 7)],
                [(9, 7), (11, 7)],
                [(9, 5), (9, 7)],
                [(2, 5), (9, 5)],
                [(2, 3), (2, 5)],
                [(2, 3), (7, 3)],
                [(7, 1), (7, 3)],
            ],
        )

    def test_inside(self):
        rectangle = (2, 9, 3, 7)
        self.assertTrue(inside((2, 5), (9, 5), rectangle))
        self.assertTrue(inside((8, 5), (9, 5), rectangle))
        self.assertFalse(inside((9, 5), (9, 5), rectangle))
        self.assertFalse(inside((2, 3), (7, 3), rectangle))


def main():
    p1, p2 = solve(parse())

    failures = 0
    failures += check(1, p1, 50 if is_sample else 4746238001)
    failures += check(2, p2, 24 if is_sample else 1552139370)
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
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    is_sample = "-s" in flags or "--sample" in flags
    run_tests = "-t" in flags or "--test" in flags

    filename = "sample.txt" if is_sample else "input.txt"
    filename = args[0] if args else filename

    if run_tests:
        unittest.main(argv=sys.argv[:1])

    main()
