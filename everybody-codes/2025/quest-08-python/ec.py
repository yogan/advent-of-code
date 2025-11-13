import sys
import unittest


def parse(filename):
    return list(map(int, open(filename).read().strip().split(",")))


def part1(xs, nails):
    total = 0

    for a, b in zip(xs, xs[1:]):
        if abs(b - a) == nails // 2:
            total += 1

    return total


def part2(xs):
    total = 0
    lines = set()

    for a, b in zip(xs, xs[1:]):
        a, b = min(a, b), max(a, b)
        total += sum([1 for c, d in lines if intersect(a, b, c, d)])
        lines.add((a, b))

    return total


def intersect(a, b, c, d):
    # when going around the circle, the points of both lines have to alternate
    return a < c < b < d or c < a < d < b


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([1, 5, 2, 6, 8, 4, 1, 7, 3], 8), 4)

    def test_part2(self):
        self.assertEqual(part2([1, 5, 2, 6, 8, 4, 1, 7, 3, 5, 7, 8, 2]), 21)

    def test_intersect(self):
        self.assertFalse(intersect(1, 2, 2, 3))
        self.assertFalse(intersect(2, 3, 1, 2))
        self.assertFalse(intersect(1, 5, 2, 5))
        self.assertFalse(intersect(2, 5, 1, 5))
        self.assertFalse(intersect(1, 6, 2, 5))
        self.assertFalse(intersect(2, 5, 1, 6))
        self.assertTrue(intersect(1, 5, 2, 6))
        self.assertTrue(intersect(2, 6, 1, 5))


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt"), 8), 4)
        failures += check(2, part2(parse("sample2.txt")), 21)
    else:
        failures += check(1, part1(parse("input1.txt"), 32), 59)
        failures += check(2, part2(parse("input2.txt")), 2925233)

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
