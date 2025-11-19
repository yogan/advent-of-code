import sys
import unittest


def parse(filename):
    return list(map(int, open(filename).readlines()))


def part1(xs):
    inPhase1 = True

    for _ in range(11):
        if inPhase1:
            inPhase1 = phase1(xs)
        else:
            phase2(xs)

    return checksum(xs)


def part2(xs):
    rounds = 0
    phase = 1

    while True:
        if phase == 1:
            if not phase1(xs):
                phase = 2
        else:
            if not phase2(xs):
                return rounds - 1
        rounds += 1


def phase1(xs):
    return swap(xs, 1)


def phase2(xs):
    return swap(xs, -1)


def swap(xs, dir):
    moved = False

    for i in range(len(xs) - 1):
        diff = xs[i] - xs[i + 1]
        if dir * diff > 0:
            moved = True
            xs[i] -= dir
            xs[i + 1] += dir

    return moved


def checksum(xs):
    return sum(i * num for i, num in enumerate(xs, 1))


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([]), 0)

    def test_phase1(self):
        xs = [9, 1, 1, 4, 9, 6]
        self.assertTrue(phase1(xs))
        self.assertEqual(xs, [8, 1, 2, 4, 8, 7])
        self.assertTrue(phase1(xs))
        self.assertEqual(xs, [7, 2, 2, 4, 7, 8])
        self.assertTrue(phase1(xs))
        self.assertEqual(xs, [6, 2, 3, 4, 7, 8])
        self.assertTrue(phase1(xs))
        self.assertEqual(xs, [5, 3, 3, 4, 7, 8])
        self.assertTrue(phase1(xs))
        self.assertEqual(xs, [4, 3, 4, 4, 7, 8])
        self.assertTrue(phase1(xs))
        self.assertEqual(xs, [3, 4, 4, 4, 7, 8])
        self.assertFalse(phase1(xs))

    def test_phase2(self):
        xs = [3, 4, 4, 4, 7, 8]
        self.assertTrue(phase2(xs))
        self.assertEqual(xs, [4, 4, 4, 4, 7, 7])
        self.assertTrue(phase2(xs))
        self.assertEqual(xs, [4, 4, 4, 5, 7, 6])
        self.assertTrue(phase2(xs))
        self.assertEqual(xs, [4, 4, 5, 5, 6, 6])
        self.assertTrue(phase2(xs))
        self.assertEqual(xs, [4, 5, 5, 5, 6, 5])
        self.assertTrue(phase2(xs))
        self.assertEqual(xs, [5, 5, 5, 5, 5, 5])
        self.assertFalse(phase2(xs))

    def test_checksum(self):
        self.assertEqual(checksum([9, 1, 1, 4, 9, 6]), 111)
        self.assertEqual(checksum([4, 5, 5, 5, 6, 5]), 109)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 109)
        failures += check(2, part2(parse("sample2.txt")), 1579)
    else:
        failures += check(1, part1(parse("input1.txt")), 324)
        failures += check(2, part2(parse("input2.txt")), 3908387)

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
