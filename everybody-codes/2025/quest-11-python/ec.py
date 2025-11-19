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


def parts_2_and_3(xs):
    round = 0

    while phase1(xs):
        round += 1

    # In phase 2, all columns eventually end up with the mean value.
    # The sum of all ducks will always be the same, and in a single round, one
    # duck moves, which modifies the value of two columns. So, the total moves
    # (rounds) is the sum of the differences of the column size and the mean
    # value, halved (because of the "two columns change" fact).
    m = mean(xs)
    diffs = [abs(m - x) for x in xs]
    return round + sum(diffs) // 2


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


def mean(xs):
    return int(sum(xs) / len(xs))


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
        failures += check(2, parts_2_and_3(parse("sample2.txt")), 1579)
    else:
        failures += check(1, part1(parse("input1.txt")), 324)
        failures += check(2, parts_2_and_3(parse("input2.txt")), 3908387)
        failures += check(3, parts_2_and_3(parse("input3.txt")), 143533074041125)

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
