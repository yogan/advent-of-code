import sys
import unittest


def parse(filename):
    return list(map(int, open(filename).read().strip().replace("|", "\n").split("\n")))


def part1(xs):
    return int(2025 * factor(xs))


def part2(xs):
    return round(10_000_000_000_000 / factor(xs))


def part3(xs):
    val = 100
    for i in range(0, len(xs) - 1, 2):
        val *= xs[i] / xs[i + 1]
    return int(val)


def factor(xs):
    factor = 1
    for a, b in zip(xs, xs[1:]):
        factor *= a / b
    return factor


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([128, 64, 32, 16, 8]), 32400)
        self.assertEqual(part1([102, 75, 50, 35, 13]), 15888)

    def test_part3(self):
        self.assertEqual(part3([5, 5, 10, 10, 20, 5]), 400)
        self.assertEqual(part3([5, 7, 21, 18, 36, 27, 27, 10, 50, 10, 50, 11]), 6818)


def main():
    failures = 0

    if is_sample:
        failures += check(1.1, part1(parse("sample1.txt")), 32400)
        failures += check(1.2, part1(parse("sample2.txt")), 15888)
        failures += check(2.1, part2(parse("sample1.txt")), 625_000_000_000)
        failures += check(2.2, part2(parse("sample2.txt")), 1_274_509_803_922)
        failures += check(3, part3(parse("sample3.txt")), 6818)
    else:
        failures += check(1, part1(parse("input1.txt")), 11773)
        failures += check(2, part2(parse("input2.txt")), 2_631_016_042_781)
        failures += check(3, part3(parse("input3.txt")), 660_846_420_232)

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
        sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them
        unittest.main(exit=True)

    main()
