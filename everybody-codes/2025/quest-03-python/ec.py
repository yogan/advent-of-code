import sys
import unittest


def parse(filename):
    return list(map(int, open(filename).read().strip().split(",")))


def part1(xs):
    return sum(set(sorted(xs)))


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([10, 5, 1, 10, 3, 8, 5, 2, 2]), 29)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 29)
    else:
        failures += check(1, part1(parse("input1.txt")), 2652)

    exit(failures)


def check(part, actual, expected=None):
    failure = 0

    if expected is None:
        symbol = "❔"
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
