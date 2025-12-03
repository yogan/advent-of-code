import sys
import unittest


def part1(banks):
    total = 0
    for batteries in banks:
        total += largest(batteries)
    return total


def largest(batteries):
    l = max(batteries[:-1])
    i = batteries.index(l)
    r = max(batteries[i + 1 :])
    return int(f"{l}{r}")


def parse():
    return [list(map(int, line.strip())) for line in open(filename).readlines()]


class Tests(unittest.TestCase):
    def test_largest(self):
        self.assertEqual(largest([9, 8, 7, 6, 1, 1, 1, 1]), 98)
        self.assertEqual(largest([8, 1, 1, 1, 1, 1, 1, 9]), 89)
        self.assertEqual(largest([2, 3, 4, 2, 3, 4, 7, 8]), 78)
        self.assertEqual(largest([8, 1, 8, 1, 9, 1, 2, 1]), 92)


def main():
    failures = 0
    failures += check(1, part1(parse()), 357 if is_sample else 16973)

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
