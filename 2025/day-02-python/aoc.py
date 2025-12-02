import sys
import unittest


def parse():
    return [
        tuple(map(int, id_range.split("-")))
        for line in open(filename).readlines()
        for id_range in line.strip().split(",")
    ]


def part1(ranges):
    total = 0

    for lo, hi in ranges:
        for id in range(lo, hi + 1):
            if invalid(id):
                total += id

    return total


def invalid(id):
    s = str(id)
    m = len(s) // 2
    return s[m:] == s[:m]


class Tests(unittest.TestCase):
    def test_invalid(self):
        self.assertTrue(invalid(11))
        self.assertTrue(invalid(1010))
        self.assertTrue(invalid(1188511885))

        self.assertFalse(invalid(10))
        self.assertFalse(invalid(101))
        self.assertFalse(invalid(1188511858))


def main():
    failures = 0
    failures += check(1, part1(parse()), 1227775554 if is_sample else 41294979841)

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
