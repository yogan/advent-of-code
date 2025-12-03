import sys
import unittest


def joltage(banks, digits):
    return sum(int(largest(batteries, digits)) for batteries in banks)


def largest(batteries, digits):
    if digits == 1:
        return max(batteries)

    offset = 1 - digits
    digit = max(batteries[:offset])
    i = batteries.index(digit)

    return digit + largest(batteries[i + 1 :], digits - 1)


def parse():
    return [line.strip() for line in open(filename).readlines()]


class Tests(unittest.TestCase):
    def test_largest(self):
        self.assertEqual(largest("987654321111111", 2), "98")
        self.assertEqual(largest("811111111111119", 2), "89")
        self.assertEqual(largest("234234234234278", 2), "78")
        self.assertEqual(largest("818181911112111", 2), "92")

        self.assertEqual(largest("987654321111111", 12), "987654321111")
        self.assertEqual(largest("811111111111119", 12), "811111111119")
        self.assertEqual(largest("234234234234278", 12), "434234234278")
        self.assertEqual(largest("818181911112111", 12), "888911112111")


def main():
    failures = 0
    failures += check(1, joltage(parse(), 2), 357 if is_sample else 16973)
    failures += check(
        2, joltage(parse(), 12), 3121910778619 if is_sample else 168027167146027
    )

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
