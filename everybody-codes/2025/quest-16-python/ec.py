import sys
import unittest


def parse(filename):
    return list(map(int, open(filename).read().strip().split(",")))


def part1(xs):
    arr = [0] * 90
    for x in xs:
        for i in range(x, 91, x):
            arr[i - 1] += 1
    return sum(arr)


def part2(xs):
    res = 1
    while any(xs):
        p = next(i for i, x in enumerate(xs) if x > 0) + 1
        res *= p
        for i in range(p - 1, len(xs), p):
            xs[i] -= 1
    return res


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([1, 2, 3, 5, 9]), 193)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 193)
        failures += check(2, part2(parse("sample2.txt")), 270)
    else:
        failures += check(1, part1(parse("input1.txt")), 212)
        failures += check(2, part2(parse("input2.txt")), 104872255488)

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
