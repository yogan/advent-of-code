import sys
import unittest
from collections import deque


def parse(filename):
    return list(map(int, open(filename).readlines()))


def part1(xs):
    wheel = fill(xs)
    return wheel[2025 % len(wheel)]


def fill(xs):
    q = deque([1])
    back = True
    offset = 0
    for x in xs:
        if back:
            q.append(x)
        else:
            q.appendleft(x)
            offset += 1
        back = not back
    q.rotate(-offset)
    return q


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([72, 58, 47, 61, 67]), 67)

    def test_fill(self):
        self.assertEqual(fill([72, 58, 47, 61, 67]), deque([1, 72, 47, 67, 61, 58]))


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 67)
    else:
        failures += check(1, part1(parse("input1.txt")), 511)

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
