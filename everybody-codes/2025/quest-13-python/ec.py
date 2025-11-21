import sys
import unittest
from collections import deque


def parse_part1(filename):
    return list(map(int, open(filename).readlines()))


def parse_part2(filename):
    return [
        tuple(map(int, line.strip().split("-")))  #
        for line in open(filename).readlines()
    ]


def part1(xs):
    wheel = fill(1, xs)
    return wheel[2025 % len(wheel)]


def part2(xs):
    wheel = fill((1, 1), xs)
    required = 20252025 % sum(abs(r - l) + 1 for l, r in wheel)
    rotations = 0

    for left, right in wheel:
        dist = required - rotations
        size = abs(right - left) + 1

        if dist < size:
            return left + dist if left < right else left - dist

        rotations += size


def fill(initial, xs):
    q = deque([initial])
    clockwise = True
    offset = 0
    for x in xs:
        if clockwise:
            q.append(x)
        else:
            q.appendleft((x[1], x[0]) if isinstance(x, tuple) else x)
            offset += 1
        clockwise = not clockwise
    q.rotate(-offset)
    return q


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1([72, 58, 47, 61, 67]), 67)

    def test_fill(self):
        self.assertEqual(
            fill(1, [72, 58, 47, 61, 67]),
            deque([1, 72, 47, 67, 61, 58]),
        )
        self.assertEqual(
            fill((1, 1), [(10, 15), (12, 13), (20, 21), (19, 23), (30, 37)]),
            deque([(1, 1), (10, 15), (20, 21), (30, 37), (23, 19), (13, 12)]),
        )


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse_part1("sample1.txt")), 67)
        failures += check(2, part2(parse_part2("sample2.txt")), 30)
    else:
        failures += check(1, part1(parse_part1("input1.txt")), 511)
        failures += check(2, part2(parse_part2("input2.txt")), 4702)

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
