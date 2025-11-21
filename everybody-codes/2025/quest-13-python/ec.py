import sys
import unittest
from collections import deque


def parse(filename):
    return list(map(int, open(filename).readlines()))


def parse_ranges(filename):
    return [
        tuple(map(int, line.strip().split("-")))  #
        for line in open(filename).readlines()
    ]


def part_1(xs):
    wheel = fill(1, xs)
    return wheel[2025 % len(wheel)]


def part_2_and_3(xs, required):
    wheel = fill((1, 1), xs)
    required %= sum(abs(r - l) + 1 for l, r in wheel)
    turns = 0

    for left, right in wheel:
        size = abs(right - left) + 1
        dist = required - turns

        if dist < size:
            return left + dist if left < right else left - dist

        turns += size


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
    fails = 0

    if is_sample:
        fails += check(1, part_1(parse("sample1.txt")), 67)
        fails += check(2, part_2_and_3(parse_ranges("sample2.txt"), 20252025), 30)
    else:
        fails += check(1, part_1(parse("input1.txt")), 511)
        fails += check(2, part_2_and_3(parse_ranges("input2.txt"), 20252025), 4702)
        fails += check(3, part_2_and_3(parse_ranges("input3.txt"), 202520252025), 37289)

    exit(fails)


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
