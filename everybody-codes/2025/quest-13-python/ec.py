import sys
import unittest
from collections import deque


def parse(filename):
    def to_range(str):
        return tuple(map(int, str.split("-") if "-" in str else (str, str)))

    return [to_range(line.strip()) for line in open(filename).readlines()]


def solve(ranges, required):
    wheel = fill(ranges)
    required %= sum(w[2] for w in wheel)
    turns = 0

    for left, right, size in wheel:
        dist = required - turns

        if dist < size:
            return left + dist if left < right else left - dist

        turns += size


def fill(ranges):
    def size(range):
        return abs(range[0] - range[1]) + 1

    wheel = deque([(1, 1, 1)])
    clockwise = True
    offset = 0

    for r in ranges:
        left, right = r

        if clockwise:
            wheel.append((left, right, size(r)))
        else:
            wheel.appendleft((right, left, size(r)))
            offset -= 1

        clockwise = not clockwise

    wheel.rotate(offset)
    return wheel


class Tests(unittest.TestCase):
    def test_fill(self):
        self.assertEqual(
            # fmt:off
            fill( [        (10,15),  (12,13),  (20,21),  (19,23),  (30,37)  ]),
            deque([(1,1,1),(10,15,6),(20,21,2),(30,37,8),(23,19,5),(13,12,2)])  # fmt:on
        )


def main():
    failures = 0

    if is_sample:
        failures += check(1, solve(parse("sample1.txt"), 2025), 67)
        failures += check(2, solve(parse("sample2.txt"), 20252025), 30)
    else:
        failures += check(1, solve(parse("input1.txt"), 2025), 511)
        failures += check(2, solve(parse("input2.txt"), 20252025), 4702)
        failures += check(3, solve(parse("input3.txt"), 202520252025), 37289)

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
