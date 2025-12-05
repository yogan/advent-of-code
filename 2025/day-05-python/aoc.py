import sys
import unittest


def part1(ranges, ids):
    total = 0

    for id in ids:
        if fresh(id, ranges):
            total += 1

    return total


def fresh(id, ranges):
    return any(lo <= id <= hi for lo, hi in ranges)


def parse():
    ranges, ids = open(filename).read().split("\n\n")
    ranges = [
        tuple(map(int, rg.strip().split("-"))) for rg in ranges.strip().split("\n")
    ]
    ids = list(map(int, ids.strip().split("\n")))

    return ranges, ids


class Tests(unittest.TestCase):
    pass
    # def test_volume(self):
    #     self.assertEqual(volume([2, 3, 4]), 24)


def main():
    failures = 0
    failures += check(1, part1(*parse()), 3 if is_sample else 509)

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
