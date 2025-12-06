import math
import re
import sys
import unittest


def part1(ops, numbers):
    total = 0
    for i in range(len(ops)):
        total += sum(numbers[i]) if ops[i] == "+" else math.prod(numbers[i])
    return total


def parse():
    table = [re.split("\\s+", line.strip()) for line in open(filename).readlines()]
    return table[-1], list(zip(*[list(map(int, ns)) for ns in table[:-1]][::-1]))


class Tests(unittest.TestCase):
    pass


def main():
    failures = 0
    failures += check(1, part1(*parse()), 4277556 if is_sample else 8108520669952)

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
