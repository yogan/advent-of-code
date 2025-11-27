import sys
import unittest


def parse(filename):
    volcano = None
    grid = []
    for row, line in enumerate(open(filename).readlines()):
        col = line.find("@")
        if col != -1:
            volcano = (row, col)
        grid.append(list(map(int, line.replace("@", "0").strip())))
    return grid, volcano


def part1(grid, volcano):
    return sum(
        grid[r][c]
        for r in range(len(grid))
        for c in range(len(grid[0]))
        if in_circle(volcano[0] - r, volcano[1] - c, 10)
    )


def in_circle(dx, dy, r):
    return dx * dx + dy * dy <= r * r


class Tests(unittest.TestCase):
    pass


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt")), 1573)
    else:
        failures += check(1, part1(*parse("input1.txt")), 1547)

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
