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


def part2(grid, volcano):
    vr, vc = volcano
    best, best_dist = 0, 0
    seen = set()

    for dist in range(1, vr + 1):
        border = set(
            (r, c)
            for r in range(len(grid))
            for c in range(len(grid[0]))
            if in_circle(vr - r, vc - c, dist) and not (r, c) in seen
        )
        seen |= border
        cur = sum(grid[r][c] for r, c in border)
        if cur > best:
            best = cur
            best_dist = dist

    return best * best_dist


def in_circle(dx, dy, r):
    return dx * dx + dy * dy <= r * r


def on_circle(dx, dy, r):
    return dx * dx + dy * dy == r * r


class Tests(unittest.TestCase):
    pass


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt")), 1573)
        failures += check(2, part2(*parse("sample2.txt")), 1090)
    else:
        failures += check(1, part1(*parse("input1.txt")), 1547)
        failures += check(2, part2(*parse("input2.txt")), 66222)

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
