import sys
import unittest


def parse(filename):
    return [list(map(int, (line.strip()))) for line in open(filename).readlines()]


def part1(grid):
    ROWS = len(grid)
    COLS = len(grid[0])

    queue = [(0, 0)]
    destroyed = set(queue)
    seen = set()

    while queue:
        r, c = queue.pop()
        if ((r, c)) in seen:
            continue
        seen.add((r, c))

        border = set(
            (r + dr, c + dc)
            for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]
            if 0 <= r + dr < ROWS and 0 <= c + dc < COLS
        )

        for xr, xc in border:
            if grid[r][c] >= grid[xr][xc]:
                destroyed.add((xr, xc))
                queue.append((xr, xc))

    return len(destroyed)


class Tests(unittest.TestCase):
    pass


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 16)
    else:
        failures += check(1, part1(parse("input1.txt")), 238)

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
