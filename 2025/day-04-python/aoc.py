import sys
import unittest


def part1(grid):
    rows, cols = len(grid), len(grid[0])
    total = 0

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == "@":
                neighbors = sum(
                    grid[nr][nc] == "@"
                    for nr, nc in [
                        (r - 1, c - 1),
                        (r - 1, c),
                        (r - 1, c + 1),
                        (r, c - 1),
                        (r, c + 1),
                        (r + 1, c - 1),
                        (r + 1, c),
                        (r + 1, c + 1),
                    ]
                    if 0 <= nr < rows and 0 <= nc < cols
                )
                if neighbors < 4:
                    total += 1

    return total


def parse():
    return [line.strip() for line in open(filename).readlines()]


class Tests(unittest.TestCase):
    pass


def main():
    failures = 0
    failures += check(1, part1(parse()), 13 if is_sample else 1551)

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
