import sys
import unittest


def parse(filename):
    grid = [list(map(int, (line.strip()))) for line in open(filename).readlines()]
    rows = len(grid)
    cols = len(grid[0])
    return grid, rows, cols


def part1(grid, rows, cols):
    return flood_fill(grid, rows, cols, [(0, 0)])


def part2(grid, rows, cols):
    return flood_fill(grid, rows, cols, [(0, 0), (rows - 1, cols - 1)])


def flood_fill(grid, rows, cols, initial):
    queue = initial
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
            if 0 <= r + dr < rows and 0 <= c + dc < cols
        )

        for br, bc in border:
            if grid[r][c] >= grid[br][bc]:
                destroyed.add((br, bc))
                queue.append((br, bc))

    return len(destroyed)


class Tests(unittest.TestCase):
    pass


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt")), 16)
        failures += check(2, part2(*parse("sample2.txt")), 58)
    else:
        failures += check(1, part1(*parse("input1.txt")), 238)
        failures += check(2, part2(*parse("input2.txt")), 5786)

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
