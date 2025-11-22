import sys
import unittest


def parse(filename):
    return [list(line.strip()) for line in open(filename).readlines()]


def part1(grid):
    rows, cols = len(grid), len(grid[0])
    total = 0

    for _ in range(10):
        grid = step(grid, rows, cols)
        total += sum(grid[r][c] == "#" for c in range(cols) for r in range(rows))

    return total


def step(grid, rows, cols):
    new = [list("." * cols) for _ in range(rows)]

    for r in range(rows):
        for c in range(cols):
            neighbors = [
                grid[r + dr][c + dc] == "#"
                for dr, dc in [(-1, -1), (-1, 1), (1, -1), (1, 1)]
                if 0 <= r + dr < rows and 0 <= c + dc < cols
            ]

            even = sum(neighbors) % 2 == 0
            active = grid[r][c] == "#"

            if active and not even or not active and even:
                new[r][c] = "#"

    return new


class Tests(unittest.TestCase):
    def test_step(self):
        self.assertEqual(
            step(
                [
                    list(".#.##."),
                    list("##..#."),
                    list("..##.#"),
                    list(".#.##."),
                    list(".###.."),
                    list("###.##"),
                ],
                6,
                6,
            ),
            [
                list(".#.#.."),
                list("##.##."),
                list("#.#..."),
                list("....##"),
                list("#.####"),
                list("##..#."),
            ],
        )


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(parse("sample1.txt")), 200)
    else:
        failures += check(1, part1(parse("input1.txt")), 483)

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
