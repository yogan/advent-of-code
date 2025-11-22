import sys
import unittest


def parse(filename):
    return to_grid(open(filename).readlines())


def to_grid(lines):
    return tuple(map(parse_line, lines))


def parse_line(line):
    return tuple(c == "#" for c in line.strip())


def parts_1_and_2(grid, rounds):
    rows, cols = len(grid), len(grid[0])
    total = 0

    for _ in range(rounds):
        grid = step(grid, rows, cols)
        total += count(grid, rows, cols)

    return total


def part_3(pattern):
    size, rounds = 34, 1_000_000_000
    grid = tuple((False,) * size for _ in range(size))
    seen, matches = dict(), dict()

    for round in range(rounds):
        if is_in_center(grid, pattern):
            matches[round] = count(grid, size, size)

        if grid in seen:
            loop_length = round - seen[grid]
            rep = rounds // loop_length
            rem = rounds % loop_length
            leftovers = sum(a for r, a in matches.items() if r < rem)
            return rep * sum(matches.values()) + leftovers

        seen[grid] = round
        grid = step(grid, size, size)


def step(grid, rows, cols):
    return tuple(
        tuple(state(grid, r, c, rows, cols) for c in range(cols)) for r in range(rows)
    )


def state(grid, r, c, rows, cols):
    even = sum(neighbors(grid, r, c, rows, cols)) % 2 == 0
    active = grid[r][c]

    return active and not even or not active and even


def neighbors(grid, r, c, rows, cols):
    return [
        grid[nr][nc]
        for nr, nc in [(r - 1, c - 1), (r - 1, c + 1), (r + 1, c - 1), (r + 1, c + 1)]
        if 0 <= nr < rows and 0 <= nc < cols
    ]


def is_in_center(grid, pattern):
    grid_rows, pattern_rows = len(grid), len(pattern)
    grid_cols, pattern_cols = len(grid[0]), len(pattern[0])

    row_offset = (grid_rows - pattern_rows) // 2
    col_offset = (grid_cols - pattern_cols) // 2

    return all(
        pattern[r] == grid[row_offset + r][col_offset : col_offset + pattern_cols]
        for r in range(pattern_rows)
    )


def count(grid, rows, cols):
    return sum(grid[r][c] for c in range(cols) for r in range(rows))


class Tests(unittest.TestCase):
    def test_step(self):
        grid = [
            ".#.##.",
            "##..#.",
            "..##.#",
            ".#.##.",
            ".###..",
            "###.##",
        ]
        expected = [
            ".#.#..",
            "##.##.",
            "#.#...",
            "....##",
            "#.####",
            "##..#.",
        ]
        self.assertEqual(step(to_grid(grid), 6, 6), to_grid(expected))

    def test_is_in_center(self):
        pattern = [
            "#......#",
            "..#..#..",
            ".##..##.",
            "...##...",
            "...##...",
            ".##..##.",
            "..#..#..",
            "#......#",
        ]
        grid = [
            "#······#·#··#··####··#··#·#······#",
            "·####·#·#······#··#······#·#·####·",
            "·#·####·###··#·####·#··###·####·#·",
            "·##··#···##·##·####·##·##···#··##·",
            "·##····##··##·######·##··##····##·",
            "··##·##·#··##·#·##·#·##··#·##·##··",
            "·##··#·#···#·##····##·#···#·#··##·",
            "#···#·##··##··#····#··##··##·#···#",
            "·##·##··###·#········#·###··##·##·",
            "#·##····##·###·#··#·###·##····##·#",
            "··##···##···###····###···##···##··",
            "····####·#··#·#····#·#··#·####····",
            "#··###··######·####·######··###··#",
            "··##··#··##·##······##·##··#··##··",
            "····####··##···#··#···##··####····",
            "#####····#··#·##··##·#··#····#####",
            "#·####······#···##···#······####·#",
            "#·####······#···##···#······####·#",
            "#####····#··#·##··##·#··#····#####",
            "····####··##···#··#···##··####····",
            "··##··#··##·##······##·##··#··##··",
            "#··###··######·####·######··###··#",
            "····####·#··#·#····#·#··#·####····",
            "··##···##···###····###···##···##··",
            "#·##····##·###·#··#·###·##····##·#",
            "·##·##··###·#········#·###··##·##·",
            "#···#·##··##··#····#··##··##·#···#",
            "·##··#·#···#·##····##·#···#·#··##·",
            "··##·##·#··##·#·##·#·##··#·##·##··",
            "·##····##··##·######·##··##····##·",
            "·##··#···##·##·####·##·##···#··##·",
            "·#·####·###··#·####·#··###·####·#·",
            "·####·#·#······#··#······#·#·####·",
            "#······#·#··#··####··#··#·#······#",
        ]
        self.assertTrue(is_in_center(to_grid(grid), to_grid(pattern)))

        # change single cell in the center:
        grid[17] = "#·####······#···#.···#······####·#"
        self.assertFalse(is_in_center(to_grid(grid), to_grid(pattern)))


def main():
    failures = 0

    if is_sample:
        failures += check(1, parts_1_and_2(parse("sample1.txt"), 10), 200)
        failures += check(2, parts_1_and_2(parse("sample2.txt"), 2025), 39349)
        failures += check(3, part_3(parse("sample3.txt")), 278388552)
    else:
        failures += check(1, parts_1_and_2(parse("input1.txt"), 10), 483)
        failures += check(2, parts_1_and_2(parse("input2.txt"), 2025), 1170986)
        failures += check(3, part_3(parse("input3.txt")), 1268866076)

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
