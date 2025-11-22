import sys
import unittest


def parse(filename):
    return [parse_line(line.strip()) for line in open(filename).readlines()]


def parse_line(line):
    return [c == "#" for c in line]


def parts_1_and_2(grid, rounds):
    rows, cols = len(grid), len(grid[0])
    total = 0

    for _ in range(rounds):
        grid = step(grid, rows, cols)
        total += sum(grid[r][c] for c in range(cols) for r in range(rows))

    return total


def part_3(pattern):
    size = 34
    grid = [[False] * size for _ in range(size)]
    seen = dict()
    matches = dict()
    rounds = 1000000000

    for round in range(rounds):
        if is_in_center(grid, pattern):
            active = sum(grid[r][c] for c in range(size) for r in range(size))
            matches[round] = active

        hash = to_binary_tuple(grid)

        if hash in seen:
            loop_length = round - seen[hash]
            rep = rounds // loop_length
            rem = rounds % loop_length
            leftovers = sum(a for r, a in matches.items() if r < rem)
            return rep * sum(matches.values()) + leftovers

        seen[hash] = round
        grid = step(grid, size, size)


def step(grid, rows, cols):
    new = [[] for _ in range(rows)]

    for r in range(rows):
        for c in range(cols):
            neighbors = [
                grid[r + dr][c + dc]
                for dr, dc in [(-1, -1), (-1, 1), (1, -1), (1, 1)]
                if 0 <= r + dr < rows and 0 <= c + dc < cols
            ]

            even = sum(neighbors) % 2 == 0
            active = grid[r][c]

            new[r].append(active and not even or not active and even)

    return new


def is_in_center(grid, pattern):
    grid_rows, grid_cols = len(grid), len(grid[0])
    pattern_rows, pattern_cols = len(pattern), len(pattern[0])

    row_offset = grid_rows // 2 - pattern_rows // 2
    col_offset = grid_cols // 2 - pattern_cols // 2

    for r, row in enumerate(grid[row_offset : row_offset + pattern_rows]):
        if pattern[r] != row[col_offset : col_offset + pattern_cols]:
            return False

    return True


def to_binary_tuple(grid):
    return tuple(to_binary(row) for row in grid)


def to_binary(row):
    return sum(bit << idx for idx, bit in enumerate(row[::-1]))


class Tests(unittest.TestCase):
    def test_step(self):
        self.assertEqual(
            step(
                [
                    parse_line(".#.##."),
                    parse_line("##..#."),
                    parse_line("..##.#"),
                    parse_line(".#.##."),
                    parse_line(".###.."),
                    parse_line("###.##"),
                ],
                6,
                6,
            ),
            [
                parse_line(".#.#.."),
                parse_line("##.##."),
                parse_line("#.#..."),
                parse_line("....##"),
                parse_line("#.####"),
                parse_line("##..#."),
            ],
        )

    def test_is_in_center(self):
        pattern = [
            parse_line("#......#"),
            parse_line("..#..#.."),
            parse_line(".##..##."),
            parse_line("...##..."),
            parse_line("...##..."),
            parse_line(".##..##."),
            parse_line("..#..#.."),
            parse_line("#......#"),
        ]
        grid = [
            parse_line("#······#·#··#··####··#··#·#······#"),
            parse_line("·####·#·#······#··#······#·#·####·"),
            parse_line("·#·####·###··#·####·#··###·####·#·"),
            parse_line("·##··#···##·##·####·##·##···#··##·"),
            parse_line("·##····##··##·######·##··##····##·"),
            parse_line("··##·##·#··##·#·##·#·##··#·##·##··"),
            parse_line("·##··#·#···#·##····##·#···#·#··##·"),
            parse_line("#···#·##··##··#····#··##··##·#···#"),
            parse_line("·##·##··###·#········#·###··##·##·"),
            parse_line("#·##····##·###·#··#·###·##····##·#"),
            parse_line("··##···##···###····###···##···##··"),
            parse_line("····####·#··#·#····#·#··#·####····"),
            parse_line("#··###··######·####·######··###··#"),
            parse_line("··##··#··##·##······##·##··#··##··"),
            parse_line("····####··##···#··#···##··####····"),
            parse_line("#####····#··#·##··##·#··#····#####"),
            parse_line("#·####······#···##···#······####·#"),
            parse_line("#·####······#···##···#······####·#"),
            parse_line("#####····#··#·##··##·#··#····#####"),
            parse_line("····####··##···#··#···##··####····"),
            parse_line("··##··#··##·##······##·##··#··##··"),
            parse_line("#··###··######·####·######··###··#"),
            parse_line("····####·#··#·#····#·#··#·####····"),
            parse_line("··##···##···###····###···##···##··"),
            parse_line("#·##····##·###·#··#·###·##····##·#"),
            parse_line("·##·##··###·#········#·###··##·##·"),
            parse_line("#···#·##··##··#····#··##··##·#···#"),
            parse_line("·##··#·#···#·##····##·#···#·#··##·"),
            parse_line("··##·##·#··##·#·##·#·##··#·##·##··"),
            parse_line("·##····##··##·######·##··##····##·"),
            parse_line("·##··#···##·##·####·##·##···#··##·"),
            parse_line("·#·####·###··#·####·#··###·####·#·"),
            parse_line("·####·#·#······#··#······#·#·####·"),
            parse_line("#······#·#··#··####··#··#·#······#"),
        ]
        self.assertTrue(is_in_center(grid, pattern))

        grid[17][16] = not grid[17][16]
        self.assertFalse(is_in_center(grid, pattern))


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
