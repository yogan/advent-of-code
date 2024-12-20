import sys
import unittest
from collections import deque


def parse(filename):
    grid = [line.strip() for line in open(filename).readlines()]
    rows, cols = len(grid), len(grid[0])
    start, end = (-1, -1), (-1, -1)
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == "S":
                start = (r, c)
            elif grid[r][c] == "E":
                end = (r, c)
    return grid, rows, cols, start, end


def possible_cheats(grid, rows, cols):
    cheats = set()
    for r in range(1, rows - 1):
        for c in range(1, cols - 1):
            if grid[r][c] != "#":
                for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
                    r1, c1 = (r + 1 * dr, c + 1 * dc)
                    r2, c2 = (r + 2 * dr, c + 2 * dc)
                    if (
                        0 <= r2 < rows
                        and 0 <= c2 < cols
                        and grid[r1][c1] == "#"
                        and grid[r2][c2] != "#"
                    ):
                        cheats.add((r1, c1))
    return cheats


def shortest_path(grid, rows, cols, start, end, cheat=None) -> int:
    queue = deque([(start, 0)])
    visited = set()

    while queue:
        (r, c), steps = queue.popleft()

        if (r, c) == end:
            return steps

        if (r, c) in visited:
            continue

        visited.add((r, c))

        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            r1, c1 = (r + dr, c + dc)
            if (
                0 <= r1 < rows
                and 0 <= c1 < cols
                and (grid[r1][c1] != "#" or (r1, c1) == cheat)
            ):
                queue.append(((r1, c1), steps + 1))

    return -1


def part1(grid, rows, cols, start, end, min_save):
    worthwile_cheats = 0
    regular_path_length = shortest_path(grid, rows, cols, start, end)

    for cheat in possible_cheats(grid, rows, cols):
        cheat_path_length = shortest_path(grid, rows, cols, start, end, cheat)
        saving = regular_path_length - cheat_path_length
        if saving >= min_save:
            worthwile_cheats += 1

    return worthwile_cheats


class Tests(unittest.TestCase):
    def test_possible_cheats(self):
        grid, rows, cols, _, _ = parse("sample.txt")
        cheats = possible_cheats(grid, rows, cols)

        self.assertEqual(len(cheats), 14 + 14 + 2 + 4 + 2 + 3 + 5 * 1)
        self.assertIn((+1, +8), cheats)
        self.assertIn((+7, 10), cheats)
        self.assertIn((+8, +8), cheats)
        self.assertIn((+7, +6), cheats)

    def test_shortest_path(self):
        grid, rows, cols, start, end = parse("sample.txt")

        self.assertEqual(shortest_path(grid, rows, cols, start, end), 84)
        self.assertEqual(shortest_path(grid, rows, cols, start, end, (+1, +8)), 84 - 12)
        self.assertEqual(shortest_path(grid, rows, cols, start, end, (+7, 10)), 84 - 20)
        self.assertEqual(shortest_path(grid, rows, cols, start, end, (+8, +8)), 84 - 38)
        self.assertEqual(shortest_path(grid, rows, cols, start, end, (+7, +6)), 84 - 64)


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(exit=True)

    def check(part, actual, expected=None):
        print(f"Part {part}{' (sample)' if is_sample else ''}: {actual} ", end="")
        if expected is None:
            print("❔")
        else:
            if actual != expected:
                print(f"≠ {expected} ❌")
                exit(1)
            print("✅")

    grid, rows, cols, start, end = parse(filename)
    min_save = 10 if is_sample else 100
    p1 = part1(grid, rows, cols, start, end, min_save)
    p2 = None

    check(1, p1, 10 if is_sample else 1409)
    check(2, p2)
