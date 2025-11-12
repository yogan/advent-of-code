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


def shortest_path(grid, rows, cols, start, end):
    visited = set()
    queue = deque([[start]])

    while queue:
        path = queue.popleft()
        pos = path[-1]

        if pos == end:
            return path

        if pos in visited:
            continue
        visited.add(pos)

        r, c = pos
        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            r1, c1 = (r + dr, c + dc)
            next = (r1, c1)
            if 0 <= r1 < rows and 0 <= c1 < cols and grid[r1][c1] != "#":
                queue.append(path + [next])


def possible_cheats(path, min_saving, max_cheat_length):
    num_cheats = 0

    for i, (r1, c1) in enumerate(path):
        for j in range(i + 1, len(path)):
            r2, c2 = path[j]
            manhattan_distance = abs(r1 - r2) + abs(c1 - c2)
            if manhattan_distance > max_cheat_length:
                continue
            saving = j - i - manhattan_distance
            if saving >= min_saving:
                num_cheats += 1

    return num_cheats


class Tests(unittest.TestCase):
    def test_shortest_path(self):
        grid, rows, cols, start, end = parse("sample.txt")
        path = shortest_path(grid, rows, cols, start, end)
        assert path is not None
        self.assertEqual(len(path) - 1, 84)


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]

    filename = "sample.txt" if "-s" in flags or "--sample" in flags else "input.txt"
    filename = args[0] if args else filename
    is_sample = filename.startswith("sample")
    run_tests = "-t" in flags or "--test" in flags

    if run_tests:
        unittest.main(argv=sys.argv[:1])

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
    no_cheat_path = shortest_path(grid, rows, cols, start, end)

    p1 = possible_cheats(
        no_cheat_path, min_saving=10 if is_sample else 100, max_cheat_length=2
    )
    p2 = possible_cheats(
        no_cheat_path, min_saving=50 if is_sample else 100, max_cheat_length=20
    )

    check(1, p1, 10 if is_sample else 1409)
    check(2, p2, 285 if is_sample else 1012821)
