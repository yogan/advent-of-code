import sys
import unittest


def parse():
    return [list(line.strip()) for line in open(filename).readlines()]


def travel(lines):
    X_MAX = len(lines[0])
    Y_MAX = len(lines)
    x, y = find_guard(lines)
    visited = {(x, y)}

    while True:
        x_next, y_next = next_pos((x, y), lines[y][x])
        if x_next < 0 or x_next >= X_MAX or y_next < 0 or y_next >= Y_MAX:
            break
        if lines[y_next][x_next] == "#":
            lines[y][x] = rotate_right(lines[y][x])
        else:
            lines[y_next][x_next] = lines[y][x]
            lines[y][x] = "."
            x, y = x_next, y_next
            visited.add((x, y))

    return visited


def has_loop(lines, guard_start):
    X_MAX = len(lines[0])
    Y_MAX = len(lines)
    x, y = guard_start
    visited = {(x, y, lines[y][x])}

    while True:
        x_next, y_next = next_pos((x, y), lines[y][x])
        if x_next < 0 or x_next >= X_MAX or y_next < 0 or y_next >= Y_MAX:
            break
        if lines[y_next][x_next] == "#":
            lines[y][x] = rotate_right(lines[y][x])
        else:
            lines[y_next][x_next] = lines[y][x]
            lines[y][x] = "."
            x, y = x_next, y_next
            if (x, y, lines[y][x]) in visited:
                return True
            visited.add((x, y, lines[y][x]))

    return False


def part2(lines, visited):
    guard_start = find_guard(lines)
    candidates = [pos for pos in visited if pos != guard_start]

    loops = 0

    while candidates:
        x, y = candidates.pop()
        lines_copy = [line.copy() for line in lines]
        lines_copy[y][x] = "#"
        if has_loop(lines_copy, guard_start):
            loops += 1

    return loops


def next_pos(pos, direction):
    x, y = pos
    if direction == "^":
        return (x, y - 1)
    if direction == ">":
        return (x + 1, y)
    if direction == "v":
        return (x, y + 1)
    if direction == "<":
        return (x - 1, y)
    raise ValueError(f"Invalid direction: {direction}")


def rotate_right(direction):
    if direction == "^":
        return ">"
    if direction == ">":
        return "v"
    if direction == "v":
        return "<"
    if direction == "<":
        return "^"
    raise ValueError(f"Invalid direction: {direction}")


def find_guard(lines):
    for y, line in enumerate(lines):
        for x, c in enumerate(line):
            if c == "^":
                return (x, y)
    raise ValueError("Guard not found")


class Tests(unittest.TestCase):
    def test_travel(self):
        self.assertEqual(
            len(
                travel(
                    [
                        list("....#....."),
                        list(".........#"),
                        list(".........."),
                        list("..#......."),
                        list(".......#.."),
                        list(".........."),
                        list(".#..^....."),
                        list("........#."),
                        list("#........."),
                        list("......#..."),
                    ]
                )
            ),
            41,
        )


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

    lines = parse()
    visited = travel([line.copy() for line in lines])
    p1 = len(visited)
    p2 = part2(lines, visited)

    check(1, p1, 41 if is_sample else 4973)
    check(2, p2, 6 if is_sample else 1482)
