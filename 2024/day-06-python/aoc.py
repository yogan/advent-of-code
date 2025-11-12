import sys
import unittest


def parse():
    return list(map(list, (open(filename).read().split())))


def travel(lab, guard_start):
    X_MAX = len(lab[0])
    Y_MAX = len(lab)
    x, y = guard_start
    visited = {(x, y, lab[y][x])}

    while True:
        x_next, y_next = next_pos((x, y), lab[y][x])
        if x_next < 0 or x_next >= X_MAX or y_next < 0 or y_next >= Y_MAX:
            break
        if lab[y_next][x_next] == "#":
            lab[y][x] = rotate_right(lab[y][x])
        else:
            lab[y_next][x_next] = lab[y][x]
            lab[y][x] = "."
            x, y = x_next, y_next
            if (x, y, lab[y][x]) in visited:
                return (True, {})
            visited.add((x, y, lab[y][x]))

    return (False, {(x, y) for x, y, _ in visited})


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


def find_guard(lab):
    for y, row in enumerate(lab):
        for x, c in enumerate(row):
            if c == "^":
                return (x, y)
    raise ValueError("Guard not found")


def part2(lab, visited):
    guard_start = find_guard(lab)
    candidates = [pos for pos in visited if pos != guard_start]

    return sum(loops(lab, pos, guard_start) for pos in candidates)


def loops(lab, pos, guard_start):
    x, y = pos
    lab_copy = [row.copy() for row in lab]
    lab_copy[y][x] = "#"

    return travel(lab_copy, guard_start)[0]


class Tests(unittest.TestCase):
    def test_travel(self):
        lab = [
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
        loops, visited = travel(lab, find_guard(lab))
        self.assertEqual(loops, False)
        self.assertEqual(len(visited), 41)


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

    lab = parse()
    _, visited = travel([row.copy() for row in lab], find_guard(lab))
    p1 = len(visited)
    p2 = part2(lab, visited)

    check(1, p1, 41 if is_sample else 4973)
    check(2, p2, 6 if is_sample else 1482)
