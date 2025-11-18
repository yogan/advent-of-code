import sys
import unittest


def parse(filename):
    lines = open(filename).read().splitlines()
    positions = parse_positions(lines)
    return positions, len(lines), len(lines[0])


def parse_positions(lines):
    positions = {}
    for row, line in enumerate(lines):
        for col, c in enumerate(line):
            if c != ".":
                pos = (row, col)
                if c in positions:
                    positions[c].add(pos)
                else:
                    positions[c] = set([pos])
    return positions


def part1(positions, rows, cols, moves):
    curr = set(positions["D"])
    seen = set(curr)

    for _ in range(moves):
        next = move_dragons(curr, rows, cols)
        seen |= next
        curr = next

    return len(positions["S"] & seen)


def part2(positions, rows, cols, moves):
    hideouts = set(positions["#"])
    dragons = set(positions["D"])
    sheep = set(positions["S"])

    eaten = 0

    for _ in range(moves):
        # move the dragons
        dragons = move_dragons(dragons, rows, cols)

        # check captures
        captured = (dragons & sheep) - hideouts
        eaten += len(captured)

        # move the sheep
        sheep = move_sheep(sheep - captured, rows)

        # check captures again
        captured = (dragons & sheep) - hideouts
        eaten += len(captured)

        # reduce sheep
        sheep -= captured

    return eaten


def part3(positions, rows, cols):
    hideouts = frozenset(positions["#"])
    sheep = tuple(positions["S"])
    dragon = positions["D"].pop()

    return dfs(sheep, dragon, hideouts, "sheep", rows, cols, {})


def dfs(sheep, dragon, hideouts, turn, rows, cols, cache):
    key = (sheep, dragon, turn)
    if key in cache:
        return cache[key]

    if not sheep:
        cache[key] = 1
        return 1

    res = 0

    if turn == "sheep":
        no_sheep_moved = True
        for r, c in sheep:
            # Forbidden move
            if (r + 1, c) == dragon and (r + 1, c) not in hideouts:
                continue

            # Both escaping and regular moves are considered as a sheep move
            no_sheep_moved = False

            # Quick escape route: all fields below the sheep are hideouts.
            # Also works when sheep is already at the bottom, as all([]) == True.
            can_escape = all((rr, c) in hideouts for rr in range(r + 1, rows))

            if not can_escape:
                moved = replace(sheep, (r, c), (r + 1, c))
                res += dfs(moved, dragon, hideouts, "dragon", rows, cols, cache)

        if no_sheep_moved:
            res += dfs(sheep, dragon, hideouts, "dragon", rows, cols, cache)

    elif turn == "dragon":
        for d in move_dragons([dragon], rows, cols):
            next_sheep = tuple(s for s in sheep if s != d or s in hideouts)
            res += dfs(next_sheep, d, hideouts, "sheep", rows, cols, cache)

    cache[key] = res
    return res


def replace(t, old, new):
    s = set(t)
    s.remove(old)
    s.add(new)
    return tuple(s)


def move_dragons(positions, rows, cols):
    next = set()
    for row, col in positions:
        # fmt:off
        for dr, dc in [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]:  # fmt:on
            r, c = row + dr, col + dc
            if 0 <= r < rows and 0 <= c < cols:
                next.add((r, c))
    return next


def move_sheep(positions, rows):
    next = set()
    for row, col in positions:
        if row + 1 < rows:
            next.add((row + 1, col))
    return next


class Tests(unittest.TestCase):
    def test_parse_positions(self):
        pos = parse_positions(
            [
                ".......",
                "..X.X..",
                ".X...X.",
                "...D...",
                ".X...X.",
                "..X.X..",
                ".......",
            ]
        )
        self.assertEqual(pos["D"], set([(3, 3)]))
        # fmt:off
        self.assertEqual(pos["X"], set([
                  (1,2),(1,4),
            (2,1),            (2,5),
            (4,1),            (4,5),
                  (5,2),(5,4),
        ]))  # fmt:on

    def test_move_dragon_center(self):
        pos = parse_positions(
            [
                ".......",
                "..X.X..",
                ".X...X.",
                "...D...",
                ".X...X.",
                "..X.X..",
                ".......",
            ]
        )
        self.assertEqual(move_dragons(pos["D"], 7, 7), pos["X"])

    def test_move_dragon_top_left(self):
        pos = parse_positions(
            [
                "...X...",
                ".D.....",
                "...X...",
                "X.X....",
                ".......",
                ".......",
                ".......",
            ]
        )
        self.assertEqual(move_dragons(pos["D"], 7, 7), pos["X"])

    def test_move_dragon_top_right(self):
        pos = parse_positions(
            [
                "......D",
                "....X..",
                ".....X.",
                ".......",
                ".......",
                ".......",
                ".......",
            ]
        )
        self.assertEqual(move_dragons(pos["D"], 7, 7), pos["X"])

    def test_move_dragon_bottom_center(self):
        pos = parse_positions(
            [
                ".......",
                ".......",
                ".......",
                ".......",
                "..X.X..",
                ".X...X.",
                "...D...",
            ]
        )
        self.assertEqual(move_dragons(pos["D"], 7, 7), pos["X"])

    def test_move_dragon_right_center(self):
        pos = parse_positions(
            [
                ".......",
                "....X.X",
                "...X...",
                ".....D.",
                "...X...",
                "....X.X",
                ".......",
            ]
        )
        self.assertEqual(move_dragons(pos["D"], 7, 7), pos["X"])

    def test_move_sheep(self):
        pos = parse_positions(
            [
                "...S",
                ".S..",
                "..S.",
                "....",
                "..SS",
            ]
        )["S"]
        expected = parse_positions(
            [
                "....",
                "...S",
                ".S..",
                "..S.",
                "....",
            ]
        )["S"]
        self.assertEqual(move_sheep(pos, 5), expected)


def main():
    failures = 0

    if is_sample:
        failures += check(1, part1(*parse("sample1.txt"), 3), 27)
        failures += check(2, part2(*parse("sample2.txt"), 3), 27)
        failures += check("3a", part3(*parse("sample3a.txt")), 15)
        failures += check("3b", part3(*parse("sample3b.txt")), 8)
        failures += check("3c", part3(*parse("sample3c.txt")), 44)
        failures += check("3d", part3(*parse("sample3d.txt")), 4406)
        failures += check("3e", part3(*parse("sample3e.txt")), 13033988838)
    else:
        failures += check(1, part1(*parse("input1.txt"), 4), 151)
        failures += check(2, part2(*parse("input2.txt"), 20), 1811)
        failures += check(3, part3(*parse("input3.txt")), 79826872784355)

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
