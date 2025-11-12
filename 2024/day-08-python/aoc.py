import sys
import unittest


def parse(lines):
    d = dict()
    for r, line in enumerate(lines):
        for c, char in enumerate(line):
            if char == ".":
                continue
            if char not in d:
                d[char] = []
            d[char].append((r, c))
    return d, len(lines), len(lines[0])


def part1(antennas, max_r, max_c):
    antinodes = set()

    for locs in antennas.values():
        for i in range(len(locs)):
            antinodes |= resonances(locs[i], locs[i + 1 :], max_r, max_c)

    return len(antinodes)


def part2(antennas, max_r, max_c):
    antinodes = set()

    for locs in antennas.values():
        antinodes |= set(locs)
        for i in range(len(locs)):
            antinodes |= harmonics(locs[i], locs[i + 1 :], max_r, max_c)

    return len(antinodes)


def resonances(antenna, others, max_r, max_c):
    result = set()

    for other in others:
        dr, dc = antenna[0] - other[0], antenna[1] - other[1]
        r1, c1 = antenna[0] + dr, antenna[1] + dc
        r2, c2 = other[0] - dr, other[1] - dc

        if on_map(r1, c1, max_r, max_c):
            result.add((r1, c1))

        if on_map(r2, c2, max_r, max_c):
            result.add((r2, c2))

    return result


def harmonics(antenna, others, max_r, max_c):
    result = set()

    for other in others:
        dr, dc = antenna[0] - other[0], antenna[1] - other[1]
        r, c = antenna[0] + dr, antenna[1] + dc

        while on_map(r, c, max_r, max_c):
            result.add((r, c))
            r += dr
            c += dc

        r, c = other[0] - dr, other[1] - dc
        while on_map(r, c, max_r, max_c):
            result.add((r, c))
            r -= dr
            c -= dc

    return result


def on_map(r, c, max_r, max_c):
    return 0 <= r < max_r and 0 <= c < max_c


class Tests(unittest.TestCase):
    def test_parse(self):
        self.assertEqual(
            parse(
                [
                    "............",
                    "........0...",
                    ".....0......",
                    ".......0....",
                    "....0.......",
                    "......A.....",
                    "............",
                    "............",
                    "........A...",
                    ".........A..",
                    "............",
                    "............",
                ]
            ),
            (
                {
                    "0": [(1, 8), (2, 5), (3, 7), (4, 4)],
                    "A": [(5, 6), (8, 8), (9, 9)],
                },
                12,
                12,
            ),
        )

    def test_resonances(self):
        self.assertEqual(resonances((3, 4), [], 10, 10), set())
        self.assertEqual(resonances((3, 4), [(5, 5)], 10, 10), {(1, 3), (7, 6)})

    def test_harmonics(self):
        self.assertEqual(
            harmonics((0, 0), [(1, 3), (2, 1)], 10, 10),
            {(2, 6), (3, 9), (4, 2), (6, 3), (8, 4)},
        )
        self.assertEqual(
            harmonics((1, 3), [(2, 1)], 10, 10),
            {(0, 5)},
        )


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

    antennas, max_r, max_c = parse(open(filename).read().splitlines())
    p1 = part1(antennas, max_r, max_c)
    p2 = part2(antennas, max_r, max_c)

    check(1, p1, 14 if is_sample else 327)
    check(2, p2, 34 if is_sample else 1233)
