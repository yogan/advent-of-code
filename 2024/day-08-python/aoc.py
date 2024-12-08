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
    locations = set()

    for locs in antennas.values():
        for i in range(len(locs)):
            locations |= antinodes(locs[i], locs[i + 1 :], max_r, max_c)

    return len(locations)


def antinodes(antenna, others, max_r, max_c):
    result = set()

    for other in others:
        dr = antenna[0] - other[0]
        dc = antenna[1] - other[1]
        r1 = antenna[0] + dr
        c1 = antenna[1] + dc
        r2 = other[0] - dr
        c2 = other[1] - dc
        if 0 <= r1 < max_r and 0 <= c1 < max_c:
            result.add((r1, c1))
        if 0 <= r2 < max_r and 0 <= c2 < max_c:
            result.add((r2, c2))

    return result


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

    def test_anti_nodes(self):
        self.assertEqual(antinodes((3, 4), [], 9, 9), set())
        self.assertEqual(antinodes((3, 4), [(5, 5)], 9, 9), {(1, 3), (7, 6)})


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

    antennas, max_r, max_c = parse(open(filename).read().splitlines())
    p1 = part1(antennas, max_r, max_c)
    p2 = None

    check(1, p1, 14 if is_sample else 327)
    check(2, p2)
