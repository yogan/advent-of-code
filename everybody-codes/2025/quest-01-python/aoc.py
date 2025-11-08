import sys
import unittest


def parse(filename):
    names, moves = open(filename).read().strip().split("\n\n")
    return names.split(","), [(m[0], int(m[1])) for m in moves.split(",")]


def part1(names, moves):
    pos = 0
    for move, steps in moves:
        if move == "L":
            pos = max(0, pos - steps)
        else:
            pos = min(len(names) - 1, pos + steps)
    return names[pos]


def part2(names, moves):
    pos, l = 0, len(names)
    for move, steps in moves:
        if move == "L":
            pos = (pos - steps) % l
        else:
            pos = (pos + steps) % l
    return names[pos]


class Tests(unittest.TestCase):
    def test_part1(self):
        self.assertEqual(part1(["A", "B"], []), "A")
        self.assertEqual(part1(["A", "B"], [("L", 1)]), "A")
        self.assertEqual(part1(["A", "B"], [("R", 1)]), "B")
        self.assertEqual(part1(["A", "B"], [("R", 2)]), "B")
        self.assertEqual(part1(["A", "B"], [("R", 1), ("L", 1)]), "A")

    def test_part2(self):
        self.assertEqual(part2(["A", "B"], []), "A")
        self.assertEqual(part2(["A", "B"], [("L", 1)]), "B")
        self.assertEqual(part2(["A", "B"], [("L", 2)]), "A")
        self.assertEqual(part2(["A", "B"], [("R", 1)]), "B")
        self.assertEqual(part2(["A", "B"], [("R", 2)]), "A")
        self.assertEqual(part2(["A", "B", "C"], [("R", 1), ("L", 2)]), "C")


if __name__ == "__main__":
    flags = set(arg for arg in sys.argv[1:] if arg.startswith("-"))
    args = [arg for arg in sys.argv[1:] if not arg.startswith("-")]
    sys.argv = sys.argv[:1]  # strip args, unittest.main() doesn't like them

    is_sample = "-s" in flags or "--sample" in flags
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

    if is_sample:
        names, moves = parse("sample.txt")
        check(1, part1(names, moves), "Fyrryn")
        check(2, part2(names, moves), "Elarzris")
    else:
        names1, moves1 = parse("input1.txt")
        names2, moves2 = parse("input2.txt")
        check(1, part1(names1, moves1), "Braeluth")
        check(2, part2(names2, moves2), "Thazaelor")
